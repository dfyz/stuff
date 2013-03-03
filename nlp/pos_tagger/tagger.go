package main

import (
	"bufio"
	"fmt"
	"io"
	"log"
	"os"
	"sort"
	"strings"
)

const UnknownPos = "???"

type TaggedWord struct {
	Word string
	Pos string
}

type PosCounter struct {
	PosCounts map[string]uint
	MostProbablePos string
	MostProbablePosCount uint
}

func NewPosCounter() *PosCounter {
	return &PosCounter {
		PosCounts: make(map[string]uint),
		MostProbablePos: UnknownPos,
	}
}

func (cnt *PosCounter) Update(pos string) {
	newCount := cnt.PosCounts[pos] + 1
	if newCount > cnt.MostProbablePosCount {
		cnt.MostProbablePos = pos
		cnt.MostProbablePosCount = newCount
	}
	cnt.PosCounts[pos] = newCount
}

type BaselineTagger struct {
	Counters map[string]*PosCounter
	GlobalCounter *PosCounter
	UseFallback bool
}

func NewBaselineTagger(useFallback bool) *BaselineTagger {
	return &BaselineTagger {
		make(map[string]*PosCounter),
		NewPosCounter(),
		useFallback,
	}
}

func (tagger *BaselineTagger) Train(sentence []TaggedWord) {
	for _, word := range sentence {
		counter, ok := tagger.Counters[word.Word]
		if !ok {
			counter = NewPosCounter()
			tagger.Counters[word.Word] = counter
		}
		counter.Update(word.Pos)
		if tagger.UseFallback {
			tagger.GlobalCounter.Update(word.Pos)
		}
	}
}

func (tagger *BaselineTagger) Tag(sentence []string) []TaggedWord {
	result := make([]TaggedWord, len(sentence))
	for i, word := range sentence {
		result[i].Word = word
		counter, ok := tagger.Counters[word]
		if !ok {
			if tagger.UseFallback {
				result[i].Pos = tagger.GlobalCounter.MostProbablePos
			} else {
				result[i].Pos = UnknownPos
			}
			continue
		}
		result[i].Pos = counter.MostProbablePos
	}
	return result
}

type Tagger interface {
	Train(sentence []TaggedWord)
	Tag(sentence []string) []TaggedWord
}

type TaggerWithName struct {
	Name string
	Tagger Tagger
}

type TaggerError struct {
	word string
	expectedPos string
	actualPos string
}

type CountedTaggerError struct {
	error TaggerError
	count uint
}
type CountedErrors []*CountedTaggerError

func (e CountedErrors) Len() int { return len(e) }
func (e CountedErrors) Swap(i, j int) { e[i], e[j] = e[j], e[i] }
func (e CountedErrors) Less(i, j int) bool { return e[i].count > e[j].count }

func main() {
	taggers := []TaggerWithName {
		 { "MostFrequent", NewBaselineTagger(false), },
		 { "MostFrequentWithFallback", NewBaselineTagger(true), },
	}

	readCorpus := func(corpusFile string, processor func (string, Tagger, string)) {
		corpus, err := os.Open(corpusFile)
		if err != nil {
			log.Fatal(err)
		}
		in := bufio.NewReader(corpus)
		for {
			line, err := in.ReadString('\n')
			if err == io.EOF {
				break
			}
			if err != nil {
				log.Fatal(err)
			}
			line = strings.TrimRight(line, "\n")
			if line == "" {
				continue
			}
			for _, t := range taggers {
				processor(line, t.Tagger, t.Name)
			}
		}
	}

	parseSentence := func(line string) []TaggedWord {
		tokens := strings.Split(line, " ")
		result := make([]TaggedWord, len(tokens))
		for i, word := range tokens {
			parts := strings.Split(word, "/")
			result[i] = TaggedWord { parts[0], parts[1] }
		}
		return result
	}

	log.Println("Learning")
	readCorpus("nlpwp-data/brown-pos-train.txt", func(line string, tagger Tagger, _ string) {
		sentence := parseSentence(line)
		tagger.Train(sentence)
	})
	log.Println("Done learning")

	if len(os.Args) > 1 { 
		for _, t := range taggers {
			words := strings.Split(os.Args[1], " ")
			fmt.Printf("%s:", t.Name)
			for _, word := range t.Tagger.Tag(words) {
				fmt.Printf(" %s/%s", word.Word, word.Pos)
			}
			fmt.Println()
		}
	} else {
		log.Println("Evaluating tagger performances")

		type TaggerStats struct {
			correctGuesses uint
			incorrectGuesses uint
			errors map[TaggerError]uint
		}

		statsByTagger := make(map[string]*TaggerStats)

		readCorpus("nlpwp-data/brown-pos-test.txt", func(line string, tagger Tagger, taggerName string) {
			expectedSentence := parseSentence(line)
			taggerInput := make([]string, len(expectedSentence))
			for i, word := range expectedSentence {
				taggerInput[i] = word.Word
			}
			actualSentence := tagger.Tag(taggerInput)
			for i, word := range expectedSentence {
				actualWord := actualSentence[i]
				correct := word.Pos == actualWord.Pos

				stats, ok := statsByTagger[taggerName]
				if !ok {
					stats = &TaggerStats {
						errors: make(map[TaggerError]uint),
					}
					statsByTagger[taggerName] = stats
				}

				if correct {
					stats.correctGuesses++
				} else {
					stats.incorrectGuesses++
					stats.errors[TaggerError{ word.Word, word.Pos, actualWord.Pos }]++
				}
			}
		})

		for _, t := range taggers {
			stats := statsByTagger[t.Name]
			totalGuesses := stats.correctGuesses + stats.incorrectGuesses
			accuracy := float64(stats.correctGuesses) / float64(totalGuesses) * 100.0
			fmt.Printf("'%s' performance: %d/%d (%.3f)\n", t.Name, stats.correctGuesses, totalGuesses, accuracy)

			errors := CountedErrors{}
			for key, value := range stats.errors {
				errors = append(errors, &CountedTaggerError { key, value })
			}

			sort.Sort(errors)
			const errorTopSize = 5
			fmt.Printf("\tTop %d errors:\n", errorTopSize)
			for i := 0; i < len(errors) && i < errorTopSize; i++ {
				err := errors[i].error
				fmt.Printf("\t%d: '%s' -> '%s' instead of '%s'\n", errors[i].count, err.word, err.actualPos, err.expectedPos)
			}
		}
	}
}