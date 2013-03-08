package main

import (
	"bufio"
	"fmt"
	"io"
	"log"
	"math"
	"os"
	"strconv"
	"strings"
	"unicode"
)

type TaggedPair struct {
	word string
	tag string
}

type Bigram struct {
	u, v string
}

type Trigram struct {
	u, v, w string
}

type Sentence []*TaggedPair

func main() {
	if len(os.Args) != 4 {
		fmt.Fprintf(os.Stderr, "Usage: LEARN_FILE TEST_FILE MODE\n")
		os.Exit(1)
	}

	readCorpus := func(filename string, labeled bool, handler func(Sentence)) {
		f, err := os.Open(filename)
		if err != nil {
			log.Fatal(err)
		}
		defer f.Close()

		in := bufio.NewReader(f)
		current := Sentence{}
		for {
			line, err := in.ReadString('\n')
			if err == io.EOF {
				return
			}
			if err != nil {
				log.Fatal(err)
			}

			line = strings.TrimRight(line, "\n")
			if line == "" {
				handler(current)
				current = Sentence{}
				continue
			}
			if labeled {
				tokens := strings.Split(line, " ")
				current = append(current, &TaggedPair{ tokens[0], tokens[1] })
			} else {
				current = append(current, &TaggedPair{ line, "" })
			}
		}
	}

	learn, test := os.Args[1], os.Args[2]
	mode, err := strconv.Atoi(os.Args[3])
	if err != nil {
		log.Fatal(err)
	}

	corpus := []Sentence{}
	wordCounts := make(map[string]uint)
	tagCounts := make(map[string]uint)
	tagBigramCounts := make(map[Bigram]uint)
	tagTrigramCounts := make(map[Trigram]uint)

	getTag := func(sent Sentence, idx int) string {
		if idx < 0 {
			return "*"
		}
		if idx >= len(sent) {
			return "STOP"
		}
		return sent[idx].tag
	}

	readCorpus(learn, true, func(sent Sentence) {
		corpus = append(corpus, sent)
		tagCounts["*"] += 2
		tagBigramCounts[Bigram{"*", "*"}]++
		for i := 0; i <= len(sent); i++ {
			if i < len(sent) {
				wordCounts[sent[i].word]++
			}

			u, v, w := getTag(sent, i - 2), getTag(sent, i - 1), getTag(sent, i)
			tagCounts[w]++
			tagBigramCounts[Bigram{v, w}]++
			tagTrigramCounts[Trigram{u, v, w}]++
		}
	});

	smartRemap := mode == 3
	remapWord := func(s string) string {
		if wordCounts[s] >= 5 {
			return s
		}

		if smartRemap {
			allCapitals := true
			lastCapital := false
			for _, ch := range s {
				if unicode.IsDigit(ch) {
					return "_NUMERIC_"
				}
				isUpper := unicode.IsUpper(ch)
				allCapitals = allCapitals && isUpper
				lastCapital = isUpper
			}
			if allCapitals {
				return "_ALL_CAPITALS_"
			} else if lastCapital {
				return "_LAST_CAPITAL_"
			}
		}
		return "_RARE_"
	}

	for _, sent := range corpus {
		for _, pair := range sent {
			pair.word = remapWord(pair.word)
		}
	}

	tagWordPairings := make(map[Bigram]uint)
	for _, sent := range corpus {
		for _, pair := range sent {
			tagWordPairings[Bigram{pair.tag, pair.word}]++
		}
	}

	type ViterbiState struct {
		pos int
		u, v string
	}

	dummyTags := []string{"*"}
	allTags := []string{}
	for key, _ := range tagCounts {
		if key != "STOP" && key != "*" {
			allTags = append(allTags, key)
		}
	}

	getTagSet := func(idx int) []string {
		if idx < 0 {
			return dummyTags
		}
		return allTags
	}

	getE := func(tag string, word string) float64 {
		num := tagWordPairings[Bigram{tag, word}]
		denom := tagCounts[tag]
		return math.Log2(float64(num) / float64(denom))
	}

	getQ := func(u, v, w string) float64 {
		num, ok := tagTrigramCounts[Trigram{u, v, w}]
		if !ok {
			log.Fatalf("Trigram (%s, %s, %s) was not in the learning set\n", u, v, w)
		}
		denom, ok := tagBigramCounts[Bigram{u, v}]
		if !ok {
			log.Fatalf("Bigram (%s, %s) was not in the learning set\n", u, v)
		}
		return math.Log2(float64(num)/float64(denom))
	}

	readCorpus(test, false, func(sent Sentence) {
		n := len(sent)
		π := make(map[ViterbiState]float64)
		bp := make(map[ViterbiState]string)

		π[ViterbiState{-1, "*", "*"}] = 0.0

		for k := 0; k < n; k++ {
			tagSetW, tagSetU, tagSetV := getTagSet(k - 2), getTagSet(k - 1), getTagSet(k)
			for _, u := range tagSetU {
				for _, v := range tagSetV {
					e := getE(v, remapWord(sent[k].word))
					currentΠ := math.Inf(-1)
					currentBp := ""
					for _, w := range tagSetW {
						prevState := ViterbiState{k - 1, w, u}
						newΠ := π[prevState] + e
						if mode >= 2 {
							q := getQ(w, u, v)
							newΠ += q
						}

						if newΠ >= currentΠ {
							currentΠ = newΠ
							currentBp = w
						}
					}

					newState := ViterbiState{k, u, v}
					π[newState] = currentΠ
					bp[newState] = currentBp
				}
			}
		}

		bestProbability := math.Inf(-1)
		bestU, bestV := "", ""
		for _, u := range getTagSet(n - 2) {
			for _, v := range getTagSet(n - 1) {
				currentProbability := π[ViterbiState{n - 1, u, v}]
				if mode >= 2 {
					currentProbability += getQ(u, v, "STOP")
				}
				if currentProbability >= bestProbability {
					bestProbability = currentProbability
					bestU = u
					bestV = v
				}
			}
		}

		if len(sent) > 0 {
			sent[n - 1].tag = bestV
			if len(sent) > 1 {
				sent[n - 2].tag = bestU
				for k := n - 3; k >= 0; k-- {
					sent[k].tag = bp[ViterbiState{k + 2, sent[k + 1].tag, sent[k + 2].tag}]
				}
			}
		}

		for _, pair := range sent {
			fmt.Printf("%s %s\n", pair.word, pair.tag)
		}
		fmt.Println()
	})
}