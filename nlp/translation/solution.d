import std.array;
import std.container;
import std.stdio;
import std.string;

immutable nullWord = "<HİÇ>";

alias string[][] Corpus;
alias double[string][string] TProbs;
alias double[ulong][ulong][ulong][ulong] QProbs;

auto ParseCorpus(string fileName, bool includeNull) {
	Corpus result;
	foreach(line; File(fileName).byLine()) {
		auto toAppend = (includeNull ? [nullWord] : []) ~ cast(string[])(line.dup.split());
		result ~= toAppend;
	}
	return result;
}

void RunEM(Corpus spCorpus, Corpus enCorpus, TProbs tProbs, QProbs qProbs, bool useQ) {
	immutable iterationCount = 5;
	foreach(iter; 1 .. iterationCount + 1) {
		stderr.writefln("Running EM iteration #%s (use q(* | *, *, *) = %s)", iter, useQ);
		double[ulong][ulong][ulong][ulong] fourgramCounts;
		double[ulong][ulong][ulong] trigramCounts;
		double[string][string] bigramCounts;
		double[string] unigramCounts;
		foreach(i; 0 .. spCorpus.length) {
			auto spLength = spCorpus[i].length;
			auto enLength = enCorpus[i].length;
			foreach(spIdx, spWord; spCorpus[i]) {
				auto denominator = 0.0;
				foreach(enIdx, enWord; enCorpus[i]) {
					auto toAdd = tProbs[spWord][enWord];
					if (useQ) {
						toAdd *= qProbs[spLength][enLength][spIdx][enIdx];
					}
					denominator += toAdd;
				}
				foreach(enIdx, enWord; enCorpus[i]) {
					auto numerator = tProbs[spWord][enWord];
					if (useQ) {
						numerator *= qProbs[spLength][enLength][spIdx][enIdx];
					}
					auto delta = numerator / denominator;
					fourgramCounts[spLength][enLength][spIdx][enIdx] += delta;
					trigramCounts[spLength][enLength][spIdx] += delta;
					bigramCounts[spWord][enWord] += delta;
					unigramCounts[enWord] += delta;
				}
			}
		}

		foreach(spWord, enWords; tProbs) {
			foreach(enWord, ref val; enWords) {
				val = bigramCounts[spWord][enWord] / unigramCounts[enWord];
			}
		}

		foreach(spLength; qProbs.keys) {
			foreach(enLength; qProbs[spLength].keys) {
				foreach(spIdx; qProbs[spLength][enLength].keys) {
					foreach(enIdx, ref val; qProbs[spLength][enLength][spIdx]) {
						val = fourgramCounts[spLength][enLength][spIdx][enIdx] / trigramCounts[spLength][enLength][spIdx];
					}
				}
			}
		}
	}
}

void Dump(TProbs tProbs, QProbs qProbs, string fileName) {
	auto tFile = File(fileName ~ ".tprobs", "w");
	foreach(spWord, enWords; tProbs) {
		foreach(enWord, val; enWords) {
			tFile.writefln("%s %s %s", spWord, enWord, val);
		}
	}

	auto qFile = File(fileName ~ ".qprobs", "w");
	foreach(spLength; qProbs.keys) {
		foreach(enLength; qProbs[spLength].keys) {
			foreach(spIdx; qProbs[spLength][enLength].keys) {
				foreach(enIdx, val; qProbs[spLength][enLength][spIdx]) {
					qFile.writefln("%s %s %s %s %s", spLength, enLength, spIdx, enIdx, val);
				}
			}
		}
	}
}

void Load(TProbs tProbs, QProbs qProbs, string fileName) {
	auto tFile = File(fileName ~ ".tprobs");
	string spWord;
	string enWord;
	double tProb;
	while (tFile.readf("%s %s %s", &spWord, &enWord, &tProb) > 0) {
		tProbs[spWord][enWord] = tProb;
	}

	auto qFile = File(fileName ~ ".qprobs");
	ulong spLength;
	ulong enLength;
	ulong spIdx;
	ulong enIdx;
	double qProb;
	while (qFile.readf("%s %s %s %s %s", &spLength, &enLength, &spIdx, &enIdx, &qProb) > 0) {
		qProbs[spLength][enLength][spIdx][enIdx] = qProb;
	}
}

void main(string[] args) {
	stderr.writeln("Reading corpora");
	auto spCorpus = ParseCorpus(args[3], false);
	auto enCorpus = ParseCorpus(args[4], true);

	assert(spCorpus.length == enCorpus.length);

	TProbs tProbs;
	QProbs qProbs;
	RedBlackTree!(string)[string] enWordPairings;

	stderr.writeln("Estimating initial alignment probabilities");
	foreach(i; 0 .. spCorpus.length) {
		foreach(spWord; spCorpus[i]) {
			foreach(enWord; enCorpus[i]) {
				tProbs[spWord][enWord] = 0.0;
				if (enWord !in enWordPairings) {
					enWordPairings[enWord] = new RedBlackTree!(string)();
				}
				enWordPairings[enWord].insert(spWord);
			}
		}
	}

	foreach(spWord, enWords; tProbs) {
		foreach(enWord, ref val; enWords) {
			val = 1.0 / enWordPairings[enWord].length;
		}
	}

	foreach(i; 0 .. spCorpus.length) {
		auto spLength = spCorpus[i].length;
		auto enLength = enCorpus[i].length;
		foreach(spIdx; 0 .. spCorpus[i].length) {
			foreach(enIdx; 0 .. enCorpus[i].length) {
				qProbs[spLength][enLength][spIdx][enIdx] = 1.0 / enLength;
			}
		}
	}

	RunEM(spCorpus, enCorpus, tProbs, qProbs, false);
	RunEM(spCorpus, enCorpus, tProbs, qProbs, true);

	Dump(tProbs, qProbs, args[5]);

	stderr.writeln("Reading test data");
	spCorpus = ParseCorpus(args[1], false);
	enCorpus = ParseCorpus(args[2], true);
	foreach(i; 0 .. spCorpus.length) {
		auto spLength = spCorpus[i].length;
		auto enLength = enCorpus[i].length;
		foreach(spIdx, spWord; spCorpus[i]) {
			auto bestProb = qProbs[spLength][enLength][spIdx][0] * tProbs[spWord][nullWord];
			ulong bestEnIdx = 0;
			foreach(enIdx, enWord; enCorpus[i][1..$]) {
				auto newProb = qProbs[spLength][enLength][spIdx][enIdx + 1] * tProbs[spWord][enWord];
				if (newProb > bestProb) {
					bestProb = newProb;
					bestEnIdx = enIdx + 1;
				}
			}
			if (bestEnIdx > 0) {
				writefln("%s %s %s", i + 1, bestEnIdx, spIdx + 1);			
			}
		}
	}
}
