import common;

import std.container;
import std.stdio;

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

void main(string[] args) {
	stderr.writeln("Reading corpora");
	auto spCorpus = ParseCorpus(args[1], false);
	auto enCorpus = ParseCorpus(args[2], true);

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

	DumpProbs(tProbs, qProbs, args[2]);
}
