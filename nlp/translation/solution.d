import common;

import std.stdio;

void main(string[] args) {
	TProbs tProbs;
	QProbs qProbs;

	LoadProbs(tProbs, qProbs, args[3]);

	stderr.writeln("Reading test data");
	auto spCorpus = ParseCorpus(args[1], false);
	auto enCorpus = ParseCorpus(args[2], true);
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
