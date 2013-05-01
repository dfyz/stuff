import common;

import std.stdio;

alias ulong[ulong][ulong] AlignmentSet;

auto GetOneWayAlignments(string probsFileName, string foreignCorpusFileName, string nativeCorpusFileName) {
	TProbs tProbs;
	QProbs qProbs;

	LoadProbs(tProbs, qProbs, probsFileName);

	stderr.writeln("Reading test data");
	auto foreignCorpus = ParseCorpus(foreignCorpusFileName, false);
	auto nativeCorpus = ParseCorpus(nativeCorpusFileName, true);
	AlignmentSet result;
	foreach(i; 0 .. foreignCorpus.length) {
		auto foreignLength = foreignCorpus[i].length;
		auto nativeLength = nativeCorpus[i].length;
		foreach(foreignIdx, foreignWord; foreignCorpus[i]) {
			auto bestProb = qProbs[foreignLength][nativeLength][foreignIdx][0] * tProbs[foreignWord][nullWord];
			ulong bestNativeIdx = 0;
			foreach(nativeIdx, nativeWord; nativeCorpus[i][1..$]) {
				auto newProb = qProbs[foreignLength][nativeLength][foreignIdx][nativeIdx + 1] * tProbs[foreignWord][nativeWord];
				if (newProb > bestProb) {
					bestProb = newProb;
					bestNativeIdx = nativeIdx + 1;
				}
			}
			if (bestNativeIdx > 0) {
				result[i + 1][foreignIdx + 1] = bestNativeIdx;
			}
		}
	}	
}

void main(string[] args) {

}
