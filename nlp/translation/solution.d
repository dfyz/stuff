import common;

import std.algorithm;
import std.array;
import std.conv;
import std.container;
import std.file;
import std.stdio;
import std.string;
import std.typecons;

alias RedBlackTree!(Tuple!(ulong, ulong))[ulong] AlignmentSet;

auto GetOneWayAlignments(string probsFileName, string foreignCorpusFileName, string nativeCorpusFileName, bool reverse) {
	auto alignmentFileName = probsFileName ~ ".alignments";

	if (!exists(alignmentFileName)) {
		TProbs tProbs;
		QProbs qProbs;

		LoadProbs(&tProbs, &qProbs, probsFileName);

		auto foreignCorpus = ParseCorpus(foreignCorpusFileName, false);
		auto nativeCorpus = ParseCorpus(nativeCorpusFileName, true);
		assert(foreignCorpus.length == nativeCorpus.length);
		auto f = File(alignmentFileName, "w");
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
					auto num1 = reverse ? foreignIdx + 1 : bestNativeIdx;
					auto num2 = reverse ? bestNativeIdx : foreignIdx + 1;
					f.writefln("%s %s %s", i + 1, num1, num2);
				}
			}
		}
	}

	AlignmentSet result;
	foreach(line; File(alignmentFileName).byLine()) {
		auto tokens = cast(string[])(line.dup.split());
		auto sentenceIdx = to!ulong(tokens[0]);
		auto nativeIdx = to!ulong(tokens[1]);
		auto foreignIdx = to!ulong(tokens[2]);
		auto tuples = sentenceIdx in result;
		if (sentenceIdx !in result) {
			result[sentenceIdx] = new RedBlackTree!(Tuple!(ulong, ulong))();
		}
		result[sentenceIdx].insert(Tuple!(ulong, ulong)(nativeIdx, foreignIdx));
	}
	return result;
}

void main(string[] args) {
	stderr.writeln("Computing SP -> EN alignments");
	auto spEnAlignments = GetOneWayAlignments(args[3], args[1], args[2], false);
	stderr.writeln("Computing EN -> SP alignments");
	auto enSpAlignments = GetOneWayAlignments(args[4], args[2], args[1], true);

	foreach(sentenceIdx; 1 .. spEnAlignments.length + 1) {
		auto spEn = spEnAlignments[sentenceIdx];
		auto enSp = enSpAlignments[sentenceIdx];
		auto answer = new RedBlackTree!(Tuple!(ulong, ulong))(array(setIntersection(spEn[], enSp[])));
		auto candidates = new RedBlackTree!(Tuple!(ulong, ulong))(array(setUnion(spEn[], enSp[])));
		auto added = true;

		auto takenSpIndexes = new RedBlackTree!(ulong)();
		auto takenEnIndexes = new RedBlackTree!(ulong)();
		foreach(t; answer) {
			takenEnIndexes.insert(t[0]);
			takenSpIndexes.insert(t[1]);
		}

		while (added) {
			added = false;
			foreach(cand; candidates) {
				auto en = cand[0];
				auto sp = cand[1];
				if (en in takenEnIndexes || sp in takenSpIndexes) {
					continue;
				}
				if (Tuple!(ulong, ulong)(en - 1, sp) in answer ||
					Tuple!(ulong, ulong)(en + 1, sp) in answer ||
					Tuple!(ulong, ulong)(en, sp - 1) in answer ||
					Tuple!(ulong, ulong)(en, sp + 1) in answer) {
					answer.insert(Tuple!(ulong, ulong)(en, sp));
					takenEnIndexes.insert(en);
					takenSpIndexes.insert(sp);
				}
			}
		}

		foreach(ans; answer) {
			writefln("%s %s %s", sentenceIdx, ans[0], ans[1]);
		}
	}
}
