module common;

import std.array;
import std.stdio;
import std.string;

alias string[][] Corpus;
alias double[string][string] TProbs;
alias double[ulong][ulong][ulong][ulong] QProbs;

immutable nullWord = "<HİÇ>";

auto ParseCorpus(string fileName, bool includeNull) {
	Corpus result;
	foreach(line; File(fileName).byLine()) {
		auto toAppend = (includeNull ? [nullWord] : []) ~ cast(string[])(line.dup.split());
		result ~= toAppend;
	}
	return result;
}

void DumpProbs(TProbs tProbs, QProbs qProbs, string fileName) {
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

void LoadProbs(TProbs tProbs, QProbs qProbs, string fileName) {
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