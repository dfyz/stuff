module common;

import std.array;
import std.conv;
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

void LoadProbs(TProbs* tProbs, QProbs* qProbs, string fileName) {
	foreach(line; File(fileName ~ ".tprobs").byLine()) {
		auto tokens = cast(string[])(line.dup.split(" "));
		(*tProbs)[tokens[0]][tokens[1]] = to!double(tokens[2]);
	}

	foreach(line; File(fileName ~ ".qprobs").byLine()) {
		auto tokens = cast(string[])(line.dup.split());
		(*qProbs)[to!ulong(tokens[0])][to!ulong(tokens[1])][to!ulong(tokens[2])][to!ulong(tokens[3])] = to!double(tokens[4]);
	}
}