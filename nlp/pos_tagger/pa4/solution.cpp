#include <cstdarg>
#include <cstdio>
#include <iostream>
#include <fstream>
#include <map>
#include <sstream>
#include <string>
#include <vector>

using std::map;
using std::string;
using std::vector;

typedef long long TValue;
typedef vector<string> TFeatureName;

enum ETag {
	ET_NOTHING = 0,
	ET_O,
	ET_I_GENE,
	ET_TAG_COUNT,
};

void Ensure(bool condition, const char* format, ...) {
	if (!condition) {
		va_list args;
		va_start(args, format);
		vfprintf(stderr, format, args);
		va_end(args);
		exit(1);
	}
}

string TagToString(ETag tag) {
	switch (tag) {
	case ET_NOTHING:
		return "*";
	case ET_O:
		return "O";
	case ET_I_GENE:
		return "I-GENE";
	default:
		Ensure(false, "Tried to convert %d to string\n", tag);
		return string();
	}
}

ETag GetTagAt(const vector<ETag>& tags, size_t pos, size_t offset) {
	return pos <= offset ? ET_NOTHING : tags[pos - 1 - offset];
}

vector<TFeatureName> ComputeFeatures(ETag u, ETag v, ETag w, const vector<string>& sentence, size_t pos) {
	vector<TFeatureName> result;

	TFeatureName f1;
	f1.push_back("TRIGRAM");
	f1.push_back(TagToString(u));
	f1.push_back(TagToString(v));
	f1.push_back(TagToString(w));
	result.push_back(f1);

	if (pos > sentence.size()) {
		return result;
	}

	const std::string& word = sentence[pos - 1];
	TFeatureName f2;
	f2.push_back("TAG");
	f2.push_back(word);
	f2.push_back(TagToString(w));
	result.push_back(f2);

	for (size_t suffixLength = 1; suffixLength <= 3; suffixLength++) {
		if (word.size() < suffixLength) {
			break;
		}
		TFeatureName fSuffix;
		fSuffix.push_back("SUFFIX");
		fSuffix.push_back(word.substr(word.size() - suffixLength));
		fSuffix.push_back(TagToString(w));
		result.push_back(fSuffix);
	}

	return result;
}

class TFeatures {
public:
	void SetWeight(const TFeatureName& name, TValue weight) {
		Weights[ToStringName(name)] = weight;
	}

	TValue GetWeight(const TFeatureName& name) const {
		auto stringName = ToStringName(name);
		auto it = Weights.find(stringName);
		return it == Weights.end() ? 0 : it->second;
	}

	void Save(const string& fileName) const {
		std::ofstream out(fileName.c_str());
		for (auto kvp: Weights) {
			if (kvp.second != 0) {
				out << kvp.first << " " << kvp.second << std::endl;
			}
		}
	}

	void Load(const string& fileName) {
		std::ifstream in(fileName.c_str());
		string line;
		while (getline(in, line)) {
			TFeatureName name;
			size_t i = 0;
			while (i < line.size()) {
				size_t j = line.find(' ', i);
				if (j == string::npos) {
					std::istringstream iStr(line.substr(i));
					TValue weight;
					iStr >> weight;
					SetWeight(name, weight);
					break;
				} else {
					name.push_back(line.substr(i, j - i));
				}
				i = j + 1;
			}
		}
	}

	void UpdateWeights(const vector<string>& sentence, const vector<ETag>& tagging, TValue delta) {
		for (size_t pos = 1; pos <= sentence.size(); pos++) {
			ETag u = GetTagAt(tagging, pos, 2);
			ETag v = GetTagAt(tagging, pos, 1);
			ETag w = GetTagAt(tagging, pos, 0);
			auto featureList = ComputeFeatures(u, v, w, sentence, pos);
			for (auto f: featureList) {
				SetWeight(f, GetWeight(f) + delta);
			}
		}
	}

private:
	string ToStringName(const TFeatureName& name) const {
		string result;
		for (size_t i = 0; i < name.size(); i++) {
			if (i > 0) {
				result.append(" ");
			}
			result += name[i];
		}
		return result;
	}

	map<string, TValue> Weights;
};

TValue LocalScore(const TFeatures& features, ETag u, ETag v, ETag w, const vector<string>& sentence, size_t pos) {
	auto featureList = ComputeFeatures(u, v, w, sentence, pos);
	TValue result = 0;
	for (auto f: featureList) {
		result += features.GetWeight(f);
	}
	return result;
}

ETag GetFirstTag(size_t pos, size_t offset) {
	return pos <= offset ? ET_NOTHING : ET_O;
}

ETag GetLastTag(size_t pos, size_t offset) {
	return pos <= offset ? ET_NOTHING : ET_I_GENE;
}

vector<ETag> MostProbableTagging(const TFeatures& features, const vector<string>& sentence) {
	size_t n = sentence.size();
	vector<vector<vector<TValue> > > dp(n + 1, vector<vector<TValue> >(ET_TAG_COUNT, vector<TValue>(ET_TAG_COUNT)));
	vector<vector<vector<ETag> > > backpointers(n + 1, vector<vector<ETag> >(ET_TAG_COUNT, vector<ETag>(ET_TAG_COUNT)));

	for (size_t pos = 1; pos <= n; pos++) {
		for (ETag w = GetFirstTag(pos, 0); w <= GetLastTag(pos, 0); w++) {
			for (ETag v = GetFirstTag(pos, 1); v <= GetLastTag(pos, 1); v++) {
				TValue bestWeight = std::numeric_limits<TValue>::min();
				ETag bp = ET_TAG_COUNT;
				for (ETag u = GetFirstTag(pos, 2); u <= GetLastTag(pos, 2); u++) {
					TValue newWeight = dp[pos - 1][u][v] + LocalScore(features, u, v, w, sentence, pos);
					if (newWeight >= bestWeight) {
						bestWeight = newWeight;
						bp = u;
					}
				}
				Ensure(bp != ET_TAG_COUNT, "Couldn't find a backpointer for position %u\n", pos);
				dp[pos][v][w] = bestWeight;
				backpointers[pos][v][w] = bp;
			}
		}
	}

	TValue bestWeight = std::numeric_limits<TValue>::min();;
	ETag bestV = ET_TAG_COUNT;
	ETag bestW = ET_TAG_COUNT;

	for (ETag v = GetFirstTag(n, 1); v <= GetLastTag(n, 1); v++) {
		for (ETag w = GetFirstTag(n, 0); w <= GetLastTag(n, 0); w++) {
			TValue newWeight = dp[n][v][w];
			if (newWeight >= bestWeight) {
				bestWeight = newWeight;
				bestV = v;
				bestW = w;
			}
		}
	}

	Ensure(bestW != ET_TAG_COUNT, "Couldn't find the tag for the last word\n");
	Ensure(bestV != ET_TAG_COUNT, "Couldn't find the tag for the last-but-one word\n");

	vector<ETag> result(n);
	if (sentence.size() > 0) {
		result[n - 1] = bestW;
		if (sentence.size() > 1) {
			result[n - 2] = bestV;
			if (sentence.size() > 2) {
				for (size_t k = n - 3; ; k--) {
					result[k] = backpointers[k + 3][result[k + 1]][result[k + 2]];
					if (k == 0) {
						break;
					}
				}
			}
		}
	}
	return result;
}

typedef vector<std::pair<vector<std::string>, vector<ETag> > > TTrainingSet;

void RunPerceptron(TFeatures& features, const TTrainingSet& learn) {
	static const size_t iterationCount = 12;
	for (size_t iter = 1; iter <= iterationCount; ++iter) {
		std::cerr << "Perceptron: starting iteration " << iter << std::endl;
		size_t progress = 0;
		for (auto sample: learn) {
			++progress;
			auto ours = MostProbableTagging(features, sample.first);
			if (progress % 1000 == 0) {
				std::cerr << progress << "/" << learn.size() << std::endl;
			}
			if (ours != sample.second) {
				features.UpdateWeights(sample.first, sample.second, 1);
				features.UpdateWeights(sample.first, ours, -1);
			}
		}
		std::cerr << "Ended iteration " << iter << std::endl;
	}
}

bool FileExists(const char* fileName) {
	std::ifstream in(fileName);
	return in.good();
}

int main(int argc, char const *argv[]) {
	const string weightFile = argv[3];
	if (!FileExists(weightFile.c_str())) {
		std::ifstream learnFile(argv[1]);
		string line;
		TTrainingSet learn;
		vector<string> sentence;
		vector<ETag> tagging;
		std::cerr << "Reading the training corpus" << std::endl;
		while (std::getline(learnFile, line)) {
			if (line.empty()) {
				learn.push_back(std::make_pair(sentence, tagging));
				sentence.clear();
				tagging.clear();
				continue;
			}
			string word;
			string tag;
			std::istringstream iStr(line);
			iStr >> word >> tag;
			sentence.push_back(word);
			if (tag == "O") {
				tagging.push_back(ET_O);
			} else if (tag == "I-GENE") {
				tagging.push_back(ET_I_GENE);
			} else {
				Ensure(false, "Unknown tag: %s\n", tag.c_str());
			}
		}
		if (!sentence.empty()) {
			learn.push_back(std::make_pair(sentence, tagging));
		}

		TFeatures features;
		RunPerceptron(features, learn);
		features.Save(weightFile);
	}

	TFeatures features;
	features.Load(weightFile);

	std::cerr << "Tagging the test set" << std::endl;
	std::ifstream testFile(argv[2]);
	vector<string> sentence;
	string line;
	while (std::getline(testFile, line)) {
		if (line.empty()) {
			auto tagging = MostProbableTagging(features, sentence);
			for (size_t i = 0; i < sentence.size(); i++) {
				std::cout << sentence[i] << " " << TagToString(tagging[i]) << std::endl;
			}
			std::cout << std::endl;
			sentence.clear();
			continue;
		}
		sentence.push_back(line);
	}
}
