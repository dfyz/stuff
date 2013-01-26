#include <cstdio>
#include <vector>

#include <sys/time.h>

suseconds_t GetTime() {
	struct timeval now;
	gettimeofday(&now, NULL);
	return now.tv_sec*1000000 + now.tv_usec;
}

int main() {
	suseconds_t start = GetTime();

	int n = 0;
	scanf("%d", &n);
	std::vector<int> v(n + 1);
	int k = 0;
	for (int i = 0; i < n; i++) {
		scanf("%d", &k);
		v[i + 1] = v[i] + k;
	}

	int m = 0;
	scanf("%d", &m);
	int first = 0;
	int second = 0;
	for (int i = 0; i < m; i++) {
		scanf("%d %d", &first, &second);
		printf("%d\n", v[second] - v[first - 1]);
	}

	suseconds_t end = GetTime();

	fprintf(stderr, "\tTime inside the program: %.2f\n", (end - start) / 1e6);
}
