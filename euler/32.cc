#include <algorithm>
#include <iostream>
#include <vector>
#include <cstdio>

using namespace std;

int Prod(const vector<int>& v, int stop_at, int& aout, int& bout) {
  int a = 0;
  int b = 0;
  for (int i = 0; i < stop_at; i++) {
    a = a * 10 + v[i];
  }
  for (int i = stop_at; i < v.size(); i++) {
    b = b * 10 + v[i];
  }
  aout = a;
  bout = b;
  return a * b;
}

bool Ok(int num) {
  vector<int> digs;
  for (int z = num; z; z /= 10) {
    digs.push_back(z % 10);
  }
  vector<int> perm;
  for (int i = 1; i <= 9; i++) {
    if (find(digs.begin(), digs.end(), i) == digs.end()) {
      perm.push_back(i);
    }
  }
  do {
    int a, b;
    if (Prod(perm, 1, a, b) == num) {
      printf("%d * %d = %d\n", a, b, num);
      return true;
    }
    if (Prod(perm, 2, a, b) == num) {
      printf("%d * %d = %d\n", a, b, num);
      return true;
    }
  } while (next_permutation(perm.begin(), perm.end()));
  return false;
}

int main() {
  int res = 0;
  for (int i = 1234; i <= 9876; i++) {
    if (Ok(i)) {
      res += i;
    }
  }
  cout << res;
}