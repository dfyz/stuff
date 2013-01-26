package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
	"time"
)

var in = bufio.NewReader(os.Stdin)
var out = bufio.NewWriter(os.Stdout)

func readTokens() []string {
	line, _ := in.ReadString('\n')
	return strings.SplitN(line[:len(line) - 1], " ", -1)
}

func parseValidInt(s string) int {
	result, _ := strconv.Atoi(s)
	return result
}

func readIntPair() (r1 int, r2 int) {
	ints := readTokens()
	r1, r2 = parseValidInt(ints[0]), parseValidInt(ints[1])
	return
}

func readInt() int {
	return parseValidInt(readTokens()[0])
}

func main() {
	start := time.Now()

	n := readInt()
	sum := make([]int, n + 1)
	for i := 0; i < n; i++ {
		k := readInt()
		sum[i + 1] = sum[i] + k
	}
	m := readInt()
	for i := 0; i < m; i++ {
		from, to := readIntPair()
		fmt.Fprintln(out, sum[to] - sum[from - 1])
	}

	out.Flush()
	end := time.Now()
	fmt.Fprintf(os.Stderr, "\tTime inside the program: %.2f\n", float64(end.UnixNano() - start.UnixNano())/1e9)
}
