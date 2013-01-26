import java.io.{BufferedReader,InputStreamReader,OutputStreamWriter,PrintWriter,StreamTokenizer};
import java.util.Scanner;

object Problem1330 {
	val in = new StreamTokenizer(new BufferedReader(new InputStreamReader(System.in)))

	def nextInt() : Int = {
		in.nextToken()
		return in.nval.toInt
	}

	def main(args: Array[String]) {
		val start = System.nanoTime()

		val n = nextInt()
		var sum = new Array[Int](n + 1)
		for (i <- 0 until n) {
			var k = nextInt()
			sum(i + 1) = sum(i) + k
		}

		val out = new PrintWriter(new OutputStreamWriter(System.out))
		val m = nextInt()
		for (_ <- 1 to m) {
			val from = nextInt()
			val to = nextInt()
			out.println(sum(to) - sum(from - 1))
		}
		out.flush()

		val end = System.nanoTime()
		System.err.printf("\tTime inside the program: %.2f\n", double2Double((end - start)/1e9))
	}
}