using System.IO;
using System.Linq;
using System.Reflection;
using NUnit.Framework;

namespace Sieve
{
	[TestFixture]
	public class Sieve_Test
	{
		[Test]
		public void TestFirstPrimes()
		{
			var sieve = new Sieve();
			var firstPrimes = new[] { 2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113 };
			Assert.AreEqual(firstPrimes, sieve.Take(firstPrimes.Length));
		}

		[Test]
		public void TestThousandPrimes()
		{
			var sieve = new Sieve();
			Assert.AreEqual(precomputedPrimes.Take(1000), sieve.Take(1000));
		}

		[Test]
		[Timeout(30000)]
		public void PerformanceTest()
		{
			Assert.AreEqual(104729, new Sieve().Take(10000).Last());
			Assert.AreEqual(15485863, new Sieve().Take(1000000).Last());
		}

		[SetUp]
		public void ReadMilionPrimes()
		{
			Stream resourceStream = Assembly.GetExecutingAssembly().GetManifestResourceStream("Sieve.primes.txt");
			var reader = new StreamReader(resourceStream);
			precomputedPrimes = reader.ReadToEnd().Split(',').Select(x => int.Parse(x)).ToArray();
		}

		private int[] precomputedPrimes;
	}
}