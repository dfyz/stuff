using System.Collections.Generic;
using System.Linq;
using NUnit.Framework;

namespace Sieve
{
	[TestFixture]
	public class WheelElement_Test
	{
		[Test]
		public void TestFirst4Primes()
		{
			const int Count = 10000;
			var expected = Enumerable.Range(2, Count).Where(ShouldBeInWheelForFirst4Primes);
			var actual = new List<long>();
			var wheelElement = new WheelElement(4);
			while (wheelElement.Number <= Count + 1)
			{
				actual.Add(wheelElement.Number);
				wheelElement = wheelElement.Next();
			}
			CollectionAssert.AreEqual(expected.ToList(), actual);
		}

		[Test]
		public void TestImmutability()
		{
			var wheelElement = new WheelElement(2);
			long oldValue = wheelElement.Number;
			wheelElement.Next();
			Assert.AreEqual(oldValue, wheelElement.Number);
		}

		private bool ShouldBeInWheelForFirst4Primes(int n)
		{
			return First4Primes.All(x => n % x != 0);
		}

		private readonly int[] First4Primes = { 2, 3, 5, 7 };
	}
}