using System.Collections.Generic;
using System.Linq;

namespace Sieve
{
	public class WheelElement
	{
		public WheelElement(int primeCount)
		{
			var primes = new List<long>();
			for (int i = 2; primes.Count != primeCount; i++)
			{
				if (!DivisibleByAny(i, primes))
				{
					primes.Add(i);
				}
			}
			long product = primes.Aggregate(1L, (x, y) => x*y);
			long startFrom = primes.Last() + 1;
			while (DivisibleByAny(startFrom, primes))
			{
				startFrom++;
			}
			for (long i = 1, prev = startFrom; i <= product; i++)
			{
				long candidate = startFrom + i;
				if (!DivisibleByAny(candidate, primes))
				{
					wheel.Add(candidate - prev);
					prev = candidate;
				}
			}
			number = startFrom;
		}

		public WheelElement Next()
		{
			return new WheelElement(Number + wheel[wheelIdx], (wheelIdx + 1) % wheel.Count, wheel);
		}

		public long Number { get { return number;  } }

		private WheelElement(long number, int wheelIdx, List<long> wheel)
		{
			this.number = number;
			this.wheelIdx = wheelIdx;
			this.wheel = wheel;
		}

		private bool DivisibleByAny(long n, IEnumerable<long> candidates)
		{
			return candidates.Any(x => n % x == 0);
		}

		private readonly List<long> wheel = new List<long>();
		private readonly int wheelIdx;
		private readonly long number;
	}
}