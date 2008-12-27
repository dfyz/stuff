using System.Collections;
using System.Collections.Generic;

namespace Sieve
{
	public class Sieve : IEnumerable<long>
	{
		public IEnumerator<long> GetEnumerator()
		{
			foreach (var p in smallPrimes)
			{
				yield return p;
			}

			var pq = new PriorityQueue<PrimeSequence>();
			var current = new WheelElement(smallPrimes.Length);
			while (true)
			{
				bool isPrime = true;
				while (!pq.IsEmpty() && pq.Min().Current <= current.Number)
				{
					if (pq.Min().Current == current.Number)
					{
						isPrime = false;
					}
					PrimeSequence ps = pq.Min();
					pq.DeleteMin();
					ps.MoveToNext();
					pq.Insert(ps);
				}
				if (isPrime)
				{
					yield return current.Number;
					pq.Insert(new PrimeSequence(current.Number, current));
				}
				current = current.Next();
			}
		}

		IEnumerator IEnumerable.GetEnumerator()
		{
			return GetEnumerator();
		}

		private static readonly long[] smallPrimes = { 2, 3, 5, 7 };
	}
}