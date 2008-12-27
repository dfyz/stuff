using System;

namespace Sieve
{
	public class PrimeSequence : IComparable<PrimeSequence>
	{
		public int CompareTo(PrimeSequence other)
		{
			return Current.CompareTo(other.Current);
		}

		public PrimeSequence(long prime, WheelElement multiplyBy)
		{
			this.prime = prime;
			head = prime * prime;
			tail = multiplyBy;
		}

		public void MoveToNext()
		{
			head = prime * tail.Number;
			tail = tail.Next();
		}

		public long Current { get { return head; } }

		private readonly long prime;
		private long head;
		private WheelElement tail;
	}
}