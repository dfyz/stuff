using System;
using System.Collections.Generic;
using NUnit.Framework;

namespace Sieve
{
	[TestFixture]
	public class PriorityQueue_Test
	{
		[Test]
		public void TestInsertion()
		{
			var pq = new PriorityQueue<int>();
			pq.Insert(8585);
			pq.Insert(34);
			pq.Insert(4);
			pq.Insert(35);
			pq.Insert(34);
			Assert.AreEqual(4, pq.Min());
		}

		[Test]
		public void TestDeletion()
		{
			var pq = new PriorityQueue<int>();
			pq.Insert(477);
			pq.Insert(8);
			pq.Insert(154);
			Assert.AreEqual(8, pq.Min());
			pq.DeleteMin();
			Assert.AreEqual(154, pq.Min());
			pq.DeleteMin();
			Assert.AreEqual(477, pq.Min());
			pq.DeleteMin();
			Assert.IsTrue(pq.IsEmpty());
		}

		[Test]
		public void TestInsertionAfterDeletion()
		{
			var pq = new PriorityQueue<int>();
			pq.Insert(432);
			pq.Insert(22);
			pq.Insert(55);
			pq.Insert(477);
			pq.DeleteMin();
			pq.Insert(23);
			Assert.AreEqual(23, pq.Min());
		}

		[Test]
		public void TestRandomNumbers()
		{
			const int Iterations = 10000;
			var rnd = new Random();
			var dumbPq = new List<int>();
			var pq = new PriorityQueue<int>();
			for (int i = 0; i < Iterations; i++)
			{
				if (rnd.Next(2) == 1 || dumbPq.Count == 0)
				{
					int n = rnd.Next();
					dumbPq.Add(n);
					dumbPq.Sort();
					pq.Insert(n);
				}
				else
				{
					dumbPq.RemoveAt(0);
					pq.DeleteMin();
				}
				if (dumbPq.Count == 0)
				{
					Assert.IsTrue(pq.IsEmpty(), "iteration {0}: expected empty queue", i);
				}
				else
				{
					Assert.AreEqual(dumbPq[0], pq.Min(), "iteration {0}", i);
				}
			}
		}

		[Test]
		public void TestCustomIComparable()
		{
			var pq = new PriorityQueue<CustomIComparable>();
			pq.Insert(new CustomIComparable { Str = "fsdajklfjkl" });
			pq.Insert(new CustomIComparable { Str = "dajk" });
			pq.Insert(new CustomIComparable { Str = "d" });
			pq.Insert(new CustomIComparable { Str = "aaaaaaaaaa" });
			Assert.AreEqual("d", pq.Min().Str);
		}

		class CustomIComparable : IComparable<CustomIComparable>
		{
			public int CompareTo(CustomIComparable other)
			{
				return Str.Length.CompareTo(other.Str.Length);
			}

			public string Str { get; set; }
		}
	}
}