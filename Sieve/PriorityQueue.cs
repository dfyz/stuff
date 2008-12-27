using System;
using System.Collections.Generic;

namespace Sieve
{
	public class PriorityQueue<T> where T : IComparable<T>
	{
		public T Min()
		{
			EnsureNonEmpty();
			return storage[0];
		}

		public bool IsEmpty()
		{
			return storage.Count == 0;
		}

		public void Insert(T item)
		{
			storage.Add(item);
			SiftUp(storage.Count - 1);
		}

		public void DeleteMin()
		{
			EnsureNonEmpty();
			Swap(storage.Count - 1, 0);
			storage.RemoveAt(storage.Count - 1);
			SiftDown(0);
		}

		private void SiftDown(int itemIdx)
		{
			while (LeftChild(itemIdx) < storage.Count)
			{
				int minChildIdx = MimimumChild(itemIdx);
				if (HeapPropertySatisfied(itemIdx, minChildIdx))
				{
					break;
				}
				Swap(itemIdx, minChildIdx);
				itemIdx = minChildIdx;
			}
		}

		private void SiftUp(int itemIdx)
		{
			while (itemIdx > 0)
			{
				if (HeapPropertySatisfied(Parent(itemIdx), itemIdx))
				{
					break;
				}
				Swap(itemIdx, Parent(itemIdx));
				itemIdx = Parent(itemIdx);
			}
		}

		private bool HeapPropertySatisfied(int a, int b)
		{
			return storage[a].CompareTo(storage[b]) <= 0;
		}

		private void EnsureNonEmpty()
		{
			if (IsEmpty())
			{
				throw new InvalidOperationException("Heap is empty");
			}
		}

		private int Parent(int n)
		{
			return (n - 1) / 2;
		}

		private int LeftChild(int n)
		{
			return n * 2 + 1;
		}

		private int RightChild(int n)
		{
			return n * 2 + 2;
		}

		private int MimimumChild(int n)
		{
			if ((RightChild(n) < storage.Count) && (storage[RightChild(n)].CompareTo(storage[LeftChild(n)]) < 0))
			{
				return RightChild(n);
			}
			return LeftChild(n);
		}

		private void Swap(int a, int b)
		{
			T temp = storage[a];
			storage[a] = storage[b];
			storage[b] = temp;
		}
		
		private readonly List<T> storage = new List<T>();
	}
}