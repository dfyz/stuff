using System;
using System.Collections.Generic;
using System.Linq;

namespace Hungarian
{
	public class AssignmentProblemSolver
	{
		public AssignmentProblemSolver(int[,] costMatrix)
		{
			originalMatrix = (int[,]) costMatrix.Clone();
			this.costMatrix = (int[,]) costMatrix.Clone();
			assignmentSize = Math.Min(this.costMatrix.GetLength(0), this.costMatrix.GetLength(1));
			PadIfNeeded();
			MakeZeroes();
		}

		public Assignment Solve()
		{
			int[] rowMatch = MakeEmptyMatchArray(n);
			int[] colMatch = MakeEmptyMatchArray(n);
			bool[] rowMarked = new bool[n];
			bool[] colMarked = new bool[n];
			int matchingSize = 0;
			while (matchingSize < n)
			{
				bool foundMatch = false;
				FillArray(rowMarked, false);
				FillArray(colMarked, false);
				foreach (int start in MatrixIndices)
				{
					if ((rowMatch[start] == -1) && TryIncreaseMatching(start, rowMarked, colMarked, rowMatch, colMatch))
					{
						foundMatch = true;
						matchingSize++;
						break;
					}
				}
				if (!foundMatch)
				{
					Transform(rowMarked, colMarked);
				}
			}
			var assignmentElements = rowMatch.Take(assignmentSize).Select((column, row) => new AssignmentElement(row, column));
			var assignmentWeight = assignmentElements.Sum(e => originalMatrix[e.Row, e.AssignedColumn]);
			return new Assignment(assignmentWeight, assignmentElements);
		}

		private bool TryIncreaseMatching(int start, bool[] rowMarked, bool[] colMarked, int[] rowMatch, int[] colMatch)
		{
			rowMarked[start] = true;
			foreach (int next in MatrixIndices)
			{
				if (costMatrix[start, next] != 0) continue;
				if (colMarked[next]) continue;
				colMarked[next] = true;
				if ((colMatch[next] == -1) || (TryIncreaseMatching(colMatch[next], rowMarked, colMarked, rowMatch, colMatch)))
				{
					rowMatch[start] = next;
					colMatch[next] = start;
					return true;
				}
			}
			return false;
		}

		private void Transform(bool[] rowMarked, bool[] colMarked)
		{
			int d = (from row in MatrixIndices
					 from col in MatrixIndices
					 where rowMarked[row] && !colMarked[col]
					 select costMatrix[row, col]).Min();
			foreach (int index in MatrixIndices)
			{
				if (rowMarked[index])
				{
					ChangeRowBy(index, -d);
				}
				if (colMarked[index])
				{
					ChangeColBy(index, d);
				}
			}
		}

		private IEnumerable<int> MatrixIndices
		{
			get { return Enumerable.Range(0, n); }
		}

		private int[] MakeEmptyMatchArray(int size)
		{
			var result = new int[size];
			FillArray(result, -1);
			return result;
		}

		private void FillArray<T>(T[] array, T value)
		{
			for (int i = 0; i < array.Length; i++)
			{
				array[i] = value;
			}
		}

		private void MakeZeroes()
		{
			foreach (int row in MatrixIndices)
			{
				int z = row;
				int minRowValue = MatrixIndices.Select(col => costMatrix[z, col]).Min();
				ChangeRowBy(z, -minRowValue);
			}
			foreach (int col in MatrixIndices)
			{
				int z = col;
				int minColValue = MatrixIndices.Select(row => costMatrix[row, z]).Min();
				ChangeColBy(z, -minColValue);
			}
		}

		private void ChangeRowBy(int row, int value)
		{
			foreach (int i in MatrixIndices)
			{
				costMatrix[row, i] += value;
			}
		}

		private void ChangeColBy(int col, int value)
		{
			foreach (int i in MatrixIndices)
			{
				costMatrix[i, col] += value;
			}
		}

		private void PadIfNeeded()
		{
			n = Math.Max(costMatrix.GetLength(0), costMatrix.GetLength(1));
			if (costMatrix.GetLength(0) == costMatrix.GetLength(1)) return;
			var paddedMatrix = new int[n, n];
			foreach (int row in MatrixIndices)
			{
				foreach (int col in MatrixIndices)
				{
					paddedMatrix[row, col] = (row < costMatrix.GetLength(0) && col < costMatrix.GetLength(1)) ? costMatrix[row, col] : 0;
				}
			}
			costMatrix = paddedMatrix;
		}

		private int[,] costMatrix;
		private int[,] originalMatrix;
		private int n;
		private readonly int assignmentSize;
	}
}