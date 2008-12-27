using System;
using System.Collections.Generic;
using System.Linq;
using NUnit.Framework;

namespace Hungarian
{
	[TestFixture]
	public class AssignmentProblemSolver_Test
	{
		[Test]
		[TestCaseSource("SmallMatrices")]
		public void TestSmallMatrices(int[,] costMatrix)
		{
			TestMatrix(costMatrix);
		}

		[Test]
		[Repeat(10000)]
		public void TestRandomMatrices()
		{
			int n = rnd.Next(1, 10);
			var randomMatrix = new int[n, n];
			for (int i = 0; i < n; i++)
			{
				for (int j = 0; j < n; j++)
				{
					randomMatrix[i, j] = rnd.Next(0, 1000000);
				}
			}
			TestMatrix(randomMatrix);
		}

		[Test]
		[TestCaseSource("NonSquareMatrices")]
		public void TestNonSquareMatrices(int[,] costMatrix)
		{
			TestMatrix(costMatrix);
		}

		[SetUp]
		public void SetUp()
		{
			rnd = new Random();
		}

		private IEnumerable<int[,]> SmallMatrices()
		{
			return new[] {
				new[,]
				{
					{ 1, 4, 4, 3 },
					{ 2, 7, 6, 8 },
					{ 4, 7, 5, 6 },
					{ 2, 5, 1, 1 }
				},
				new[,]
				{
					{ 1, 2, 3 },
					{ 2, 4, 6 },
					{ 3, 6, 9 },
				},
				new[,]
				{
					{ 1, 4, 5 },
					{ 5, 7, 6 },
					{ 5, 8, 8 },
				}
			};
		}

		private IEnumerable<int[,]> NonSquareMatrices()
		{
			return new[] {
				new[,]
				{
					{ 1, 2, 3 },
					{ 4, 5, 6 }
				},
				new[,]
				{
					{ 1366, 566, 5999, 100 },
					{ 499, 100, 199, 105 }
				},
				new[,]
				{
					{ 3 },
					{ 4 },
					{ 5 },
					{ 6 }
				}
			};
		}

		private void TestMatrix(int[,] costMatrix)
		{
			var assignment = new AssignmentProblemSolver(costMatrix).Solve();
			int expectedWeight = Bruteforce(costMatrix);
			Assert.AreEqual(expectedWeight, assignment.Weight);
			Assert.AreEqual(Math.Min(costMatrix.GetLength(0), costMatrix.GetLength(1)), assignment.Elements.Count());
			var usedRows = new bool[costMatrix.GetLength(0)];
			var usedCols = new bool[costMatrix.GetLength(1)];
			foreach (var element in assignment.Elements)
			{
				Assert.False(usedRows[element.Row]);
				usedRows[element.Row] = true;
				Assert.False(usedCols[element.AssignedColumn]);
				usedCols[element.AssignedColumn] = true;
			}
		}

		private int Bruteforce(int[,] costMatrix)
		{
			return Rec(costMatrix, 0, new bool[costMatrix.GetLength(1)], 0, costMatrix.GetLength(0), costMatrix.GetLength(1));
		}

		private int Rec(int[,] costMatrix, int row, bool[] usedCols, int cost, int rows, int cols)
		{
			if (row == Math.Min(rows, cols))
			{
				return cost;
			}
			int res = int.MaxValue;
			for (int col = 0; col < cols; col++)
			{
				if (usedCols[col]) continue;
				usedCols[col] = true;
				res = Math.Min(res, Rec(costMatrix, row + 1, usedCols, cost + costMatrix[row, col], rows, cols));
				usedCols[col] = false;
			}
			return res;
		}

		private Random rnd;
	}
}