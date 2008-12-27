namespace Hungarian
{
	public class AssignmentElement
	{
		public readonly int Row;
		public readonly int AssignedColumn;

		public AssignmentElement(int row, int assignedColumn)
		{
			Row = row;
			AssignedColumn = assignedColumn;
		}
	}
}