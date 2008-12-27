using System.Collections.Generic;

namespace Hungarian
{
	public class Assignment
	{
		public readonly int Weight;
		public readonly IEnumerable<AssignmentElement> Elements;

		public Assignment(int weight, IEnumerable<AssignmentElement> elements)
		{
			Weight = weight;
			Elements = elements;
		}
	}
}