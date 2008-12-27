using System;
using System.Drawing;
using System.Collections;
using System.ComponentModel;
using System.Windows.Forms;

namespace XCalc
{
	/// <summary>
	/// Summary description for Form1.
	/// </summary>
	public class Form1 : System.Windows.Forms.Form
	{
		private Expression _e;
		private System.Windows.Forms.TextBox ParseInput;
		private System.Windows.Forms.Button ParseButton;
		private System.Windows.Forms.TreeView expTree;
		/// <summary>
		/// Required designer variable.
		/// </summary>
		private System.ComponentModel.Container components = null;

		public Form1()
		{
			//
			// Required for Windows Form Designer support
			//
			InitializeComponent();
			//
			// TODO: Add any constructor code after InitializeComponent call
			//
		}

		/// <summary>
		/// Clean up any resources being used.
		/// </summary>
		protected override void Dispose( bool disposing )
		{
			if( disposing )
			{
				if(components != null)
				{
					components.Dispose();
				}
			}
			base.Dispose( disposing );
		}

		#region Windows Form Designer generated code
		/// <summary>
		/// Required method for Designer support - do not modify
		/// the contents of this method with the code editor.
		/// </summary>
		private void InitializeComponent()
		{
			this.expTree = new System.Windows.Forms.TreeView();
			this.ParseInput = new System.Windows.Forms.TextBox();
			this.ParseButton = new System.Windows.Forms.Button();
			this.SuspendLayout();
			// 
			// expTree
			// 
			this.expTree.ImageIndex = -1;
			this.expTree.Name = "expTree";
			this.expTree.SelectedImageIndex = -1;
			this.expTree.Size = new System.Drawing.Size(296, 264);
			this.expTree.TabIndex = 0;
			// 
			// ParseInput
			// 
			this.ParseInput.Location = new System.Drawing.Point(16, 280);
			this.ParseInput.Name = "ParseInput";
			this.ParseInput.Size = new System.Drawing.Size(264, 20);
			this.ParseInput.TabIndex = 1;
			this.ParseInput.Text = "";
			// 
			// ParseButton
			// 
			this.ParseButton.Location = new System.Drawing.Point(56, 320);
			this.ParseButton.Name = "ParseButton";
			this.ParseButton.Size = new System.Drawing.Size(168, 32);
			this.ParseButton.TabIndex = 2;
			this.ParseButton.Text = "Пропарсить ВСЕ!!!";
			this.ParseButton.Click += new System.EventHandler(this.ParseButton_Click);
			// 
			// Form1
			// 
			this.AutoScaleBaseSize = new System.Drawing.Size(5, 13);
			this.ClientSize = new System.Drawing.Size(304, 358);
			this.Controls.AddRange(new System.Windows.Forms.Control[] {
																		  this.ParseButton,
																		  this.ParseInput,
																		  this.expTree});
			this.Name = "Form1";
			this.Text = "Form1";
			this.Load += new System.EventHandler(this.Form1_Load);
			this.ResumeLayout(false);

		}
		#endregion

		public string FormName (Expression e)
		{
			if ((e as CompoundExpression) != null)
				return "Составное выражение " + e.calculate();
			if ((e as BinaryOperation) != null)
			{
				if ((((BinaryOperation)e) as Addition) != null)
					return "Сложение " + e.calculate();
				if ((((BinaryOperation)e) as Substract) != null)
					return "Вычитание " + e.calculate();
				if ((((BinaryOperation)e) as Multiplication) != null)
					return "Умножение " + e.calculate();
				if ((((BinaryOperation)e) as Division) != null)
					return "Деление " + e.calculate();
			}
			if ((e as Number) != null)
			{
				return "Число " + ((Number)e).calculate();
			}
			else 
			return "Недопустимое значение!";
		}
		public void ParseTree ( TreeNodeCollection n , Expression ex )
		{
			foreach (Expression e in ex.Members)
			{	
				TreeNode t = n.Add(FormName(e));
				ParseTree ( t.Nodes, e );	
			}
		}

		private void ParseButton_Click(object sender, System.EventArgs e)
		{
			Parser P = new Parser(new LexicalAnalyser(ParseInput.Text));
			Expression E = P.Parse();
			expTree.BeginUpdate();
			expTree.Nodes.Clear();
			TreeNode tr = expTree.Nodes.Add(FormName(E));
			ParseTree(tr.Nodes, E);
			expTree.EndUpdate();
		}

		private void Form1_Load(object sender, System.EventArgs e)
		{
			// ЗДЕСЬ ВАДИМУ НЕПОНЯТНО!!!!
			
			expTree.Nodes.Clear();
		
			//	Console.WriteLine();
			//	Console.WriteLine("{0} = {1}",args[0],e.calculate());
			//Console.ReadLine();
			/* TreeNode t = expTree.Nodes.Add("Родительский КлассЪ");
			TreeNode t1 = t.Nodes.Add("Дочерний КлассЪ");*/
			
			
		}

	}
}
