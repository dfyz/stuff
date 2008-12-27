using System;
using System.IO;
using System.Collections;

namespace XCalc
{

	//Перечисление типов лексем
	public enum LexemeType
	{
		end = 001,
		num = 002,
		add = 101,
		sub = 102,
		mul = 201,
		div = 202,
		bracketOpen = 003,
		bracketClose = 004
		
	}
	
	//Конец класса

	//Абстрактный класс "Выражение"
	public class Expression
	{
		public virtual decimal calculate()
		{
			return 0;
		}

		public virtual ICollection Members { get {return null;} }
	
		public static void Parse(Parser parser, int basePriority)
		{
			int context = 0;
			while ( true )
			{
				switch ( context )
				{
					case 0:
						context = 1;
						switch ( parser.Analyser.Peek().Type )
						{
							case LexemeType.num:
								Number.Parse(parser, 0);
								continue;
							case LexemeType.bracketOpen:
								CompoundExpression.Parse(parser, 0);
								continue;
							case LexemeType.bracketClose:
								return;
							case LexemeType.end:
								return;
							default:
								throw new Exception("На входе инвалидное выражение!");
						}
					case 1:
						bool lPriority = ( parser.Analyser.Peek().GetPriority() <= basePriority );
 						switch ( parser.Analyser.Peek().Type )
						{
							case LexemeType.add: case LexemeType.sub: case LexemeType.mul: case LexemeType.div:
								if ( lPriority ) return;
								BinaryOperation.Parse(parser, parser.Analyser.Peek().GetPriority());	
								continue;
							case LexemeType.bracketClose: case LexemeType.end:
								context = 0;
								return;
							default:
								throw new Exception("На входе невалидное выражение!");
						}
						break;
				}
			}
		}
	}
	
	//Конец класса

	//Класс "Число" 
	public class Number : Expression
	{
		private decimal _value;
		
		public decimal Value
		{
			get
			{
				return _value;
			}
		}

		public Number(decimal invalue)
		{
			_value = invalue;
		}
		
		public override ICollection Members
		{
			get
			{
				return new Expression [0] {};
			}
		}

		public override decimal calculate() 
		{
			return this.Value;
		}

		public static new void Parse(Parser parser,int basePriority)
		{
			if ( parser.Analyser.Peek().Type != LexemeType.num )
				throw new Exception("Ожидается число!");
 			Lexeme lex = parser.Analyser.Read();
			parser.ParseStack.Push(new Number((decimal)lex.Value));
		}
	}
	
	//Конец класса

	//Класс "Бинарная операция", работает с двумя числами или выражениями
	public class BinaryOperation : Expression
	{
		protected Expression _exp1;
		protected Expression _exp2;
		
		public Expression Exp1
		{
			get
			{
				return _exp1;
			}
			
		}
		public Expression Exp2
		{
			get
			{
				return _exp2;
			}
		}

		public override ICollection Members
		{
			get
			{
			return new Expression [2] {_exp1,_exp2};
			}
		}
		public BinaryOperation(Expression exp1, Expression exp2)
		{
			_exp1 = exp1;
			_exp2 = exp2;
		}

		public new static void Parse(Parser parser,int basePriority)
		{
			switch (parser.Analyser.Peek().Type)
			{
				case LexemeType.add: case LexemeType.sub: case LexemeType.mul: case LexemeType.div:
					break;
				default:
					throw new InvalidOperationException("Ожидается арифметическое действие!");
			}
			Lexeme lexeme = parser.Analyser.Peek();
			LexemeType operation = parser.Analyser.Read().Type;
			Expression.Parse(parser, lexeme.GetPriority());
			switch (operation)
			{
				case LexemeType.add:
					parser.ParseStack.Push(new Addition((Expression)parser.ParseStack.Pop(),(Expression)parser.ParseStack.Pop()));
					break;
				case LexemeType.sub:
					Expression op1 = (Expression)parser.ParseStack.Pop();
					parser.ParseStack.Push(new Substract((Expression)parser.ParseStack.Pop(),op1));
					break;
				case LexemeType.mul:
					parser.ParseStack.Push(new Multiplication((Expression)parser.ParseStack.Pop(),(Expression)parser.ParseStack.Pop()));
					break;
				case LexemeType.div:
					Expression op2 = (Expression)parser.ParseStack.Pop();
					parser.ParseStack.Push(new Division((Expression)parser.ParseStack.Pop(),op2));
					break;
			}
		}
	}
	
	//Конец класса

	//Классы "Математические действия" - сложение, вычитание, умножение, деление
	public class Addition : BinaryOperation
	{
		public Addition(Expression exp1,Expression exp2) : base(exp1, exp2)	{}

		public override decimal calculate()
		{
			return _exp1.calculate() + _exp2.calculate();			   
		}
	}
	
	public class Substract : BinaryOperation
	{
		public Substract(Expression exp1,Expression exp2) : base(exp1, exp2) {}

		public override decimal calculate()
		{
			return _exp1.calculate() - _exp2.calculate();			   
		}
	}
	
	public class Multiplication : BinaryOperation
	{
		public Multiplication(Expression exp1,Expression exp2) : base(exp1, exp2) {}

		public override decimal calculate()
		{
			return _exp1.calculate() * _exp2.calculate();			   
		}
	}
	
	public class Division : BinaryOperation
	{
		public Division(Expression exp1,Expression exp2) :  base(exp1, exp2)  {}

		public override decimal calculate()
		{
			return _exp1.calculate() / _exp2.calculate();			   
		}
	}

	//Конец класса

	//Класс "Составное выражение", нужен для реализации скобок
	public class CompoundExpression : Expression
	{
		private Expression _exp;

		public Expression Exp
		{
			get
			{
				return _exp;
			}
		}

		public CompoundExpression(Expression exp)
		{
			_exp = exp;
		}
		
		public override ICollection Members
		{
			get
			{
				return new Expression [1] {_exp};
			}
		}
		public override decimal calculate()
		{
			return _exp.calculate();
		}

		public static new void Parse(Parser parser,int basePriority)
		{
			if ( parser.Analyser.Peek().Type != LexemeType.bracketOpen )
				throw new Exception("Ожидается ( !");
			Lexeme lexeme = parser.Analyser.Peek();
			parser.Analyser.Read();
			Expression.Parse(parser, lexeme.GetPriority());
			if ( parser.Analyser.Peek().Type != LexemeType.bracketClose )
				throw new Exception("Ожидается ) !");
			parser.Analyser.Read();
			parser.ParseStack.Push(new CompoundExpression((Expression)parser.ParseStack.Pop()));
		}
	}

	//Конец класса

	//Класс "Лексема", в нем хранятся элементарные лексемы
	public class Lexeme
	{
		protected LexemeType _type;
		
		public int GetPriority()
		{
			return unchecked
				((int)_type/100);
		}
		public Lexeme() 
		{
		}

		public Lexeme(LexemeType type)
		{
			_type = type;
		}

		public virtual LexemeType Type 
		{
			get
			{ 
				return _type; 
			} 
		}

		public virtual object Value
		{
			get
			{
				throw new Exception("Invalid operation");
			}
		}
	}

	//Конец класса

	//Класс "Числовая лексема", в нем хранятся лексемы-числа
	public class NumberLexeme : Lexeme
	{
		protected decimal _val;

		public NumberLexeme(decimal val) : base(LexemeType.num)
		{
			_val = val;
		}

		public override object Value
		{
			get
			{
				return _val;
			}
		}
	}

	//Конец класса

	//Класс "Анализатор лексем", вычленяет элементарные лексемы из строки 
	public class LexicalAnalyser
	{
		private bool _readFlag; 
		private StringReader _input;
		private Lexeme _buffer = null;

		public LexicalAnalyser(string input)
		{
			_input = new StringReader(input);
		}	
		
		public Lexeme Peek()
		{
			if ( _buffer == null )
				_GetNext();
			return ( _buffer == null ? new Lexeme(LexemeType.end) : _buffer );
		}

		public Lexeme Read()
		{
			Lexeme prev = _buffer;
			_GetNext();
			return ( prev == null ? new Lexeme(LexemeType.end) : prev );
		}

		private void _GetNext()
		{
			Lexeme buffer = null;
			while (_input.Peek() != -1)  
			{
				_readFlag = true; 
				switch(_input.Peek())
				{
					case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9':
						buffer = new NumberLexeme(GetDecimal());
						_readFlag = false;
						break;
					case '+': 
						buffer = new Lexeme(LexemeType.add);
						break;
					case '-':
						buffer =  new Lexeme(LexemeType.sub);
						break;
					case '*': 
						buffer =  new Lexeme(LexemeType.mul);
						break;
					case '/':
						buffer =  new Lexeme(LexemeType.div);
						break;
					case ' ':
						break;
					case '(':
						buffer =  new Lexeme(LexemeType.bracketOpen);
						break;
					case ')':
						buffer =  new Lexeme(LexemeType.bracketClose);
						break;
					default:				
						throw new Exception("Недопустимый символ!!!");	
				}
				
				if ( _readFlag == true )
					_input.Read();

				if ( buffer == null )
					continue;

				_buffer = buffer;
				return;
			}
			_buffer = null;
			return;
		}

		private decimal GetDecimal()
		{
			string buffer = "";
			bool onePoint = false;
			while ( ",.0123456789".IndexOf((char)_input.Peek()) >= 0 )
			{
				if ( (char)_input.Peek() == ',' || (char)_input.Peek() == '.' )
				{
					if ( onePoint == true )
						throw new Exception ("Товарищ, вы напортачили с десятичной точкой!");
					onePoint = true;
					_input.Read();
					buffer += ',';
					continue;
				}
				buffer += (char)_input.Read(); 
			}
			return Convert.ToDecimal(buffer); 
		}
	}
	
	//Конец класса

	//Класс "Парсер", обрабатывает соотношения между полученными лексемами
	public class Parser
	{
		private LexicalAnalyser _analyser;
		private Stack _parseStack = new Stack();
		internal LexicalAnalyser Analyser
		{
			get
			{
				return _analyser;
			}
		}
		internal Stack ParseStack
		{
			get 
			{
				return _parseStack;
			}
		}
		
		public Parser(LexicalAnalyser analyser)
		{
			_analyser = analyser;
		}

		public Expression Parse()
		{
		Expression.Parse(this,0);
			if ( ((Lexeme)_analyser.Peek()).Type != LexemeType.end || _parseStack.Count != 1 )
				throw new Exception ("В стеке неверное выражение!");
			return (Expression)_parseStack.Pop();			
		}
	}

	//Конец класса

	//Класс с точкой входа в программу
	class Class1
	{
		static void Main(string[] args)
		{
		//	Parser p = new Parser(new LexicalAnalyser(args[0]));
		//	Expression e = p.Parse();
		//	Console.WriteLine();
		//	Console.WriteLine("{0} = {1}",args[0],e.calculate());
			//Console.ReadLine();
			Form1 f = new Form1();
			f.ShowDialog();
		}
	}
	//Конец класса
}
