using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Hime.Redist;
using Hime.CentralDogma;

namespace vSprog
{
    class Program
    {
        static void Main(string[] args)
        {
			CompilationTask task = new CompilationTask ();
			task.AddInputFile ("../../Parser/vSprogGrammar.gram");
			task.Mode = Hime.CentralDogma.Output.Mode.Assembly;
			task.Execute ();
			Hime.CentralDogma.SDK.AssemblyReflection assembly = new Hime.CentralDogma.SDK.AssemblyReflection("vSprogGrammar.dll");

            //System.IO.StreamReader reader = new System.IO.StreamReader("../../Parser/code.txt");

            const string input = "a:=1";
				
			var dict = new Dictionary<string, SemanticAction> ();
			dict ["SimpleType"] = (x,y) => {};
			dict ["OnProgram"] = (x,y) => {};
            dict["OnNumberLiteral"] = VisitNumberLiteral;
            

			Hime.Redist.Parsers.IParser parser = null;
			try {
				parser = assembly.GetParser<String> (input,dict);
			} catch (Exception) {
				throw new Exception ("Not all semantic actions declared in grammar was assigned in code. Check dictionary containing semantic actions.");
			}

            ParseResult result = parser.Parse();

            foreach (var error in result.Errors)
            {
                Console.WriteLine(error.Message);
            }
            if (result.IsSuccess)
                PrintTree(result.Root, 0);
            else
                Console.WriteLine("Error encountered");
            Console.ReadLine();
        }

        private static void VisitNumberLiteral(Symbol head, SemanticBody body)
        {
            Console.WriteLine(body[2].Value);
            int haaaax;
            if (IsLiteral(body[2].Value, out haaaax)) { Console.WriteLine("den er satme fin"); }
            else { Console.WriteLine("det r fnme ente en literal"); }

        }

        private static bool IsLiteral(string inputString, out int IntEtEllerAndet)
        {
            return int.TryParse(inputString, out IntEtEllerAndet);
        }

		public static void PrintTree(ASTNode node, int level) {
			// print level
			for (int i = 0; i < level; i++) {
				Console.Write ("--");
			}
			if (level != 0) {
				Console.Write (' ');
			}
				
			Console.WriteLine (node.Symbol.Value);
			foreach (var child in node.Children) {
				PrintTree (child, level + 1);
			}
		}

		public static void PrettyPrint(Symbol head, SemanticBody body)
		{
			Console.Write(head.Name + " -> ");
			for (int i = 0; i < body.Length; i++)
			{
				Console.Write(body[i].Value + " ");
			}
			Console.WriteLine();
		}
    }
}
