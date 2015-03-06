using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Hime.Redist;

namespace vSprog
{
    class Program
    {
        static void Main(string[] args)
        {
            System.IO.StreamReader reader = new System.IO.StreamReader("C:/Users/Jens/Documents/GitHub/P4/Compiler/vSprog/vSprog/Parser/code.txt");

            vSprogLexer lexer = new vSprogLexer(reader);
            Console.WriteLine("String lexed.. Parsing");
            MyActions myActions = new MyActions(); 
            vSprogParser parser = new vSprogParser(lexer, myActions);
            ParseResult result = parser.Parse();
            foreach (var error in result.Errors)
            {
                Console.WriteLine(error.Message);
            }
            Console.WriteLine("Parse Completed: {0}", result.Root.Children);
            Console.ReadLine();
        }
    }
    class MyActions : vSprogParser.Actions
    {
        public override void PrettyPrint(Symbol head, SemanticBody body)
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
