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
            Console.WriteLine("Parse Completed: {0}", myActions.StructString.Pop());
            Console.ReadLine();
        }
    }
    class MyActions : vSprogParser.Actions
    {
        public Stack<string> StatementString = new Stack<string>();
        private Stack<string> StructString = new Stack<string>();
        private Stack<string> TypeDeclsString = new Stack<string>();
        private Stack<string> TypeDeclString = new Stack<string>();
        private Stack<string> SimpleTypeString = new Stack<string>();
        public override void StructProduction(Symbol head, SemanticBody body)
        {
            string tmpString = string.Format("{0} {1} {2} {3} {4} {5} {6}",
                               body[0].Value, body[1].Value, body[2].Value, body[3].Value, body[4].Value, TypeDeclsString.Pop(), body[6].Value);
            StructString.Push(tmpString);
            Console.WriteLine(tmpString);
        }

        public override void TypeDecls(Symbol head, SemanticBody body)
        {
            string tmpString = TypeDeclString.Pop() + " " + body[1].Value + " " + TypeDeclString.Pop();
            TypeDeclsString.Push(tmpString);
            Console.WriteLine(tmpString);
        }
        public override void TypeDecl(Symbol head, SemanticBody body)
        {
            string tmpString = body[0].Value + body[1].Value + SimpleTypeString.Pop();
            TypeDeclString.Push(tmpString);
            Console.WriteLine(tmpString);
        }
        public override void SimpleType(Symbol head, SemanticBody body)
        {
            string tmpString = body[0].Value;
            SimpleTypeString.Push(tmpString);
            Console.WriteLine(tmpString);
        }
        public void PrettyPrint(Symbol head, SemanticBody body)
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
