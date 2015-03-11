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
            System.IO.StreamReader reader = new System.IO.StreamReader("../../Parser/code.txt");

            vSprogLexer lexer = new vSprogLexer(reader);
            Console.WriteLine("String lexed.. Parsing");
            MyActions myActions = new MyActions(); 
            vSprogParser parser = new vSprogParser(lexer, myActions);
            ParseResult result = parser.Parse();
            foreach (var error in result.Errors)
            {
                Console.WriteLine(error.Message);
            }
            //Console.WriteLine("Parse Completed: {0}", myActions.StructString.Pop());
            Console.ReadLine();
        }
    }
    class MyActions : vSprogParser.Actions
    {
        public Queue<string> StatementString = new Queue<string>();
        public Queue<string> DeclarationString = new Queue<string>();
        private Queue<string> StructString = new Queue<string>();
        private Queue<string> TypeDeclsString = new Queue<string>();
        private Queue<string> TypeDeclString = new Queue<string>();
        private Queue<string> SimpleTypeString = new Queue<string>();
        public override void EndStatementList(Symbol head, SemanticBody body)
        {
            string tmpString = StatementString.Dequeue() + ";";
            StatementString.Enqueue(tmpString);
            Console.WriteLine(tmpString);
        }
        public override void DeclarationStatement(Symbol head, SemanticBody body)
        {
            StatementString.Enqueue(DeclarationString.Dequeue());
        }
        public override void StructDeclaration(Symbol head, SemanticBody body)
        {
            DeclarationString.Enqueue(StructString.Dequeue());
        }
        public override void StructProduction(Symbol head, SemanticBody body)
        {
            string tmpString = string.Format("{0} {1} {2} {3} {4} {5} {6}",
                               body[0].Value, body[1].Value, body[2].Value, body[3].Value, body[4].Value, TypeDeclsString.Dequeue(), body[6].Value);
            StructString.Enqueue(tmpString);
            Console.WriteLine(tmpString);
        }

        public override void TypeDecls(Symbol head, SemanticBody body)
        {
            string tmpString = TypeDeclString.Dequeue() + " " + body[1].Value + " " + TypeDeclString.Dequeue();
            TypeDeclsString.Enqueue(tmpString);
            Console.WriteLine(tmpString);
        }
        public override void TypeDecl(Symbol head, SemanticBody body)
        {
            string tmpString = body[0].Value + body[1].Value + SimpleTypeString.Dequeue();
            TypeDeclString.Enqueue(tmpString);
            Console.WriteLine(tmpString);
        }
        public override void SimpleType(Symbol head, SemanticBody body)
        {
            string tmpString = body[0].Value;
            SimpleTypeString.Enqueue(tmpString);
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
