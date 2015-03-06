using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace vSprog
{
    class Program
    {
        static void Main(string[] args)
        {
            vSprogLexer lexer = new vSprogLexer("let a(x): int -> int = { x * x };");
            Console.WriteLine("String lexed.. Parsing");
            vSprogParser parser = new vSprogParser(lexer);
            Console.WriteLine("Parse Completed");
        }
    }
}
