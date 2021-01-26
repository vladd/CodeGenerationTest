using System;

using AutoNotify;

namespace TestApp
{
    class Program
    {
        static void Main(string[] args)
        {
            var t = new Test();
            t.PropertyChanged += (o, args) => Console.WriteLine($"Changed value of {args.PropertyName}");
            t.X = 10;
        }
    }

    partial class Test
    {
        [AutoNotify]
        int x;
    }
}
