open System

let fileName = "test.txt"

let path = System.IO.Path.Combine(__SOURCE_DIRECTORY__, fileName)

let input = System.IO.File.ReadAllLines(path)

input
|> Array.map int
|> Array.reduce (fun a b -> a + b)
|> Console.WriteLine