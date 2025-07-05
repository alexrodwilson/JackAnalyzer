// For more information see https://aka.ms/fsharp-console-apps
open Tokenizer
open System.IO

let tokenizeFile fn = 
  let outFn = Path.ChangeExtension(fn, ".xml")
  let text = File.ReadAllText fn
  let tokens = tokenize text
  File.WriteAllText(outFn, text)

[<EntryPoint>]
let main args =
    //printfn "Arguments passed to function : %A" args
    // Return 0. This indicates success.
  let fn = args[0]
  match fn with
    | fn when Directory.Exists fn -> 
      let jackFiles = Directory.GetFiles(fn, "*.jack")
      for f in jackFiles do 
        tokenizeFile f
        printfn "%s written" (Path.ChangeExtension (f, ".xml"))
    | fn when (File.Exists fn) && Path.GetExtension(fn) = ".jack" ->
      tokenizeFile fn
      printfn "%s written" (Path.ChangeExtension(fn, ".xml"))
    | fn -> printfn "%s is not a valid file/directory" fn
  0
