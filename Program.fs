// For more information see https://aka.ms/fsharp-console-apps
open Tokenizer
open System.IO
open CompilationEngine

let tokenizeAndXmlizeFile (fn: string) = 
  let directoryName = Path.GetDirectoryName(fn)
  let fileNameWithoutExtension = Path.GetFileNameWithoutExtension(fn)
  let tokenOutFn = Path.Combine(directoryName, (fileNameWithoutExtension + "T" + ".xml"))
  let vmOutFn = Path.ChangeExtension (fn, ".vm")
  let text = File.ReadAllText fn
  let tokens = tokenize text
//  printfn "%A" ("Starting to write " + xmlOutFn + "...")
  let vm = CompilationEngine.CompileClass tokens
 // File.WriteAllLines(tokenOutFn, "<tokens>" :: (List.map tokenToXml  tokens) @ ["</tokens>"])
 // printfn "%s written" tokenOutFn
  File.WriteAllText (vmOutFn, vm)
  printfn "%s written" vmOutFn

[<EntryPoint>]
let main args =
    //printfn "Arguments passed to function : %A" args
    // Return 0. This indicates success.
  let fn = args[0]
  match fn with
    | fn when Directory.Exists fn -> 
      let jackFiles = Directory.GetFiles(fn, "*.jack")
      for f in jackFiles do 
        tokenizeAndXmlizeFile f
    | fn when (File.Exists fn) && Path.GetExtension(fn) = ".jack" ->
       tokenizeAndXmlizeFile fn
    | fn -> printfn "%s is not a valid file/directory" fn
  0
