open System
let (|Prefix|_|) (p:string) (s:string) =
    if s.StartsWith(p) then
        Some(s.Substring(p.Length))
    else
        None
let s = """ if (x < 0) {
         //handles the sign
  let sign = "negative";
}"""

let isControlChar ch = 
  (ch = '\n') || (ch = '\t') || (ch = '\r') || (ch = ' ')

let isSymbol ch = 
  let symbols = set ['{'; '}'; '('; ')'; '['; ']'; '.'; ','; ';';'+'; '-'; '*'; '/'; '&'; '|'; '<'; '>'; '='; '~']
  (symbols.Contains ch)
  
let getNext f (s : string) =
  let mutable i = 0
  while (f (s.Chars i)) do 
    i <- i + 1
  s.Substring(0, i)

let getNextKeywordOrIdentifier s = (getNext (fun ch -> (System.Char.IsLetterOrDigit ch) || ch = '_') s)
let getNextNumber s = (getNext System.Char.IsDigit s)
let getNextString s = (getNext (fun ch -> ch <> '"') s)

let rec advanceUntilChar (ch : char) (str : string) = 
  match str with 
    | str when str.Chars(0) = ch ->  (str.Substring 1)
    | _ -> (advanceUntilChar ch (str.Substring 1))

let rec advanceUntilWord (target : string) (str : string) = 
  match str with 
    | str when (str.StartsWith target)  ->  (str.Substring target.Length)
    | _ -> (advanceUntilWord target (str.Substring 1))

let tokenize(s : string) =
  let rec helper (code : string) (tokenStrings : list<String>) = 
    match code with 
      | "" -> tokenStrings
      | _ when (code.Chars 0) = '/' && (code.Chars 1) = '/' -> (helper (advanceUntilChar '\n' code) tokenStrings)
      | _ when (code.Chars 0) = '/' && (code.Chars 1) = '*' -> (helper (advanceUntilWord "*/" code) tokenStrings)
      | _ when (System.Char.IsNumber (code.Chars 0)) -> 
        let number = (getNextNumber code)
        helper (code.Substring number.Length) (number :: tokenStrings)
      | _ when (code.Chars 0) = '"' ->
        let text = getNextString (code.Substring 1)
        helper (advanceUntilChar '"' (code.Substring 1)) (text :: tokenStrings)
      | _ when (isSymbol (code.Chars 0)) -> (helper (code.Substring 1) ((string (code.Chars 0)) :: tokenStrings))
      | _ when (code.Chars 0) = '_' || (System.Char.IsLetter (code.Chars 0)) -> 
        let word = getNextKeywordOrIdentifier(code)
        helper (code.Substring word.Length) (word :: tokenStrings)
      | _ -> (helper (code.Substring 1) tokenStrings)
  let res = (helper s [])
  List.rev res
    
printfn "%s" (getNextKeywordOrIdentifier "doggo_doggo= 45") 
printfn "%s" (getNextKeywordOrIdentifier """_knife23{}""") 
printfn "%s" (advanceUntilChar 'x' s) 
printfn "%s" (advanceUntilChar '/' s) 
printfn "%s" (advanceUntilWord "sign" s) 
printfn "%s" (advanceUntilWord "handles" s) 
let xs = (tokenize s)
List.iter (fun x -> (printfn "%s" x)) xs
