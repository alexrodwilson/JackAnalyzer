module Tokenizer 
  type Token = 
    | Keyword of string
    | Symbol of char
    | Identifier of string
    | IntConstant of int
    | StringConstant of string

  let keywords = set ["class"; "constructor"; "function"; "method"; "field"; "static"; "var"; "int"; "char"; "boolean"; "void"; "true"; "false"; "null"; "this"; "let"; "do"; "if"; "else"; "while"; "return"]
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
    let rec helper (code : string) (tokens: list<Token>) = 
      match code with 
        | "" -> tokens
        | _ when (code.Chars 0) = '/' && (code.Chars 1) = '/' -> (helper (advanceUntilChar '\n' code) tokens)
        | _ when (code.Chars 0) = '/' && (code.Chars 1) = '*' -> (helper (advanceUntilWord "*/" code) tokens)
        | _ when (System.Char.IsNumber (code.Chars 0)) -> 
          let number = (getNextNumber code)
          
          helper (code.Substring number.Length) ((IntConstant (int number)) :: tokens)
        | _ when (code.Chars 0) = '"' ->
          let text = getNextString (code.Substring 1)
          helper (advanceUntilChar '"' (code.Substring 1)) ((StringConstant text) :: tokens)
        | _ when (isSymbol (code.Chars 0)) ->
          let symbol = Symbol (code.Chars 0)
          (helper (code.Substring 1) (symbol :: tokens))
        | _ when (code.Chars 0) = '_' || (System.Char.IsLetter (code.Chars 0)) -> 
          let word = getNextKeywordOrIdentifier(code)
          let keywordOrIdentifier = if (keywords.Contains word) then (Keyword word)
                                    else Identifier word
          helper (code.Substring word.Length) (keywordOrIdentifier :: tokens)
        | _ -> (helper (code.Substring 1) tokens)
    let res = (helper s [])
    List.rev res
  
  let tokenToXml token = 
    match token with
      | Keyword w -> "<keyword> " + w + " </keyword>"
      | Symbol ch -> 
        let chString =
          match ch with 
            | '<' -> "&lt;"
            | '>' -> "&gt;"
            | '"' -> "&quot;"
            | '&' -> "&amp;"
            | ch -> string ch
        "<symbol> " + chString + " </symbol>"
      | Identifier w -> "<identifier> " + w + " </identifier>"
      | IntConstant i -> "<integerConstant> " + (string i) + " </integerConstant>"
      | StringConstant s -> "<stringConstant> " + s + " </stringConstant>"


  let s = """ if (x < 0) {
           //handles the sign
    let sign = "negative"; }"""

//  printfn "%s" (getNextKeywordOrIdentifier "doggo_doggo= 45") 
//  printfn "%s" (getNextKeywordOrIdentifier """_knife23{}""") 
//  printfn "%s" (advanceUntilChar 'x' s) 
//  printfn "%s" (advanceUntilChar '/' s) 
//  printfn "%s" (advanceUntilWord "sign" s) 
//  printfn "%s" (advanceUntilWord "handles" s) 
//  let xs = (tokenize s)
//  List.iter (fun x -> (printfn "%A" x)) xs
