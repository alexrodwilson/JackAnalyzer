#load "Tokenizer.fs"
open Tokenizer
let deconstruct token = 
  match token with 
  | Identifier i -> i
  | Keyword k -> k
  | Symbol s -> (string s)
  | IntConstant i -> (string i)
  | StringConstant s -> s

let advanceUntil test tokens returnLastToken = 
  let rec aux toReturn remainingTokens =
    match remainingTokens with
    | head::tail when test head -> if returnLastToken then
                                    List.rev (head::toReturn), tail
                                   else 
                                    (List.rev toReturn), remainingTokens
    | head::tail -> aux (head::toReturn) tail
  aux [] tokens


let getConsecutivePatterns startTest endTest tokens =
  let rec aux patterns remainingTokens = 
    match remainingTokens with
    | [] -> List.rev patterns, remainingTokens
    | head::tail when not (startTest head) -> (List.rev patterns), remainingTokens
    | head::tail ->
      let pattern, remainingTokens = advanceUntil endTest remainingTokens true
      aux (pattern::patterns) remainingTokens
  aux [] tokens

let eatIf (test: Token list -> bool) (tokens: Token list) =
  match tokens with
  | head::tail when (test tokens) -> tail
  | _ -> failwith "Bad Argument" 

let getNextTokenIf test tokens = 
  match tokens with
  | head::tail when (test tokens) -> head, tail
  | head::tail -> failwith "Bad Argument"

let isSameToken (token: Token) = 
  (fun (xs: Token list) -> xs.Head = token)

let typeToString token = 
  match token with
  | Identifier _ -> "identifier"
  | Keyword _ -> "keyword"
  | Symbol _ -> "symbol"
  | IntConstant _ -> "intConstant"
  | StringConstant _ -> "stringConstant"

let isSameType token = 
  fun (xs: Token list) -> (typeToString xs.Head) = (typeToString token)

let isOneOfTokens (tokenList: Token list) = 
  fun (xs: Token list) -> List.contains xs.Head tokenList

let isTypeProgramStructure xs = 
  (isSameType (Identifier "_") xs) || 
  (isSameToken (Keyword "int") xs) ||
  (isSameToken (Keyword "char") xs)||
  (isSameToken (Keyword "boolean") xs)

let CompileClassVarDecs tokens = 
  let classVarDecPatterns, remainingTokens = getConsecutivePatterns (fun x -> x = (Keyword "static") || x = (Keyword "field")) (fun x -> x = (Symbol ';')) tokens

  let doOneDec listOfTokens =
    let staticOrField, listOfTokens = getNextTokenIf (isOneOfTokens [(Keyword "static"); (Keyword "field")]) listOfTokens 
    let typ, listOfTokens = getNextTokenIf isTypeProgramStructure listOfTokens
    let varName, listOfTokens = getNextTokenIf  (isSameType (Identifier "_")) listOfTokens
    let otherVarsXml = listOfTokens |> List.map tokenToXml
                                    |> List.reduce (fun x y -> x + "\n  " + y)
    $$"""<classVarDec>
  {{tokenToXml staticOrField}}
  {{tokenToXml typ}}
  {{tokenToXml varName}}
  {{otherVarsXml}}
</classVarDec>
"""
  match classVarDecPatterns with
  | [] -> "", remainingTokens
  | _ ->
    let xml = classVarDecPatterns
              |> List.map doOneDec
              |> List.reduce (+)
    (xml, remainingTokens)


let CompileSubroutineDecs tokens =
  ("", tokens)

let CompileClass tokens = 
 let tokens = eatIf (isSameToken (Keyword "class")) tokens
 let className, tokens = getNextTokenIf ((isSameType (Identifier "_"))) tokens
 let tokens = eatIf (isSameToken (Symbol '{')) tokens
 let classVarDecs, tokens = CompileClassVarDecs tokens
 let classSubroutineDecs, tokens = CompileSubroutineDecs tokens
 let tokens = eatIf (isSameToken (Symbol '}')) tokens
 $$"""<class>
  <keyword> class </keyword>
  <keyword> {{deconstruct className}} </keyword>
  <symbol> { </symbol>"""
  + classVarDecs
  + classSubroutineDecs
  + """
  <symbol> } </symbol>
</class>
"""
let classTokens = [
  Keyword "class";
  Identifier "Main";
  Symbol '{';
  Symbol '}';
]
let testTokens = [
  Keyword "var";
  Keyword "int";
  Identifier "i"
  Symbol ',';
  Identifier "j"
  Symbol ';';
  Keyword "var";
  Keyword "String";
  Identifier "s";
  Symbol ';'
 ]

let testTokens2 = [
  Symbol '}';
  Identifier "doggo";
  Keyword "null";
]

let classVarDecsTest = [
  Keyword "static";
  Keyword "boolean";
  Identifier "test";
  Symbol ','; 
  Identifier "test2";
  Symbol ';';
  Keyword "static";
  Keyword "int";
  Identifier "i";
  Symbol ';'
]

printfn "%A" (getConsecutivePatterns (fun x -> x = (Keyword "static")) (fun x -> x = (Symbol ';')) classVarDecsTest)
printfn "%A" (CompileClassVarDecs testTokens)
printfn "%A" (CompileClassVarDecs classVarDecsTest)
printfn "%A" ((isOneOfTokens [(Keyword "nut"); (Identifier "salad"); (Keyword "int"); (Symbol '{')]) [(Identifier "Point"); (Identifier "string")]) 
printfn "%A" ((isOneOfTokens [(Keyword "nut"); (Identifier "Point"); (Keyword "int"); (Symbol '{')]) [(Identifier "Point"); (Identifier "string")]) 
printfn "%A" (isTypeProgramStructure [(Keyword "nut")]) 
printfn "%A" (isTypeProgramStructure [(Keyword "int")]) 
printfn "%A" (isTypeProgramStructure [(Keyword "char")]) 
printfn "%A" (isTypeProgramStructure [(Keyword "boolean")]) 
printfn "%A" (isTypeProgramStructure [(Identifier "Point")]) 
printfn "%A" (CompileClass classTokens)
printfn "%A" (eatIf (isSameToken (Symbol '}')) testTokens2)
printfn "%A" (eatIf (isSameToken (Keyword "var")) testTokens)
//printfn "%A" (getNextTokenIf (isSameType (Identifier "dog")) testTokens)
//printfn "%A" (getNextTokenIf (isSameToken (Identifier "var")) testTokens)
printfn "%A" (getNextTokenIf (isSameToken (Keyword "var")) testTokens)
printfn "%A" (getNextTokenIf (isSameType (Keyword "_")) testTokens)
printfn "%A" (advanceUntil (fun x -> x = (Symbol ';')) testTokens true)
printfn "%A" (getConsecutivePatterns (fun x -> x = (Keyword "var")) (fun x -> x = (Symbol ';')) testTokens)
