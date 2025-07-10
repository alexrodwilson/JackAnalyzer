#load "Tokenizer.fs"
open Tokenizer
let deconstruct token = 
  match token with 
  | Identifier i -> i
  | Keyword k -> k
  | Symbol s -> (string s)
  | IntConstant i -> (string i)
  | StringConstant s -> s

let advanceUntil target tokens returnTarget = 
  let rec aux toReturn remainingTokens =
    match remainingTokens with
    | head::tail when head = target -> if returnTarget then
                                        List.rev (head::toReturn), tail
                                       else 
                                        (List.rev toReturn), remainingTokens
    | head::tail -> aux (head::toReturn) tail
  aux [] tokens


let getConsecutivePatterns startToken endToken tokens =
  let rec aux patterns remainingTokens = 
    match remainingTokens with
    | [] -> List.rev patterns, remainingTokens
    | head::tail when head <> startToken -> (List.rev patterns), remainingTokens
    | head::tail ->
      let pattern, remainingTokens = advanceUntil endToken remainingTokens true
      aux (pattern::patterns) remainingTokens
  aux [] tokens

let eatIf (test: Token list -> bool) (tokens: Token list): Token list =
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
  | Identifier _ -> "Identifier"
  | Keyword _ -> "Keyword"
  | Symbol _ -> "Symbol"
  | IntConstant _ -> "IntConstant"
  | StringConstant _ -> "StringConstant"

let isSameType token = 
  fun (xs: Token list) -> (typeToString xs.Head) = (typeToString token)

let CompileClassVarDecs tokens =
  ("", tokens)

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
  <symbol> { </symbol>
  {classVarDecs}
  {classSubroutineDecs}
  <symbol> } </symbol>
  """
  
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
  Keyword "null";]

printfn "%A" (eatIf (isSameToken (Symbol '}')) testTokens2)
printfn "%A" (eatIf (isSameToken (Keyword "var")) testTokens)
//printfn "%A" (getNextTokenIf (isSameType (Identifier "dog")) testTokens)
//printfn "%A" (getNextTokenIf (isSameToken (Identifier "var")) testTokens)
printfn "%A" (getNextTokenIf (isSameToken (Keyword "var")) testTokens)
printfn "%A" (getNextTokenIf (isSameType (Keyword "_")) testTokens)
printfn "%A" (advanceUntil (Symbol ';') testTokens true)
printfn "%A" (getConsecutivePatterns (Keyword "var") (Symbol ';') testTokens)
