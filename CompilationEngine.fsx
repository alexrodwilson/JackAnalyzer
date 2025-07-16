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
    | [] -> toReturn, []
    | head::tail when test head -> if returnLastToken then
                                    List.rev (head::toReturn), tail
                                   else 
                                    (List.rev toReturn), remainingTokens
    | head::tail -> aux (head::toReturn) tail
  aux [] tokens

let advanceUntilMatchingCurlyBracket tokens = 
  let rec aux toReturn remainingTokens lbs rbs = 
    match remainingTokens with
    | [] -> failwith "No matching bracket found"
    | head::tail when head = (Symbol '{') -> aux (head::toReturn) tail (lbs + 1) rbs
    | head::tail when head = (Symbol '}') && (lbs = rbs + 1) -> List.rev (head::toReturn), tail
    | head::tail when head = (Symbol '}') -> aux (head::toReturn) tail lbs (rbs + 1)
    | head::tail -> aux (head::toReturn) tail lbs rbs
  aux [] tokens 0 0


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
  | head::tail -> failwith ("Bad Argument " + (string head)) 

let getNextTokenIf test tokens = 
  match tokens with
  | head::tail when (test tokens) -> head, tail
  | head::tail -> failwith ("Bad Argument " + (string head)) 

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

let isTypeOrVoid xs =
  (isTypeProgramStructure xs) || xs.Head = (Keyword "void")

let check test token = 
  if test token then token
  else failwith ("Unexpected token found in stream" + (string token))

let maybeGetNextTokenIf test tokens =
  match tokens with
  | [] -> failwith "Token stream empty unexpectedly"
  | head::tail when test tokens -> Some head, tail
  | _ -> None, tokens



let isOp token = 
  let ops = ['+'; '-'; '*'; '/'; '&'; '|'; '<'; '>';'=']
  match token with
  | Symbol s when (List.contains s ops) -> true
  | _ -> false

let getTokensBeforeOp tokens = 
  advanceUntil (fun x -> isOp x) tokens false

let wrapXml wrappingS sToWrap =
  $$"""<{{wrappingS}}> 
{{sToWrap}} 
</{{wrappingS}}>"""

let CompileSubroutineCall tokens = 
  failwith "Not implemented yet"

let rec CompileTerm (tokens: Token list) = 
  let toXmlAndWrap = tokenToXml >> (wrapXml "term")
  match tokens.Head with
  | IntConstant i -> toXmlAndWrap tokens.Head, tokens.Tail
  | StringConstant s ->  toXmlAndWrap tokens.Head, tokens.Tail
  | Keyword k -> toXmlAndWrap tokens.Head, tokens.Tail
  | Symbol s when s = '(' -> 
    let leftBracketXml = tokenToXml tokens.Head
    let expressionTokens, remainingTokens = advanceUntil (fun x -> x = (Symbol ')')) tokens.Tail false
    let expressionXml = CompileExpression expressionTokens
    let rightBracketXml = tokenToXml remainingTokens.Head
    let termXml = $$"""{{leftBracketXml}}
{{expressionXml}}
{{rightBracketXml}}"""
    wrapXml "term" termXml, remainingTokens.Tail       
  | Symbol s when (s = '-' || s = '~') -> 
    let unaryOpXml = tokenToXml tokens.Head
    let termXml, remainingTokens = CompileTerm tokens.Tail
    (wrapXml "term" (unaryOpXml + "\n" + termXml), remainingTokens)
  | Identifier i when tokens.Tail = [] -> toXmlAndWrap tokens.Head, tokens.Tail
  | Identifier i when tokens.Tail.Head = (Symbol '[') ->
    let varNameXml = tokenToXml tokens.Head
    let leftBracketXml = tokenToXml tokens.Tail.Head
    let expressionTokens, remainingTokens = advanceUntil (fun x -> x = (Symbol ']')) tokens.Tail.Tail false
    let expressionXml = CompileExpression expressionTokens
    let rightBracketXml = tokenToXml remainingTokens.Head
    wrapXml "term" $$"""{{varNameXml}}
{{leftBracketXml}}
{{expressionXml}}
{{rightBracketXml}}""", remainingTokens.Tail
  | Identifier i when tokens.Tail.Head = (Symbol '(') ->
    toXmlAndWrap (CompileSubroutineCall tokens), []

and CompileExpression tokens =
  let rec aux expectingTerm remainingTokens xml =
    match remainingTokens with 
    | [] -> wrapXml "expression" xml
    | head::tail when (not expectingTerm) -> 
      match head with
      | _ when (isOp head) -> aux true tail (xml + "\n" + (tokenToXml head) + "\n")
      | _ -> failwith ("Unexpected token when expected op in expression: " + (string head))
    | head::tail -> 
      let termXml, remainingTokens = CompileTerm remainingTokens
      aux false remainingTokens (xml + termXml)
  aux true tokens ""


let CompileLetStatement tokens = 
  let tokens = eatIf (isSameToken (Keyword "let")) tokens
  let varName, tokens = getNextTokenIf (isSameType (Identifier "_")) tokens
  let maybeLeftSquareBracket, tokens = maybeGetNextTokenIf (isSameToken (Symbol '[')) tokens
  let arrayIndexExpressionXml , tokens =
    match maybeLeftSquareBracket with
    | Some t -> let expressionTokens, tokens = advanceUntil (fun x -> x = (Symbol ']')) tokens false
                let expressionXml = CompileExpression expressionTokens
                let tokens = eatIf (isSameToken (Symbol ']')) tokens
                ($$"""<symbol> [ </symbol>
<expression>
{{expressionXml}}
</expression>
<symbol> ] </symbol>

""", tokens)
    | None -> ("", tokens)

  let tokens = eatIf (isSameToken (Symbol '=')) tokens
  let rhsExpressionTokens, remainingTokens = advanceUntil (fun x -> x = (Symbol ';')) tokens false
  let rhsExpressionXml = CompileExpression rhsExpressionTokens
  let remainingTokens = eatIf (isSameToken (Symbol ';')) tokens
  $$"""<keyword> let </keyword>
{{tokenToXml varName}}
{{arrayIndexExpressionXml}}
<symbol> = </symbol>
{{rhsExpressionXml}}
""" 
  
  

let CompileVarDecs tokens =
  let patterns, remainingTokens = getConsecutivePatterns (fun x -> x = (Keyword "var")) (fun x -> x = (Symbol ';')) tokens
  let compileOne tokens = 
    let tokens = eatIf (isSameToken (Keyword "var")) tokens
    let typ, tokens = getNextTokenIf isTypeProgramStructure tokens
    let varName, tokens = getNextTokenIf (isSameType (Identifier "_")) tokens
    let otherVarsXml =  tokens |> List.map tokenToXml
                               |> List.reduce (fun x y -> x + "\n" + y)
    $$"""{{tokenToXml typ}} 
{{tokenToXml varName}}
{{otherVarsXml}}
"""
  patterns |> List.map  compileOne
           |> (List.reduce (+)), remainingTokens



let CompileClassVarDecs tokens = 
  let classVarDecPatterns, remainingTokens = getConsecutivePatterns (fun x -> x = (Keyword "static") || x = (Keyword "field")) (fun x -> x = (Symbol ';')) tokens
  let doOneDec listOfTokens =
    let staticOrField, listOfTokens = getNextTokenIf (isOneOfTokens [(Keyword "static"); (Keyword "field")]) listOfTokens 
    let typ, listOfTokens = getNextTokenIf isTypeProgramStructure listOfTokens
    let varName, listOfTokens = getNextTokenIf  (isSameType (Identifier "_")) listOfTokens
    let otherVarsXml = listOfTokens |> List.map tokenToXml
                                    |> List.reduce (fun x y -> x + "\n" + y)
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


let CompileParameterList tokens =
  let rec aux tokens count xml = 
    match tokens with
    | [] -> let xml = "<parameterList>" + "\n" + xml + "</parameterList>" + "\n"
            xml, count
    | head::tail when head = Symbol ',' -> aux tail count (xml + (tokenToXml head) + "\n")
                                           
    | head::tail -> let typ, tokens = getNextTokenIf isTypeProgramStructure tokens
                    let varName, tokens = getNextTokenIf (isSameType (Identifier "_")) tokens
                    let newXml = $$"""{{tokenToXml typ}}
{{tokenToXml varName}}
"""
                    aux tokens (count + 1) (xml + newXml)
  aux tokens 0 ""
        
let CompileSubroutineBody (tokens: Token list) = 
  $$"""<symbol> { </symbol>
<symbol> } </symbol>
"""

let CompileSubroutineDecs tokens =
  let doOneSubroutineDec tokens =
    let constructorFunctionMethod, ts = getNextTokenIf (isOneOfTokens [Keyword "constructor"; Keyword "function"; Keyword "method"]) tokens
    let voidOrType, ts = getNextTokenIf isTypeOrVoid ts
    let subroutineName, ts = getNextTokenIf (isSameType (Identifier "_")) ts
    let ts = eatIf (isSameToken (Symbol '(')) ts
    let parameterTokens, ts = advanceUntil (fun x -> x = (Symbol ')')) ts false
    let parameterXml, parameterCount = CompileParameterList parameterTokens 
    let ts = eatIf (isSameToken (Symbol ')')) ts
    let subroutineBodyTokens, ts = advanceUntilMatchingCurlyBracket ts
    let subroutineBodyXml = CompileSubroutineBody subroutineBodyTokens
    $$"""<subroutineDec>
{{tokenToXml constructorFunctionMethod}}
{{tokenToXml voidOrType}}
{{tokenToXml subroutineName}}
<symbol> ( </symbol>
""" + parameterXml
      + """<symbol> ) </symbol>
"""
      + subroutineBodyXml 
      + """</subroutineDec>
"""
      , ts
  let rec aux remainingTokens xml =
    match remainingTokens with
    | [] -> xml
    | head::tail when head = Keyword "constructor" || head = Keyword "function" || head = Keyword "method" -> 
      let oneDecXml, remainingTokens = doOneSubroutineDec remainingTokens
      let xml = xml + oneDecXml
      aux remainingTokens xml
    | head::tail -> failwith ("Wrong Token when compiling subroutine declarations " + (string head))
  aux tokens ""

    
let CompileClass tokens = 
 let tokens = eatIf (isSameToken (Keyword "class")) tokens
 let className, tokens = getNextTokenIf ((isSameType (Identifier "_"))) tokens
 let tokens = eatIf (isSameToken (Symbol '{')) tokens
 let classVarDecs, tokens = CompileClassVarDecs tokens
 let classSubroutineDecs = CompileSubroutineDecs tokens
 let tokens = eatIf (isSameToken (Symbol '}')) tokens
 $$"""<class>
<keyword> class </keyword>
<keyword> {{deconstruct className}} </keyword>
<symbol> { </symbol>
"""
  + classVarDecs
  + classSubroutineDecs
  + """<symbol> } </symbol>
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

let subroutineDecsTest = [
  Keyword "function";
  Keyword "void";
  Identifier "more";
  Symbol '(';  
  Identifier "Point";
  Identifier "p";
  Symbol ',';
  Keyword "int";
  Identifier "i";
  Symbol ',';
  Keyword "boolean";
  Identifier "b";
  Symbol ')';
  Symbol '{';
  Symbol '}';
  Keyword "constructor";
  Keyword "int";
  Identifier "main";
  Symbol '(';
  Symbol ')';
  Symbol '{';
  Symbol '}'
]
let oneSubroutineTest = [
  Keyword "function";
  Keyword "void";
  Identifier "more";
  Symbol '(';
  Symbol ')';
  Symbol '{';
  Symbol '}';
]
let curlyBracketsTest = [
  Symbol '{';
  Keyword "toast";
  Symbol '{';
  Identifier "dogloose";
  Symbol '}'
  Symbol ';';
  Symbol '}';
  Keyword "int";
  Symbol '}'
]
let curlyBracketsTest2 = [
  Symbol '{';
  Symbol '}';
  Keyword "pooch"
]

let paramTest = [
  Identifier "Point";
  Identifier "p";
  Symbol ',';
  Keyword "int";
  Identifier "i";
  Symbol ',';
  Keyword "boolean";
  Identifier "b";
]

let paramTest2 = [
  Keyword "int";
  Identifier "i"
]

let paramTest1 = []

let varDecsTest = [Keyword "var"; Keyword "int"; Identifier "i"; Symbol ','; Identifier "j"; Symbol ';'; 
  Keyword "var"; Identifier "String"; Identifier "s"; Symbol ';']

let letTest = [Keyword "let"; Identifier "x"; Symbol '='; Identifier "x"; Symbol ';']
let expressionTest1 = [IntConstant 2]
let expressionTest2 = [IntConstant 2; Symbol '+'; IntConstant 20]
//let expressionTest3 = [Identifier "things"; Symbol '['; IntConstant 4; Symbol ']']
let termTest1  = [Symbol '-'; IntConstant 10]
let termTest2 = [Identifier "a"; Symbol '['; IntConstant 3; Symbol '*'; IntConstant 10; Symbol ']'; Symbol '+'; StringConstant "dog"]
let termTest3 = [Symbol '('; IntConstant 1; Symbol ')'; Symbol '*'; IntConstant 3]
let termTest4 = [Keyword "null"]
let termTest5 = [IntConstant 99]
let termTest6 = [Identifier "boris"]
let beforeOpTest = [Symbol '('; IntConstant 3; Symbol '+'; IntConstant 1; Symbol ')'; Symbol '-'; IntConstant 2]

//printfn "%A" (CompileTerm termTest1) 
//printfn "%A" (CompileTerm termTest2)
//printfn "%A" (getTokensBeforeOp beforeOpTest)
printfn ""
printfn "%A" (CompileExpression expressionTest2)
printfn ""
printfn "%A" (CompileExpression termTest3)
printfn ""
printfn "%A" (CompileExpression termTest2)
//printfn "%A" (wrapXml "expression" (wrapXml "term" "dog"))
//printfn "%A" (CompileTerm termTest3) 
//printfn "%A" (CompileTerm termTest4) 
//printfn "%A" (CompileTerm termTest5) 
//printfn "%A" (CompileTerm termTest6) 
//printfn "%A" (CompileExpression expressionTest2) 
//printfn "%A" (CompileLetStatement letTest)  
//printfn "%A" (CompileVarDecs varDecsTest)
//printfn "%A" (CompileParameterList paramTest2)
//printfn "%A" (CompileParameterList paramTest1)
//printfn "%A" (CompileParameterList paramTest)
//printfn "%A" (doOneSubroutineDec oneSubroutineTest)
//printfn "%A" (advanceUntilMatchingCurlyBracket curlyBracketsTest2)
//printfn "%A" (advanceUntilMatchingCurlyBracket curlyBracketsTest)
//printfn "%A" (CompileSubroutineDecs subroutineDecsTest)
//printfn "%A" (getConsecutivePatterns (fun x -> x = (Keyword "static")) (fun x -> x = (Symbol ';')) classVarDecsTest)
//printfn "%A" (CompileClassVarDecs testTokens)
//printfn "%A" (CompileClassVarDecs classVarDecsTest)
//printfn "%A" ((isOneOfTokens [(Keyword "nut"); (Identifier "salad"); (Keyword "int"); (Symbol '{')]) [(Identifier "Point"); (Identifier "string")]) 
//printfn "%A" ((isOneOfTokens [(Keyword "nut"); (Identifier "Point"); (Keyword "int"); (Symbol '{')]) [(Identifier "Point"); (Identifier "string")]) 
//printfn "%A" (isTypeProgramStructure [(Keyword "nut")]) 
//printfn "%A" (isTypeProgramStructure [(Keyword "int")]) 
//printfn "%A" (isTypeProgramStructure [(Keyword "char")]) 
//printfn "%A" (isTypeProgramStructure [(Keyword "boolean")]) 
//printfn "%A" (isTypeProgramStructure [(Identifier "Point")]) 
//printfn "%A" (CompileClass classTokens)
//printfn "%A" (eatIf (isSameToken (Symbol '}')) testTokens2)
//printfn "%A" (eatIf (isSameToken (Keyword "var")) testTokens)
//printfn "%A" (getNextTokenIf (isSameType (Identifier "dog")) testTokens)
//printfn "%A" (getNextTokenIf (isSameToken (Identifier "var")) testTokens)
//printfn "%A" (getNextTokenIf (isSameToken (Keyword "var")) testTokens)
//printfn "%A" (getNextTokenIf (isSameType (Keyword "_")) testTokens)
//printfn "%A" (advanceUntil (fun x -> x = (Symbol ';')) testTokens true)
//printfn "%A" (getConsecutivePatterns (fun x -> x = (Keyword "var")) (fun x -> x = (Symbol ';')) testTokens)
