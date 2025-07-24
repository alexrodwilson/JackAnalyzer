#load "Tokenizer.fs"
open Tokenizer
let SPACES_PER_INDENT = 2
let deconstruct token = 
  match token with 
  | Identifier i -> i
  | Keyword k -> k
  | Symbol s -> (string s)
  | IntConstant i -> (string i)
  | StringConstant s -> s

let indent nestingLevel (string: string) = 
  let spaces = String.replicate (nestingLevel * SPACES_PER_INDENT) " "
  let s = string.Replace("\n", "\n" + spaces)
  spaces + s


let advanceUntil test tokens returnLastToken = 
  let rec aux toReturn remainingTokens =
    match remainingTokens with
    | [] -> List.rev toReturn, []
    | head::tail when test head -> if returnLastToken then
                                    List.rev (head::toReturn), tail
                                   else 
                                    (List.rev toReturn), remainingTokens
    | head::tail -> aux (head::toReturn) tail
  aux [] tokens

let advanceUntilMatchingBracket openingBracket closingBracket tokens includeBrackets = 
  let rec aux toReturn remainingTokens lbs rbs = 
    match remainingTokens with
    | [] -> failwith "No matching bracket found"
    | head::tail when head = openingBracket -> aux (head::toReturn) tail (lbs + 1) rbs
    | head::tail when head = closingBracket && (lbs = rbs + 1) ->
      match includeBrackets with
      | true -> List.rev (head::toReturn), tail
      | false -> (List.rev toReturn).Tail, tail
    | head::tail when head = closingBracket -> aux (head::toReturn) tail lbs (rbs + 1)
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
  | [] -> failwith "Testing an empty list for a token"
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


let rec CompileTerm (tokens: Token list) nestingLevel = 
 // let toXmlAndWrap = tokenToXml >> (wrapXml "term")
  let innerXml, leftOverTokens =
    match tokens.Head with
    | IntConstant i -> indent (nestingLevel + 1) (tokenToXml tokens.Head), tokens.Tail
    | StringConstant s ->indent (nestingLevel + 1) (tokenToXml tokens.Head), tokens.Tail
    | Keyword k when (List.contains k ["true"; "false"; "null"; "this"]) -> indent (nestingLevel + 1) (tokenToXml tokens.Head), tokens.Tail
    | Symbol s when s = '(' -> 
      let expressionTokens, remainingTokens = advanceUntilMatchingBracket (Symbol '(') (Symbol ')') tokens false 
      let expressionXml = CompileExpression expressionTokens (nestingLevel + 1)
      let termXml = $$"""{{indent (nestingLevel + 1) (tokenToXml (Symbol '('))}}
{{expressionXml}}
{{indent (nestingLevel + 1) (tokenToXml (Symbol ')'))}}"""
      termXml, remainingTokens       
    | Symbol s when (s = '-' || s = '~') -> 
      let unaryOpXml = tokenToXml tokens.Head
      let termXml, remainingTokens = CompileTerm tokens.Tail (nestingLevel + 1)
      (unaryOpXml + "\n" + termXml), remainingTokens
    | Identifier i when tokens.Tail = [] -> indent (nestingLevel + 1 ) (tokenToXml tokens.Head), []
    | Identifier i when tokens.Tail.Head = (Symbol '[') ->
      let varNameXml = tokenToXml tokens.Head
      let leftBracketXml = tokenToXml tokens.Tail.Head
      let expressionTokens, remainingTokens = advanceUntil (fun x -> x = (Symbol ']')) tokens.Tail.Tail false
      let expressionXml = CompileExpression expressionTokens (nestingLevel + 1)
      let rightBracketXml = tokenToXml remainingTokens.Head
      $$"""{{indent (nestingLevel + 1) varNameXml}}
{{indent (nestingLevel + 1) leftBracketXml}}
{{expressionXml}}
{{indent (nestingLevel + 1) rightBracketXml}}""", remainingTokens.Tail
    | Identifier i when tokens.Tail.Head = (Symbol '(') ->
      let subroutineName, tokens = getNextTokenIf (isSameType (Identifier "_")) tokens
      let tokens = eatIf (isSameToken (Symbol '(')) tokens
      let expressionListTokens, tokens = advanceUntil (fun x -> x = (Symbol ')')) tokens false
      let expressionListXml, count = CompileExpressionList expressionListTokens (nestingLevel + 1)
      let tokens = eatIf (isSameToken (Symbol ')')) tokens
      $$"""{{indent (nestingLevel + 1) (tokenToXml subroutineName)}}
{{indent (nestingLevel + 1) (tokenToXml (Symbol '('))}}
{{expressionListXml}}
{{indent (nestingLevel + 1) (tokenToXml (Symbol ')'))}}""", tokens
    | Identifier i when tokens.Tail.Head = (Symbol '.') ->
      let classOrVarName, tokens = getNextTokenIf (isSameType (Identifier "_")) tokens
      let tokens = eatIf (isSameToken (Symbol '.')) tokens
      let subroutineName, tokens = getNextTokenIf (isSameType (Identifier "_")) tokens
      let tokens = eatIf (isSameToken (Symbol '(')) tokens
      let expressionListTokens, tokens = advanceUntil (fun x -> x = (Symbol ')')) tokens false
      let expressionListXml, count = CompileExpressionList expressionListTokens (nestingLevel + 1)
      let tokens = eatIf (isSameToken (Symbol ')')) tokens
      $$"""{{indent (nestingLevel + 1) (tokenToXml classOrVarName)}}
{{indent (nestingLevel + 1) (tokenToXml (Symbol '.'))}}
{{indent (nestingLevel + 1) (tokenToXml subroutineName)}}
{{indent (nestingLevel + 1) (tokenToXml (Symbol '('))}}
{{expressionListXml}}
{{indent (nestingLevel + 1) (tokenToXml (Symbol ')'))}}""", tokens
    | Identifier i -> indent (nestingLevel + 1) (tokenToXml tokens.Head), tokens.Tail
    | _ -> failwith ("Unexpected token found in CompileTerm: " + (string tokens.Head))
  (indent nestingLevel "<term>") + "\n" + innerXml + "\n" + (indent nestingLevel "</term>"), leftOverTokens
  

and CompileExpression tokens nestingLevel =
  let rec aux expectingTerm remainingTokens xml =
    match remainingTokens with 
    | [] -> $$"""{{indent nestingLevel "<expression>"}}
{{xml}}
{{indent nestingLevel "</expression>"}}"""
    | head::tail when (not expectingTerm) -> 
      match head with
      | _ when (isOp head) -> aux true tail (xml + "\n" + (indent (nestingLevel + 1) (tokenToXml head)) + "\n")
      | _ -> failwith ("Unexpected token when expected op in expression: " + (string head))
    | head::tail -> 
      let termXml, remainingTokens = CompileTerm remainingTokens (nestingLevel + 1)
      aux false remainingTokens (xml + termXml)
  let xml = aux true tokens ""
  indent nestingLevel xml

and CompileExpressionList tokens nestingLevel = 
  let rec aux remainingTokens xml count expectingExpression =
    match remainingTokens with
    | [] -> $$"""{{indent nestingLevel "<expressionList>"}} 
{{xml}}{{indent nestingLevel "</expressionList>"}}""", count
    | head::tail when expectingExpression ->
      let currentExpressionTokens, remainingTokens = advanceUntil (fun x -> x = (Symbol ',')) remainingTokens false
      let currentExpressionXml =  (indent (nestingLevel + 1) (CompileExpression currentExpressionTokens 0)) + "\n"
      aux remainingTokens (xml + currentExpressionXml) (count + 1) false
    | head::tail when not expectingExpression ->
      let comma, remainingTokens = getNextTokenIf (isSameToken (Symbol ',')) remainingTokens
      aux remainingTokens (xml + (indent (nestingLevel + 1) (tokenToXml comma)) + "\n") count true
  let xml, count = aux tokens "" 0 true
  xml, count

let CompileLetStatement tokens nestingLevel = 
  let tokens = eatIf (isSameToken (Keyword "let")) tokens
  let varName, tokens = getNextTokenIf (isSameType (Identifier "_")) tokens
  let maybeLeftSquareBracket, tokens = maybeGetNextTokenIf (isSameToken (Symbol '[')) tokens
  let arrayIndexExpressionXml , tokens =
    match maybeLeftSquareBracket with
    | Some t -> let expressionTokens, tokens = advanceUntil (fun x -> x = (Symbol ']')) tokens false
                let expressionXml = CompileExpression expressionTokens 0
                let tokens = eatIf (isSameToken (Symbol ']')) tokens
                ($$"""
{{indent (nestingLevel + 1) "<symbol> [ </symbol>"}}
{{indent (nestingLevel + 1) expressionXml}}
{{indent (nestingLevel + 1) "<symbol> ] </symbol>"}} """), tokens
    | None -> ("", tokens)
  let tokens = eatIf (isSameToken (Symbol '=')) tokens
  let rhsExpressionTokens, remainingTokens = advanceUntil (fun x -> x = (Symbol ';')) tokens false
  let rhsExpressionXml = CompileExpression rhsExpressionTokens 0
  let remainingTokens = eatIf (isSameToken (Symbol ';')) remainingTokens
  $$"""{{indent nestingLevel "<letStatement>"}}
{{indent (nestingLevel + 1) "<keyword> let </keyword>"}}
{{indent (nestingLevel + 1) (tokenToXml varName)}}{{arrayIndexExpressionXml}}
{{indent (nestingLevel + 1) (tokenToXml (Symbol '='))}}
{{indent (nestingLevel + 1) rhsExpressionXml}}
{{indent (nestingLevel + 1) (tokenToXml (Symbol ';'))}}
{{indent nestingLevel "</letStatement>"}}""", remainingTokens

let CompileDoStatement tokens nestingLevel =
  let tokens = eatIf (isSameToken (Keyword "do")) tokens
  let subroutineCallTokens, tokens = advanceUntil (fun x -> x = (Symbol ';')) tokens false
  let subroutineCallXml, notNeeded = CompileTerm subroutineCallTokens 0
  let tokens = eatIf (isSameToken (Symbol ';')) tokens
  $$"""{{indent nestingLevel "<doStatement>"}}
{{indent (nestingLevel + 1) "<keyword> do </keyword>"}}
{{indent (nestingLevel + 1) subroutineCallXml}}
{{indent (nestingLevel + 1) "<symbol> ; </symbol>"}}
{{indent nestingLevel "</doStatement>"}}""", tokens

let CompileReturnStatement tokens nestingLevel = 
  let tokens = eatIf (isSameToken (Keyword "return")) tokens
  let expressionTokens, tokens = advanceUntil (fun x -> x = (Symbol ';')) tokens false
  let expressionXml =
    match expressionTokens with 
    | [] -> ""
    | _ -> "\n" + (CompileExpression expressionTokens 0) 
  let tokens = eatIf (isSameToken (Symbol ';')) tokens
  $$"""{{indent nestingLevel "<returnStatement>"}}
{{indent (nestingLevel + 1) "<keyword> return </keyword>"}}{{indent (nestingLevel + 1) expressionXml}}
{{indent (nestingLevel + 1) "<symbol> ; </symbol>"}}
{{indent nestingLevel "</returnStatement>"}}""", tokens

let rec CompileStatements tokens nestingLevel = 
  let rec aux tokens xml =
    match tokens with
    | [] -> xml, tokens
    | head::tail when head = Symbol '}' -> xml, tokens
    | head::tail when head = (Keyword "let") -> 
      let letStatementXml, tokens = CompileLetStatement tokens 0
      aux tokens (xml + "\n" + letStatementXml)
    | head::tail when head = (Keyword "do") ->
      let doStatementXml, tokens = CompileDoStatement tokens 0
      aux tokens (xml + "\n" + doStatementXml)
    | head::tail when head = (Keyword "return") ->
      let returnStatementXml, tokens = CompileReturnStatement tokens 0
      aux tokens (xml + "\n" + returnStatementXml)
    | head::tail when head = (Keyword "if") ->
      let ifStatementXml, tokens = CompileIfStatement tokens 0
      aux tokens (xml + "\n" + ifStatementXml)
    | head::tail when head = (Keyword "while") ->
      let whileStatementXml, tokens = CompileWhileStatement tokens 0
      aux tokens (xml + "\n" + whileStatementXml)
    | head::tail -> failwith ("Unexpected token in CompileStatements" + (string head))
  let statementsXml, remainingTokens = aux tokens ""
  indent nestingLevel $$"""{{indent nestingLevel "<statements>"}}{{indent (nestingLevel + 1) statementsXml}}
{{indent nestingLevel "</statements>"}}""", remainingTokens

and CompileIfStatement tokens nestingLevel =
  let tokens = eatIf (isSameToken (Keyword "if")) tokens
  let expressionTokens, tokens = advanceUntilMatchingBracket (Symbol '(') (Symbol ')') tokens false
  let expressionXml = CompileExpression expressionTokens 0
  let statementsTokens, tokens = advanceUntilMatchingBracket (Symbol '{') (Symbol '}') tokens false
  let statementsXml, _ = CompileStatements statementsTokens 0
  match tokens with
  | head::tail when head = (Keyword "else") -> 
    let tokens = eatIf (isSameToken (Keyword "else")) tokens 
    let elseStatementsTokens, tokens =  advanceUntilMatchingBracket (Symbol '{') (Symbol '}') tokens false
    let elseStatementsXml, _  = CompileStatements elseStatementsTokens 0
    $$"""{{indent nestingLevel "<ifStatement>"}}
{{indent (nestingLevel + 1) (tokenToXml (Keyword "if"))}}
{{indent (nestingLevel + 1) (tokenToXml (Symbol '('))}}
{{indent (nestingLevel + 1) expressionXml}}
{{indent (nestingLevel + 1) (tokenToXml (Symbol ')'))}}
{{indent (nestingLevel + 1) (tokenToXml (Symbol '{'))}}
{{indent (nestingLevel + 1) statementsXml}}
{{indent (nestingLevel + 1) (tokenToXml (Symbol '}'))}}
{{indent (nestingLevel + 1) (tokenToXml (Keyword "else"))}}
{{indent (nestingLevel + 1) (tokenToXml (Symbol '{'))}}
{{indent (nestingLevel + 1) elseStatementsXml}}
{{indent (nestingLevel + 1) (tokenToXml (Symbol '}'))}}
{{indent nestingLevel "</ifStatement>"}}""", tokens

  | _ ->  $$"""{{indent nestingLevel "<ifStatement>"}}
{{indent (nestingLevel + 1) (tokenToXml (Keyword "if"))}}
{{indent (nestingLevel + 1) (tokenToXml (Symbol '('))}}
{{indent (nestingLevel + 1) expressionXml}}
{{indent (nestingLevel + 1) (tokenToXml (Symbol ')'))}}
{{indent (nestingLevel + 1) (tokenToXml (Symbol '{'))}}
{{indent (nestingLevel + 1) statementsXml}}
{{indent (nestingLevel + 1) (tokenToXml (Symbol '}'))}}
{{indent nestingLevel "</ifStatement>"}}""", tokens


and CompileWhileStatement tokens nestingLevel = 
  let tokens = eatIf (isSameToken (Keyword "while")) tokens
  let expressionTokens, tokens = advanceUntilMatchingBracket (Symbol '(') (Symbol ')') tokens false
  let statementsTokens, tokens = advanceUntilMatchingBracket (Symbol '{') (Symbol '}') tokens false
  let expressionXml = CompileExpression expressionTokens 0
  let statementsXml, _ = CompileStatements statementsTokens 0
  $$"""{{indent nestingLevel "<whileStatement>"}}
{{indent (nestingLevel + 1) (tokenToXml (Keyword "while"))}}
{{indent (nestingLevel + 1) (tokenToXml (Symbol '('))}}
{{indent (nestingLevel + 1) expressionXml}}
{{indent (nestingLevel + 1) (tokenToXml (Symbol ')'))}}
{{indent (nestingLevel + 1) (tokenToXml (Symbol '{'))}}
{{indent (nestingLevel + 1) statementsXml}}
{{indent (nestingLevel + 1) (tokenToXml (Symbol '}'))}}
{{indent nestingLevel "</whileStatement>"}}""", tokens


let CompileVarDecs tokens =
  match tokens with
  | [] -> "", []
  | head::tail when head = Symbol '{' && tokens.Tail.Head = Symbol '}' -> "", []
  | head::tail when head = Symbol '}' && tail = [] -> "", tokens
  | _ -> 
    let patterns, remainingTokens = getConsecutivePatterns (fun x -> x = (Keyword "var")) (fun x -> x = (Symbol ';')) tokens
    let compileOne tokens = 
      let tokens = eatIf (isSameToken (Keyword "var")) tokens
      let typ, tokens = getNextTokenIf isTypeProgramStructure tokens
      let varName, tokens = getNextTokenIf (isSameType (Identifier "_")) tokens
      let otherVarsXml =  tokens |> List.map tokenToXml
                                 |> List.reduce (fun x y -> x + "\n" + y)
      $$"""<varDec>
{{indent 1 "<keyword> var </keyword>"}}
{{indent 1 (tokenToXml typ)}} 
{{indent 1 (tokenToXml varName)}}
{{indent 1 otherVarsXml}} 
</varDec>"""
    patterns |> List.map  compileOne
             |> List.reduce (fun x y -> x + "\n" + y), remainingTokens


let CompileClassVarDecs tokens = 
  let classVarDecPatterns, remainingTokens = getConsecutivePatterns (fun x -> x = (Keyword "static") || x = (Keyword "field")) (fun x -> x = (Symbol ';')) tokens
  let doOneDec listOfTokens =
    let staticOrField, listOfTokens = getNextTokenIf (isOneOfTokens [(Keyword "static"); (Keyword "field")]) listOfTokens 
    let typ, listOfTokens = getNextTokenIf isTypeProgramStructure listOfTokens
    let varName, listOfTokens = getNextTokenIf  (isSameType (Identifier "_")) listOfTokens
    let otherVarsXml = listOfTokens |> List.map tokenToXml
                                    |> List.reduce (fun x y -> x + "\n" + y)
    $$"""{{indent 1 "<classVarDec>"}}
{{indent 2 (tokenToXml staticOrField)}}
{{indent 2 (tokenToXml typ)}}
{{indent 2 (tokenToXml varName)}}
{{indent 2 otherVarsXml}}
{{indent 1 "</classVarDec>"}}
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
    | [] -> 
      let xml = $$"""<parameterList>
{{indent 1 xml}}  
</parameterList>"""
      xml
    | head::tail when head = Symbol ',' -> aux tail count (xml + "\n" + (tokenToXml head) + "\n")
                                           
    | head::tail -> 
      let typ, tokens = getNextTokenIf isTypeProgramStructure tokens
      let varName, tokens = getNextTokenIf (isSameType (Identifier "_")) tokens
      let newXml = $$"""{{tokenToXml typ}}
{{tokenToXml varName}}"""
      aux tokens (count + 1) (xml + newXml)
  match tokens with 
  | [] -> """<parameterList> </parameterList>"""
  | _ -> aux tokens 0 ""
        
let CompileSubroutineBody (tokens: Token list) = 
  let tokens = eatIf (isSameToken (Symbol '{')) tokens
  let varDecsXml, tokens = CompileVarDecs tokens
  let statementsXml, tokens = CompileStatements tokens 0
  let tokens = eatIf (isSameToken (Symbol '}')) tokens
  $$"""<subroutineBody>
{{indent 1 "<symbol> { </symbol>"}}
{{indent 1 varDecsXml}}
{{indent 1 statementsXml}}
{{indent 1 "<symbol> } </symbol>"}}
</subroutineBody"""
  (*let varDecPatterns, tokens = getConsecutivePatterns (fun x -> x = (Keyword "var")) (fun x -> x = (Symbol ';')) tokens
  let compileVarDec varDecTokens =
    let varDecTokens = eatIf (isSameToken (Keyword "var")) varDecTokens
    let typ, varDecTokens = getTokenIf isTypeProgramStructure varDecTokens
    let varName, varDecTokens = getTokenIf (isSameType (Identifier "_")) varDecTokens
    let handleCommaThenVarName expectingComma ts xml
      match ts with
      | head::tail when head = (Symbol ';') -> xml
      | head::tail when head = (Symbol ',') && expectingComma -> handleCommaThenVarName false tail (xml + "\n" + (tokenToXml head))
      | head::tail when ((isSameType (Identifier "_")) ts) && not expectingComma -> handleCommaThenVarName true tail (xml + "\n" + (tokenToXml head))
      | head::tail -> failwith ("Unexpected token in CompileSubroutineBody: " + (string head))
    let commaAndVarXml = handleCommaThenVarName true varDecTokens ""
    $$"""{{tokenToXml (Keyword "var")}}
{{tokenToXml type}}
{{tokenToXml varName}}
{{commaAndVarXml}}"""
  varDecPatterns |> List.map*)
  

    

let CompileSubroutineDecs tokens =
  let doOneSubroutineDec tokens =
    let constructorFunctionMethod, ts = getNextTokenIf (isOneOfTokens [Keyword "constructor"; Keyword "function"; Keyword "method"]) tokens
    let voidOrType, ts = getNextTokenIf isTypeOrVoid ts
    let subroutineName, ts = getNextTokenIf (isSameType (Identifier "_")) ts
    let ts = eatIf (isSameToken (Symbol '(')) ts
    let parameterTokens, ts = advanceUntil (fun x -> x = (Symbol ')')) ts false
    let parameterXml = CompileParameterList parameterTokens 
    let ts = eatIf (isSameToken (Symbol ')')) ts
    let subroutineBodyTokens, ts = advanceUntilMatchingBracket (Symbol '{') (Symbol '}') ts true
    let subroutineBodyXml = CompileSubroutineBody subroutineBodyTokens
    $$"""<subroutineDec>
{{indent 1 (tokenToXml constructorFunctionMethod)}}
{{indent 1 (tokenToXml voidOrType)}}
{{indent 1 (tokenToXml subroutineName)}}
{{indent 1 "<symbol> ( </symbol>"}}  
{{indent 1 parameterXml}}
{{indent 1 "<symbol> ) </symbol>"}}
{{indent 1  subroutineBodyXml}}
</subroutineDec>
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
  Identifier "No";
  Identifier "Alarms";
  Identifier "andNoSurprises"
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

let letTest = [Keyword "let"; Identifier "x"; Symbol '='; IntConstant 2; Symbol '+'; IntConstant 3; Symbol ';']
let letTest2 = [Keyword "let"; Identifier "x"; Symbol '['; IntConstant 5; Symbol '-'; IntConstant 2; Symbol ']'; Symbol '='; StringConstant "dog"; Symbol ';']
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
let returnTest = [Keyword "return"; Identifier "doSomething"; Symbol '('; IntConstant 2; Symbol '+'; IntConstant 1; 
  Symbol ','; StringConstant "dog"; Symbol ','; Identifier "i"; Symbol ')'; Symbol ';'] 
let subroutineCallTest2 = [Identifier "point"; Symbol '.'; Identifier "doSomething"; Symbol '('; IntConstant 1; Symbol '+'; IntConstant 2; Symbol '-'; IntConstant 1;
                           Symbol ')']
let doTest = [Keyword "do"; Identifier "doSomething"; Symbol '('; IntConstant 1; Symbol '+'; IntConstant 2;
Symbol ','; StringConstant "dog"; Symbol ','; Identifier "i"; Symbol ')'; Symbol ';']
let statementsTest = List.concat [letTest; doTest; returnTest]
let ifStatementTest = [Keyword "if"; Symbol '('; Symbol '('; IntConstant 1; Symbol '+'; IntConstant 3; Symbol ')'; Symbol '='; 
  IntConstant 4; Symbol ')'; Symbol '{'; Keyword "do"; Identifier "someFunction"; Symbol '('; StringConstant "dog"; Symbol ')'; Symbol ';'; Symbol '}';
  Keyword "else"; Symbol '{'; Keyword "do"; Identifier "someOtherFunction"; Symbol '('; IntConstant 69; Symbol ')'; Symbol ';'; Symbol '}';
  Identifier "No"; Identifier "Surprises"; Identifier "Please"]
let ifStatementTest2 = [Keyword "if"; Symbol '('; Symbol '('; IntConstant 1; Symbol '+'; IntConstant 3; Symbol ')'; Symbol '='; 
  IntConstant 4; Symbol ')'; Symbol '{'; Keyword "do"; Identifier "someFunction"; Symbol '('; StringConstant "dog"; Symbol ')'; Symbol ';'; Symbol '}';
  Identifier "No"; Identifier "Surprises"; Identifier "Please"]
//printfn "%A" (CompileTerm termTest1) 
//printfn "%A" (CompileTerm termTest2)        
//printfn "%A" (getTokensBeforeOp beforeOpTest)
//printfn "%A" (advanceUntilMatchingBracket (Symbol '(') (Symbol ')') [Symbol '('; Symbol '('; IntConstant 1; Symbol '+'; IntConstant 3; Symbol ')'; 
 // Symbol '='; IntConstant 4; Symbol ')' ])
let whileStatementTest = [Keyword "while"; Symbol '('; Identifier "i"; Symbol '='; IntConstant 2; Symbol ')'; Symbol '{';
  Keyword "let"; Identifier "x"; Symbol '='; IntConstant 2; Symbol '+'; IntConstant 3; Symbol ';';
  Keyword "let"; Identifier "x"; Symbol '['; IntConstant 5; Symbol '-'; IntConstant 2; Symbol ']'; Symbol '='; StringConstant "dog"; Symbol ';';
  Symbol '}'; Identifier "No"; Identifier "Surprises"; Identifier "Please"]
let expressionListTest = [IntConstant 4; Symbol '+'; IntConstant 2; Symbol ','; StringConstant "dog"; Symbol '+'; StringConstant "cat"; Symbol ','; Identifier "i"]
let subroutineBodyTest = List.concat [[Symbol '{'; Keyword "var"; Keyword "int"; Identifier "i"; Symbol ','; Identifier "j"; Symbol ';'; Keyword "var"; Identifier "String"; Identifier "s"; Symbol ';';]; statementsTest; [Symbol '}']]
let emptyBracketsTest = [Symbol '{'; Symbol '}']
//printfn "%A" (CompileSubroutineDecs subroutineDecsTest)
//printfn "%A" (getConsecutivePatterns (fun x -> false) (fun x -> false) [Symbol ';'; Identifier "plop"]) 
//printfn ""
printfn "%A" (CompileSubroutineBody emptyBracketsTest)
printfn ""
printfn "%A" (CompileSubroutineBody subroutineBodyTest)
printfn ""
printfn "%A" (CompileVarDecs [Symbol '{'; Symbol '}'])
printfn ""
printfn "%A" (CompileVarDecs varDecsTest)
printfn ""
printfn "%A" (CompileClassVarDecs classVarDecsTest)
printfn ""
printfn "%A" (CompileIfStatement ifStatementTest2 1)
printfn ""
printfn "%A" (CompileExpressionList expressionListTest 1)
printfn ""
printfn "%A" (CompileWhileStatement whileStatementTest 1)
printfn ""
printfn "%A" (CompileIfStatement ifStatementTest 1)
printfn ""
printfn "%A" (CompileStatements statementsTest 1)
printfn ""
printfn "%A" (CompileReturnStatement returnTest 1)
printfn ""
printfn "%A" (CompileLetStatement letTest 1)
printfn ""
printfn "%A" (CompileLetStatement letTest2 1)
printfn ""
printfn "%A" (CompileDoStatement doTest 1)
printfn ""
printfn "%A" (CompileReturnStatement [Keyword "return"; Symbol ';'] 1)
//printfn "%A" (CompileExpression [IntConstant 2; Symbol '+'; IntConstant 1])
//printfn ""
//printfn "%A" (CompileExpressionList [IntConstant 2; Symbol '+'; IntConstant 1])
//printfn ""
//printfn "%A" (CompileExpression expressionTest2)
//printfn ""
//printfn "%A" (CompileExpression termTest3)
//printfn ""
//printfn "%A" (CompileExpression termTest2)
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
