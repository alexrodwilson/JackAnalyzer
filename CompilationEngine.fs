module CompilationEngine
//#load "Tokenizer.fs"
//#load "SymbolTable.fs"
//#load "VMWriter.fs"
open Tokenizer
open VMWriter

let SPACES_PER_INDENT = 2
let mutable ticker = 0
let mutable CLASS_NAME = ""

type Category = Class | Subroutine | InTable
type Role = Definition | Use
type SubroutineKind = Function | Method | Constructor 

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
    | [] -> 
      printfn "toReturn: %A"  toReturn
      printfn ""
      printfn "remainingTokens: %A" remainingTokens
      failwith ("No matching bracket found")
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
  | [] -> failwith "No tokens found"
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

let identifierToXml identifier category role symbolTable = 
  let name = 
    match identifier with
    | Identifier i -> i
    | _ -> failwith ("Wrong token passed to identifierToXml: " + (string identifier))
  let cat = 
    match category with
    | Class | Subroutine -> string category
    | InTable -> string (SymbolTable.kindOf name symbolTable)
  let index = 
    match category with
    | Class | Subroutine -> "None"
    | InTable -> string (SymbolTable.indexOf name symbolTable)
  let nameXml = $$"""<name> {{name}} </name>"""
  let categoryXml = $$"""<category> {{cat}} </category>"""
  let roleXml = $$"""<role> {{role}} </role>"""
  let indexXml = $$"""<index> {{index}} </index>"""
  $$"""<identifier>
{{indent 1 nameXml}}
{{indent 1 categoryXml}}
{{indent 1 roleXml}}
{{indent 1 indexXml}}
</identifier>"""

let stringConstantToVm s =
  "Not implemented yet"

let getStringFromIdentifierToken identifier = 
  match identifier with
  | Identifier i -> i
  | _ -> failwith ("Expecting identifier, given: " + (string identifier))

let getSegmentAndIndex identifier symbolTable = 
  let name = getStringFromIdentifierToken identifier
  let kind = SymbolTable.kindOf name symbolTable 
  let segment =
    match kind with
    | SymbolTable.Static -> STATIC
    | SymbolTable.Field -> THIS
    | SymbolTable.Arg -> ARG
    | SymbolTable.Var -> LOCAL
    | SymbolTable.None -> failwith("Symbol not found in symbolTable during compilation process: " + (string identifier))
  let index = SymbolTable.indexOf name symbolTable
  segment, index

let identifierToVm token symbolTable =
   let segment, index = getSegmentAndIndex token symbolTable 
   writePush segment index

let rec CompileTerm (tokens: Token list) symbolTable = 
  let innerXml, leftOverTokens =
    match tokens.Head with
    | IntConstant i ->  writePush CONST i, tokens.Tail
    | StringConstant s ->  (stringConstantToVm s), tokens.Tail
    | Keyword k when (List.contains k ["true"; "false"; "null"; "this"]) -> 
      match k with 
      | "this" -> writePush POINTER 0, tokens.Tail 
      | "false" | "null" -> writePush CONST 0, tokens.Tail
      | "true" -> $"{writePush CONST 1}
{writeArithmetic NEG}", tokens.Tail
    | Symbol s when s = '(' -> 
      let expressionTokens, remainingTokens = advanceUntilMatchingBracket (Symbol '(') (Symbol ')') tokens false 
      let expressionVM = CompileExpression expressionTokens symbolTable
      expressionVM, remainingTokens      
    | Symbol s when (s = '-' || s = '~') -> 
      let unaryOpVm = 
       match s with
        | '-' -> writeArithmetic NEG 
        | '~' -> writeArithmetic NOT
      let termVm, remainingTokens = CompileTerm tokens.Tail symbolTable
      (termVm + "\n" + unaryOpVm), remainingTokens
    | Identifier i when tokens.Tail = [] ->  identifierToVm tokens.Head symbolTable, []
    | Identifier i when tokens.Tail.Head = (Symbol '[') ->
      let varNameXml = identifierToXml tokens.Head InTable Use symbolTable
      let expressionTokens, remainingTokens = advanceUntilMatchingBracket (Symbol '[') (Symbol ']') tokens.Tail false
      let expressionXml = CompileExpression expressionTokens symbolTable
      $$"""{{varNameXml}}
{{tokenToXml (Symbol '[')}}
{{expressionXml}}
{{tokenToXml (Symbol ']')}}""", remainingTokens
    | Identifier i when tokens.Tail.Head = (Symbol '(') ->
      let methodNameToken, tokens = getNextTokenIf (isSameType (Identifier "_")) tokens
      let fullMethodName = CLASS_NAME + "." + (getStringFromIdentifierToken methodNameToken)  
      let expressionListTokens, tokens = advanceUntilMatchingBracket (Symbol '(') (Symbol ')') tokens false
      let expressionListVm, nOfArgs = CompileExpressionList expressionListTokens symbolTable
      $"{writePush ARG 0}
{expressionListVm}{writeCall fullMethodName (nOfArgs + 1)}", tokens
    | Identifier classOrVarName when tokens.Tail.Head = (Symbol '.') ->
      let classOrVarNameToken, tokens = getNextTokenIf (isSameType (Identifier "_")) tokens
      let tokens = eatIf (isSameToken (Symbol '.')) tokens
      let subroutineNameToken, tokens = getNextTokenIf (isSameType (Identifier "_")) tokens
      let subroutineName = getStringFromIdentifierToken subroutineNameToken
     // let subroutineNameXml = identifierToXml subroutineNameToken Subroutine Use symbolTable
      let expressionListTokens, tokens = advanceUntilMatchingBracket (Symbol '(') (Symbol ')') tokens false
      let expressionListVm, nOfExpressions = CompileExpressionList expressionListTokens symbolTable
      match (SymbolTable.kindOf classOrVarName symbolTable) with
      | SymbolTable.None ->      
        let fullName = (classOrVarName + "." + subroutineName) 
        $"{expressionListVm}{writeCall fullName nOfExpressions}", tokens
      | _ -> 
        let fullName = (SymbolTable.typeOf classOrVarName symbolTable) + "." + subroutineName
        let segment, index = getSegmentAndIndex classOrVarNameToken symbolTable
        $"{writePush segment index}
{expressionListVm}{writeCall fullName (nOfExpressions + 1)}", tokens
    | Identifier i -> identifierToVm tokens.Head symbolTable, tokens.Tail
    | _ -> failwith ("Unexpected token found in CompileTerm: " + (string tokens.Head))
  innerXml, leftOverTokens

and CompileExpression tokens symbolTable =
  let opToVm token =
    match token with
    | Symbol s -> 
      match s with 
      | '*' -> writeCall "Math.multiply" 2
      | '/' -> writeCall "Math.divide" 2
      | '+' -> writeArithmetic ADD 
      | '-' -> writeArithmetic SUB
      | '=' -> writeArithmetic EQ
      | '>' -> writeArithmetic GT
      | '<' -> writeArithmetic LT
      | '&' -> writeArithmetic AND
      | '|' -> writeArithmetic OR
      | '~' -> writeArithmetic NEG
      | _ -> failwith ("Unexpected symbol where operator expected: " + (string s))
  let rec aux tokens expectingTerm =
    match tokens with 
    | [] -> ""
    | [a] -> 
      let term, _ = CompileTerm [a]  symbolTable
      term
    | head :: tail  when head = Symbol '(' -> 
      let term, leftOver = CompileTerm tokens symbolTable
      match leftOver with 
      | [] -> term
      | head :: tail -> 
        let bVm = opToVm leftOver.Head
        let cVm, tokens = CompileTerm leftOver.Tail symbolTable
        $"{term}
{cVm}
{bVm}" + (aux tokens false)
    | head :: tail  when not (isOp head) -> 
      let aVm, tokens = CompileTerm tokens symbolTable
      match tokens with
      | [] -> aVm
      | head :: tail -> 
        let bVm = opToVm tokens.Head
        let cVm, tokens = CompileTerm tokens.Tail symbolTable
        $"{aVm}
{cVm}
{bVm}" + (aux tokens false)
    | head :: tail when (isOp head) ->
      match expectingTerm with
      | true -> 
        let unaryTerm, leftOvers = CompileTerm tokens symbolTable
        match leftOvers with
        | [] -> unaryTerm
        | head :: tail -> 
          let bVm = opToVm leftOvers.Head
          let cVm, leftOvers = CompileTerm leftOvers.Tail symbolTable
          $"{unaryTerm}
{cVm}
{bVm}" + (aux leftOvers false)
      | false -> 
        let opVm = opToVm head
        let termVm, tokens = CompileTerm tail symbolTable
        $"{termVm}
{opVm}" + (aux tokens false)
  aux tokens true 

and CompileExpressionList tokens symbolTable = 
  let rec aux remainingTokens vm count expectingExpression =
    match remainingTokens with
    | [] -> vm, count
    | head::tail when expectingExpression ->
      let currentExpressionTokens, remainingTokens = advanceUntil (fun x -> x = (Symbol ',')) remainingTokens false
      let currentExpressionVm = (CompileExpression currentExpressionTokens symbolTable) 
      aux remainingTokens (vm + currentExpressionVm + "\n") (count + 1) false
    | head::tail when not expectingExpression ->
      let comma, remainingTokens = getNextTokenIf (isSameToken (Symbol ',')) remainingTokens
      aux remainingTokens vm count true
    | head::tail -> failwith ("Unexpected input to expressionList" + (string head))
  let vm, count = aux tokens "" 0 true
  match vm with
  | "" -> vm, count 
  | _ -> "\n" + vm, count
  
let CompileLetStatement tokens symbolTable = 
  let tokens = eatIf (isSameToken (Keyword "let")) tokens
  let varNameToken, tokens = getNextTokenIf (isSameType (Identifier "_")) tokens
  match tokens.Head with
  | Symbol s when s = '[' ->
    let expressionTokens, tokens = advanceUntilMatchingBracket (Symbol '[') (Symbol ']') tokens false
    let arrayExpressionVm = CompileExpression expressionTokens symbolTable
    let tokens = eatIf (isSameToken (Symbol '=')) tokens
    let rhsExpressionTokens, remainingTokens = advanceUntil (fun x -> x = (Symbol ';')) tokens false
    let rhsExpressionXml = CompileExpression rhsExpressionTokens symbolTable
    let remainingTokens = eatIf (isSameToken (Symbol ';')) remainingTokens
    "not implemented yet", remainingTokens
  | Symbol s when s = '=' ->
    let tokens = eatIf (isSameToken (Symbol '=')) tokens
    let rhsExpressionTokens, remainingTokens = advanceUntil (fun x -> x = (Symbol ';')) tokens false
    let rhsExpressionVm = CompileExpression rhsExpressionTokens symbolTable
    let remainingTokens = eatIf (isSameToken (Symbol ';')) remainingTokens
    let segment, index = getSegmentAndIndex varNameToken  symbolTable
    $"{rhsExpressionVm}
{writePop segment index}
"   , remainingTokens
  | _ -> failwith("Unexpected token found when compiling let statement, expecting '[' or '=': " + (string tokens.Head))

let CompileDoStatement tokens symbolTable =
  let tokens = eatIf (isSameToken (Keyword "do")) tokens
  let subroutineCallTokens, tokens = advanceUntil (fun x -> x = (Symbol ';')) tokens false
  let subroutineCallXml, notNeeded = CompileTerm subroutineCallTokens symbolTable
  let tokens = eatIf (isSameToken (Symbol ';')) tokens
  $"{subroutineCallXml}
{writePop TEMP 0}
", tokens

let CompileReturnStatement tokens subroutineIsVoid subroutineKind symbolTable = 
  let tokens = eatIf (isSameToken (Keyword "return")) tokens
  let expressionTokens, tokens = advanceUntil (fun x -> x = (Symbol ';')) tokens false
  let expressionVm =
    match expressionTokens with 
    | [] -> ""
    | _ -> CompileExpression expressionTokens symbolTable 
  let tokens = eatIf (isSameToken (Symbol ';')) tokens
  let constructorVm =
    match subroutineKind with
    | Constructor -> ""
//{writePush POINTER 0}"
    | Function | Method -> ""
  let voidVm = 
    match subroutineIsVoid with
    | true -> "\n" + (writePush CONST 0)
    | false -> ""
  $"{expressionVm}{voidVm}{constructorVm}
{writeReturn()}
", tokens

let rec CompileStatements tokens funcIsVoid subroutineKind symbolTable = 
  let rec aux tokens xml =
    match tokens with
    | [] -> xml, tokens 
    | head::tail when head = Symbol '}' -> xml, tokens 
    | head::tail when head = (Keyword "let") -> 
      let letStatementXml, tokens = CompileLetStatement tokens symbolTable
      aux tokens (xml + letStatementXml)
    | head::tail when head = (Keyword "do") ->
      let doStatementXml, tokens = CompileDoStatement tokens symbolTable
      aux tokens (xml + doStatementXml)
    | head::tail when head = (Keyword "return") ->
      let returnStatementXml, tokens = CompileReturnStatement tokens funcIsVoid subroutineKind symbolTable
      aux tokens (xml + returnStatementXml)
    | head::tail when head = (Keyword "if") ->
      let ifStatementXml, tokens = CompileIfStatement tokens funcIsVoid subroutineKind symbolTable
      aux tokens (xml + ifStatementXml)
    | head::tail when head = (Keyword "while") ->
      let whileStatementXml, tokens = CompileWhileStatement tokens funcIsVoid subroutineKind symbolTable
      aux tokens (xml + whileStatementXml)
    | head::tail -> failwith ("Unexpected token in CompileStatements" + (string head))
  aux tokens ""

and CompileIfStatement tokens isVoidFunc subroutineKind symbolTable =
  let label1 = "IF_FALSE_" + (string ticker)
  ticker <- ticker + 1
  let label2 = "IF_TRUE_" + (string ticker)
  ticker <- ticker + 1
  let tokens = eatIf (isSameToken (Keyword "if")) tokens
  let conditionTokens, tokens = advanceUntilMatchingBracket (Symbol '(') (Symbol ')') tokens false
  let conditionVm = CompileExpression conditionTokens symbolTable
  let statementsTokens, tokens = advanceUntilMatchingBracket (Symbol '{') (Symbol '}') tokens false
  let conditionTrueStatementsVm, _ = CompileStatements statementsTokens isVoidFunc subroutineKind symbolTable
  match tokens with
  | head::tail when head = (Keyword "else") -> 
    let tokens = eatIf (isSameToken (Keyword "else")) tokens 
    let elseStatementsTokens, tokens =  advanceUntilMatchingBracket (Symbol '{') (Symbol '}') tokens false
    let conditionFalseStatementsVm, _ = CompileStatements elseStatementsTokens isVoidFunc subroutineKind symbolTable
    $"{conditionVm}
{writeArithmetic NOT}
{writeIf label1}
{conditionTrueStatementsVm}{writeGoto label2}
{writeLabel label1}
{conditionFalseStatementsVm}{writeLabel label2}" + "\n", tokens 
  | _ ->  
    $"{conditionVm}
{writeArithmetic NOT}
{writeIf label1}
{conditionTrueStatementsVm}{writeLabel label1}" + "\n", tokens 

and CompileWhileStatement tokens isVoidFunc subroutineKind symbolTable = 
  let label1 = "WHILE_TRUE_" + (string ticker)
  ticker <- ticker + 1
  let label2 = "WHILE_FALSE_" + (string ticker)
  ticker <- ticker + 1
  let tokens = eatIf (isSameToken (Keyword "while")) tokens
  let expressionTokens, tokens = advanceUntilMatchingBracket (Symbol '(') (Symbol ')') tokens false
  let statementsTokens, tokens = advanceUntilMatchingBracket (Symbol '{') (Symbol '}') tokens false
  let expressionVm = CompileExpression expressionTokens symbolTable
  let statementsVm, _ = CompileStatements statementsTokens isVoidFunc subroutineKind symbolTable
  $"{writeLabel label1}
{expressionVm}
{writeArithmetic NOT}
{writeIf label2}
{statementsVm}{writeGoto label1}
{writeLabel label2}
", tokens 

let CompileVarDecs tokens symbolTable =
  let mutable mutSymbolTable = symbolTable
  let mutable mutNOfLocals = 0
  match tokens with
  | [] -> "", [], mutNOfLocals, symbolTable
  | head::tail when head = Symbol '{' && tokens.Tail.Head = Symbol '}' -> "", [], mutNOfLocals, symbolTable
  | head::tail when head = Symbol '}' && tail = [] -> "", tokens, mutNOfLocals, symbolTable
  | _ -> 
    let patterns, remainingTokens = getConsecutivePatterns (fun x -> x = (Keyword "var")) (fun x -> x = (Symbol ';')) tokens
    let compileOne tokens = 
      let tokens = eatIf (isSameToken (Keyword "var")) tokens
      let typeToken, tokens = getNextTokenIf isTypeProgramStructure tokens
      let typeXml, typeName = 
        match typeToken with
        | Keyword k -> (tokenToXml typeToken), k
        | Identifier i -> (identifierToXml typeToken Class Use mutSymbolTable), i
        | _ -> failwith $"Expected a Keyword or an Identifier token, but received {typeToken}"
      let varNameToken, tokens = getNextTokenIf (isSameType (Identifier "_")) tokens
      let varName = 
        match varNameToken with 
        | Identifier i -> i
        | _ -> failwith ("WrongToken when variable name expected: " + (string varNameToken))
      mutSymbolTable <- SymbolTable.add varName typeName SymbolTable.Var mutSymbolTable
      mutNOfLocals <- mutNOfLocals + 1
      let identifierTokens = tokens |> List.filter (fun x -> match x with
                                                             | Identifier _ -> true
                                                             | _ -> false)
      for token in identifierTokens do
        let name = match token with | Identifier i -> i | _ -> failwith ("WrongToken when expecting only identifiers: " + (string token))
        mutSymbolTable <- SymbolTable.add name typeName SymbolTable.Var mutSymbolTable
        mutNOfLocals <- mutNOfLocals + 1
      let varNameXml = identifierToXml varNameToken InTable Definition mutSymbolTable
      let otherVarsXml =  
        tokens 
        |> List.map (fun x -> match x with
                              | Identifier _ -> identifierToXml x InTable Definition mutSymbolTable
                              | _ -> tokenToXml x)
        |> List.fold (fun x y -> x + "\n" + y) ""
      $$"""<varDec>
{{indent 1 "<keyword> var </keyword>"}}
{{indent 1  typeXml}}
{{indent 1 varNameXml}}{{indent 1 otherVarsXml}} 
</varDec>"""
    
    patterns |> List.map compileOne
             |> List.fold (fun x y -> x + "\n" + y) "", remainingTokens, mutNOfLocals,  mutSymbolTable



let CompileClassVarDecs tokens symbolTable = 
  let mutable mutSymbolTable = symbolTable
  let classVarDecPatterns, remainingTokens = getConsecutivePatterns (fun x -> x = (Keyword "static") || x = (Keyword "field")) (fun x -> x = (Symbol ';')) tokens
  let doOneDec listOfTokens =
    let staticOrField, listOfTokens = getNextTokenIf (isOneOfTokens [(Keyword "static"); (Keyword "field")]) listOfTokens 
    let typeToken, listOfTokens = getNextTokenIf isTypeProgramStructure listOfTokens
    let typeXml, typeName = 
      match typeToken with
      | Keyword k -> tokenToXml typeToken, k
      | Identifier i -> identifierToXml typeToken Class Use mutSymbolTable, i
      | _ -> failwith $"Expected a Keyword or an Identifier token, but received {typeToken}"
    let varNameToken, listOfTokens = getNextTokenIf  (isSameType (Identifier "_")) listOfTokens
    let varName = match varNameToken with | Identifier i -> i | _ -> failwith "Should not reach here"
    let varKind =
      match staticOrField with
      | Keyword "static" -> SymbolTable.Static
      | Keyword "field" -> SymbolTable.Field
      | _ -> failwith ("Incorrect token found where expected static or field: " + (string staticOrField))
    mutSymbolTable <- SymbolTable.add varName typeName varKind mutSymbolTable
    let varNameXml = identifierToXml varNameToken InTable Definition mutSymbolTable
    let identifierTokens = listOfTokens |> List.filter (fun x -> match x with 
                                                                 | Identifier _ -> true
                                                                 | _ -> false)
    for token in identifierTokens do
      let name = match token with | Identifier i -> i | _ -> failwith ("Wrong token found where identifier should be: " + (string token))
      mutSymbolTable <- SymbolTable.add name typeName varKind mutSymbolTable
    let otherVarsXml = listOfTokens |> List.map (fun x -> match x with
                                                          | Identifier _ -> identifierToXml x InTable Definition mutSymbolTable
                                                          | _ -> tokenToXml x)
                                    |> List.reduce (fun x y -> x + "\n" + y)
    $$"""<classVarDec>
{{indent 1 (tokenToXml staticOrField)}}
{{indent 1  typeXml}}
{{indent 1 varNameXml}}
{{indent 1 otherVarsXml}}
</classVarDec>"""
  match classVarDecPatterns with
  | [] -> "", remainingTokens, mutSymbolTable
  | _ ->
    let xml = classVarDecPatterns
              |> List.map doOneDec
              |> List.reduce (fun x y -> x + "\n" + y) 
    xml, remainingTokens, mutSymbolTable


let CompileParameterList tokens symbolTable = //SymbolTable.add varName typeName varKind mutSymbolTable
  let rec aux tokens count symbolTable = 
    match tokens with
    | [] -> symbolTable
    | head::tail when head = Symbol ',' -> aux tail count symbolTable
    | head::tail -> 
      let typeToken, tokens = getNextTokenIf isTypeProgramStructure tokens
      let varNameToken, tokens = getNextTokenIf (isSameType (Identifier "_")) tokens
      let typeName = 
        match typeToken with
        | Keyword k ->  k
        | Identifier i -> i
        | _ -> failwith $"Expecting a Keyword or an Identifier token, but received {typeToken}"
      let varName = getStringFromIdentifierToken varNameToken 
      let symbolTable = SymbolTable.add varName typeName SymbolTable.Arg symbolTable
      aux tokens (count + 1) symbolTable
  match tokens with 
  | [] -> symbolTable
  | _ -> aux tokens 0 symbolTable
        
let CompileSubroutineBody (tokens: Token list) subroutineIsVoid subroutineKind symbolTable = 
  let tokens = eatIf (isSameToken (Symbol '{')) tokens
  let varDecsXml, tokens, nOfVarDecs, symbolTable = CompileVarDecs tokens symbolTable
  let statementsXml, tokens = CompileStatements tokens subroutineIsVoid subroutineKind symbolTable
  let tokens = eatIf (isSameToken (Symbol '}')) tokens
  let symbolTable = SymbolTable.wipeSubroutineSymbols symbolTable
  statementsXml, nOfVarDecs, symbolTable
    

let CompileSubroutineDecs tokens symbolTable =
  let doOneSubroutineDec tokens symbolTable =
    let constructorFunctionMethod, ts = getNextTokenIf (isOneOfTokens [Keyword "constructor"; Keyword "function"; Keyword "method"]) tokens
    let subroutineKind = 
      match constructorFunctionMethod with
      | Keyword "constructor" -> Constructor
      | Keyword "method" -> Method
      | Keyword "function" -> Function
      | _ -> failwith ("Expected a token indicating the subroutine kind here. Instead got: " + (string constructorFunctionMethod))
    let voidOrTypeToken, ts = getNextTokenIf isTypeOrVoid ts
    let subroutineIsVoid = 
      match voidOrTypeToken with
      | Keyword "void" -> true
      | Keyword _ | Identifier _ -> false
      | _ -> failwith ("Unexpected token when expecting void or a type: " + (string voidOrTypeToken))
    let subroutineNameToken, ts = getNextTokenIf (isSameType (Identifier "_")) ts
    let subroutineName = CLASS_NAME + "." + (getStringFromIdentifierToken subroutineNameToken)
    let symbolTable =
      match subroutineKind with
      | Method -> SymbolTable.add "this" CLASS_NAME SymbolTable.Arg symbolTable
      | Function | Constructor -> symbolTable
   // let subroutineNameXml = identifierToXml subroutineNameToken Subroutine Definition symbolTable
    let ts = eatIf (isSameToken (Symbol '(')) ts
    let parameterTokens, ts = advanceUntil (fun x -> x = (Symbol ')')) ts false
    let symbolTable = CompileParameterList parameterTokens symbolTable 
    let ts = eatIf (isSameToken (Symbol ')')) ts
    let subroutineBodyTokens, ts = advanceUntilMatchingBracket (Symbol '{') (Symbol '}') ts true
    let subroutineBodyVm, nOfLocals, symbolTable = CompileSubroutineBody subroutineBodyTokens subroutineIsVoid subroutineKind symbolTable
    let subroutineExtraVm = 
      match subroutineKind with
      | Constructor -> $"""
{writePush CONST (SymbolTable.varCount SymbolTable.Field symbolTable)}
{writeCall "Memory.alloc" 1}
{writePop POINTER 0}"""
      | Method -> $"
{writePush ARG 0}
{writePop POINTER 0}"
      | Function -> ""
    let vm = $"{(writeFunction subroutineName nOfLocals)}{subroutineExtraVm}
{subroutineBodyVm}"
    vm, ts, symbolTable
  let rec aux remainingTokens xml symbolTable =
    match remainingTokens with
    | head::tail when head = Keyword "constructor" || head = Keyword "function" || head = Keyword "method" -> 
      let oneDecXml, remainingTokens, symbolTable = doOneSubroutineDec remainingTokens symbolTable
      let xml = xml + oneDecXml
      aux remainingTokens xml symbolTable
    | _ -> xml, remainingTokens, symbolTable
  aux tokens "" symbolTable


let CompileClass tokens =
 let symbolTable = SymbolTable.create()
 let tokens = eatIf (isSameToken (Keyword "class")) tokens
 let classNameToken, tokens = getNextTokenIf ((isSameType (Identifier "_"))) tokens
 CLASS_NAME <- getStringFromIdentifierToken classNameToken
 let tokens = eatIf (isSameToken (Symbol '{')) tokens
 let classVarDecs, tokens, symbolTable = CompileClassVarDecs tokens symbolTable
 let classSubroutineDecs, tokens, symbolTable = CompileSubroutineDecs tokens symbolTable
 let tokens = eatIf (isSameToken (Symbol '}')) tokens
 classSubroutineDecs

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
  Identifier "b";
  Symbol ','; 
  Identifier "test2";
  Symbol ';';
  Keyword "static";
  Keyword "int";
  Identifier "i";
  Symbol ',';
  Identifier "p";
  Symbol ';'
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
let varDecsTest2 = [Keyword "var"; Keyword "int"; Identifier "i"; Symbol ','; Identifier "j"; Symbol ';']
let letTest = [Keyword "let"; Identifier "x"; Symbol '='; IntConstant 2; Symbol '+'; IntConstant 3; Symbol ';']
let letTest2 = [Keyword "let"; Identifier "foo"; Symbol '['; IntConstant 5; Symbol '-'; IntConstant 2; Symbol ']'; Symbol '='; StringConstant "dog"; Symbol ';']
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
let subroutineBodyTest = List.concat [[Symbol '{'; Keyword "var"; Keyword "int"; Identifier "i"; Symbol ','; Identifier "x"; Symbol ','; Identifier "j"; Symbol ';'; 
  Keyword "var"; Identifier "String"; Identifier "s"; Symbol ';';]; statementsTest; [Symbol '}']]
let subroutineDecsTest = List.concat[
  [Keyword "function";
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
  Symbol ')';];
  subroutineBodyTest;
 [Keyword "constructor";
  Keyword "int";
  Identifier "main";
  Symbol '(';
  Symbol ')';
  Symbol '{';
  Symbol '}']]
let ifStatementTest = [Keyword "if"; Symbol '('; Symbol '('; IntConstant 1; Symbol '+'; IntConstant 3; Symbol ')'; Symbol '='; 
  IntConstant 4; Symbol ')'; Symbol '{'; Keyword "do"; Identifier "someFunction"; Symbol '('; Identifier "i"; Symbol ')'; Symbol ';'; Symbol '}';
  Keyword "else"; Symbol '{'; Keyword "do"; Identifier "someOtherFunction"; Symbol '('; IntConstant 69; Symbol ')'; Symbol ';'; Symbol '}';
  Identifier "No"; Identifier "Surprises"; Identifier "Please"]
let ifStatementTest2 = [Keyword "if"; Symbol '('; Symbol '('; IntConstant 1; Symbol '+'; IntConstant 3; Symbol ')'; Symbol '='; 
  IntConstant 4; Symbol ')'; Symbol '{'; Keyword "do"; Identifier "someFunction"; Symbol '('; StringConstant "dog"; Symbol ')'; Symbol ';'; Symbol '}';
  Identifier "No"; Identifier "Surprises"; Identifier "Please"]
//printfn "%A" (getTokensBeforeOp beforeOpTest)
//printfn "%A" (advanceUntilMatchingBracket (Symbol '(') (Symbol ')') [Symbol '('; Symbol '('; IntConstant 1; Symbol '+'; IntConstant 3; Symbol ')'; 
 // Symbol '='; IntConstant 4; Symbol ')' ])
let whileStatementTest = [Keyword "while"; Symbol '('; Identifier "i"; Symbol '='; IntConstant 2; Symbol ')'; Symbol '{';
  Keyword "let"; Identifier "x"; Symbol '='; IntConstant 2; Symbol '+'; IntConstant 3; Symbol ';';
  Keyword "let"; Identifier "x"; Symbol '['; IntConstant 5; Symbol '-'; IntConstant 2; Symbol ']'; Symbol '='; StringConstant "dog"; Symbol ';';
  Symbol '}'; Identifier "No"; Identifier "Surprises"; Identifier "Please"]
let expressionListTest = [IntConstant 4; Symbol '+'; IntConstant 2; Symbol ','; StringConstant "dog"; Symbol '+'; StringConstant "cat"; Symbol ','; Identifier "i"]

let classTest = List.concat [[Keyword "class"; Identifier "Point"; Symbol '{'] ; classVarDecsTest; subroutineDecsTest; [Symbol '}']]

let emptyBracketsTest = [Symbol '{'; Symbol '}']
let st = SymbolTable.add "HEIGHT" "int" SymbolTable.Static ( SymbolTable.add "b" "boolean" SymbolTable.Arg (SymbolTable.add "p" "Point" SymbolTable.Arg (SymbolTable.add "x" "int" SymbolTable.Arg (SymbolTable.add "i" "int" SymbolTable.Var (SymbolTable.add "foo" "string" SymbolTable.Var (SymbolTable.create()))))))
//printfn "%A" (CompileExpression expressionTest2 st)
//printfn ""
//printfn "%A" (CompileTerm [Identifier "foo"] st)
//printfn ""
//printfn "%A" (CompileTerm [Identifier "foo"; Symbol '['; IntConstant 5; Symbol ']']  st)
//printfn ""
//printfn "%A" (CompileTerm [Identifier "bar"; Symbol '('; IntConstant 3; Symbol ')'] st)
//printfn ""
//printfn "%A" (CompileTerm [Identifier "foo"; Symbol '.'; Identifier "FunkName"; Symbol '('; IntConstant 3; Symbol ','; StringConstant "slop"; Symbol ')']  st)
//printfn ""
//printfn "%A" (CompileTerm [Identifier "notInTableHA"; Symbol '.'; Identifier "FunkName"; Symbol '('; IntConstant 3; Symbol ','; StringConstant "slop"; Symbol ')']  st)
//printfn ""
//printfn "%A" (CompileLetStatement letTest2 st)
//printfn ""
//printfn "%A" (CompileDoStatement doTest  st)
//printfn ""
//printfn "%A" (CompileReturnStatement returnTest false st)
//printfn ""
//printfn "%A" (CompileIfStatement ifStatementTest false 2 st)
//printfn ""
//printfn "%A" (CompileIfStatement ifStatementTest2 false 2 st)
//printfn ""
////printfn "%A" (CompileWhileStatement whileStatementTest 0 st)
//printfn ""
////printfn "%A" (CompileStatements statementsTest 0 st)
//printfn ""
//printfn "%A" (CompileVarDecs varDecsTest2 (SymbolTable.create())) 
//printfn ""
//printfn "%A" (CompileVarDecs varDecsTest st)
//printfn ""
//printfn "%A" (CompileClassVarDecs classVarDecsTest st)
//printfn ""
//printfn "%A" (CompileParameterList paramTest (SymbolTable.create()))
//printfn ""
////printfn "%A" (CompileSubroutineBody subroutineBodyTest 4 st)
//printfn ""
//printfn "%A" (CompileSubroutineDecs subroutineDecsTest  3 st)
//printfn ""
//printfn "%A" (CompileClass classTest)
//printfn ""
//printfn "%A" (CompileExpressionList expressionListTest st)
//printfn ""
//printfn "%A" (CompileExpressionList [IntConstant 8001; Symbol ','; IntConstant 16; Symbol ','; IntConstant -1] st)
//printfn ""
//printfn "%A" (CompileExpressionList [IntConstant 2] st)
printfn ""
printfn "%A" (CompileExpression [IntConstant 2; Symbol '+'; IntConstant 5; Symbol '+'; IntConstant 1] st)
printfn ""
printfn "%A" (CompileTerm [IntConstant 2] st)
printfn ""
printfn "%A" (CompileTerm [IntConstant 5] st)
printfn ""
printfn "%A" (CompileExpression [IntConstant 3; Symbol '+'; IntConstant 4; Symbol '-'; IntConstant 2] st)
printfn ""
printfn "%A" (CompileExpression [Symbol '('; Symbol '-'; IntConstant 1; Symbol ')'] st)
printfn ""
printfn "%A" (CompileExpression [Symbol '('; Symbol '('; IntConstant 2; Symbol '+'; IntConstant 1; Symbol ')'; Symbol '+'; IntConstant 1; Symbol ')'; Symbol '-'; IntConstant 1] st)
printfn ""
printfn "%A" (CompileTerm [Symbol '('; Symbol '-'; IntConstant 1; Symbol ')'] st)
printfn ""
printfn "%A" (CompileTerm [Symbol '-'; IntConstant 1] st)
printfn ""
printfn "%A" (CompileExpression [ Symbol '('; IntConstant 69; Symbol '+'; IntConstant 33;  Symbol ')'; Symbol '+'; IntConstant 42] st)
printfn ""
printfn "%A" (CompileExpressionList [IntConstant 33; Symbol ','; IntConstant 22; Symbol '+'; IntConstant 39; Symbol ','; Symbol '('; IntConstant 2; Symbol '+'; IntConstant 1; Symbol ')'] st)
printfn ""
printfn "%A" (CompileExpression [Symbol '-'; IntConstant 1; Symbol '+'; IntConstant 3] st)
printfn ""
//printfn "%A" (CompileLetStatement [Keyword "let"; Identifier "x"; Symbol '='; Identifier "Main"; Symbol '.'; Identifier "peek"; Symbol '('; IntConstant 8000; Symbol ')'; Symbol ';'] st)
printfn "%A" (CompileExpression [ Identifier "Main"; Symbol '.'; Identifier "peek"; Symbol '('; IntConstant 8000; Symbol ')'] st)
(*
printfn "%A" (CompileClass classTest)
printfn ""
printfn "%A" (CompileSubroutineDecs subroutineDecsTest)
//printfn "%A" (getConsecutivePatterns (fun x -> false) (fun x -> false) [Symbol ';'; Identifier "plop"]) 
printfn ""
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
*)
