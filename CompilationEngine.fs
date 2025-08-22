module CompilationEngine
open Tokenizer
open VMWriter

let SPACES_PER_INDENT = 2
let mutable ticker = 0
let mutable CLASS_NAME = ""

type Category = Class | Subroutine | InTable
type Role = Definition | Use
type SubroutineKind = Function | Method | Constructor 

let removeBlankLines (s: string) = 
  s.Split ('\n') |> Seq.filter (fun x -> x <> "")
                 |> String.concat "\n" 

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
    | [] ->  failwith ("No matching bracket found")
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

let isOp token = 
  let ops = ['+'; '-'; '*'; '/'; '&'; '|'; '<'; '>';'=']
  match token with
  | Symbol s when (List.contains s ops) -> true
  | _ -> false

let stringConstantToVm s =
 let stringLength = String.length s
 let loopVm = s 
              |> Seq.map (fun x -> (int x)) 
              |> Seq.map (fun x -> 
              $$"""{{writePush CONST x}}
{{writeCall "String.appendChar" 2}}""")
              |> Seq.reduce (fun x y -> x + "\n" + y) 
 $$"""{{writePush CONST stringLength}}
{{writeCall "String.new" 1}}
{{loopVm}}"""


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
  let innerVm, leftOverTokens =
    match tokens.Head with
    | IntConstant i ->  writePush CONST i, tokens.Tail
    | StringConstant s ->  (stringConstantToVm s), tokens.Tail
    | Keyword k when (List.contains k ["true"; "false"; "null"; "this"]) -> 
      match k with 
      | "this" -> writePush POINTER 0, tokens.Tail 
      | "false" | "null" -> writePush CONST 0, tokens.Tail
      | "true" -> $"{writePush CONST 1}
{writeArithmetic NEG}", tokens.Tail
      | _ -> failwith ("Expecting keywords this, false or true but got: " + k)
    | Symbol s when s = '(' -> 
      let expressionTokens, remainingTokens = advanceUntilMatchingBracket (Symbol '(') (Symbol ')') tokens false 
      let expressionVM = CompileExpression expressionTokens symbolTable
      expressionVM, remainingTokens      
    | Symbol s when (s = '-' || s = '~') -> 
      let unaryOpVm = 
       match s with
        | '-' -> writeArithmetic NEG 
        | '~' -> writeArithmetic NOT
        | _ -> failwith ("Expected unary operators - or ~ here but got: " + (string s))
      let termVm, remainingTokens = CompileTerm tokens.Tail symbolTable
      (termVm + "\n" + unaryOpVm), remainingTokens
    | Identifier i when tokens.Tail = [] ->  identifierToVm tokens.Head symbolTable, []
    | Identifier i when tokens.Tail.Head = (Symbol '[') ->
      let arrayNameVm = identifierToVm tokens.Head symbolTable
      let expressionTokens, remainingTokens = advanceUntilMatchingBracket (Symbol '[') (Symbol ']') tokens.Tail false
      let expressionVm = CompileExpression expressionTokens symbolTable
      $"{arrayNameVm}
{expressionVm}
{writeArithmetic ADD}
{writePop POINTER 1}
{writePush THAT 0}", remainingTokens
    | Identifier i when tokens.Tail.Head = (Symbol '(') ->
      let methodNameToken, tokens = getNextTokenIf (isSameType (Identifier "_")) tokens
      let fullMethodName = CLASS_NAME + "." + (getStringFromIdentifierToken methodNameToken)  
      let expressionListTokens, tokens = advanceUntilMatchingBracket (Symbol '(') (Symbol ')') tokens false
      let expressionListVm, nOfArgs = CompileExpressionList expressionListTokens symbolTable
      $"{writePush POINTER 0}
{expressionListVm}{writeCall fullMethodName (nOfArgs + 1)}", tokens
    | Identifier classOrVarName when tokens.Tail.Head = (Symbol '.') ->
      let classOrVarNameToken, tokens = getNextTokenIf (isSameType (Identifier "_")) tokens
      let tokens = eatIf (isSameToken (Symbol '.')) tokens
      let subroutineNameToken, tokens = getNextTokenIf (isSameType (Identifier "_")) tokens
      let subroutineName = getStringFromIdentifierToken subroutineNameToken
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
  innerVm, leftOverTokens

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
    | _ -> failwith ("Expected an operator Symbol but got: " + (string token))
  let rec aux tokens vm =
    match tokens with
    | [] -> vm
    | head :: tail -> 
      let op = opToVm head
      let term, tokens = CompileTerm tail symbolTable
      let vm = vm + $"{term}
{op}
"  
      aux tokens vm
  
  match tokens with
    | [] -> ""
    | head :: tail  -> 
      let firstTerm, tokens = CompileTerm tokens symbolTable
      match tokens with 
      | [] -> firstTerm
      | head :: tail -> 
         (aux tokens (firstTerm + "\n"))


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
  | "" -> "", count 
  | _ -> "\n" + vm, count
  
let CompileLetStatement tokens symbolTable = 
  let tokens = eatIf (isSameToken (Keyword "let")) tokens
  let varNameToken, tokens = getNextTokenIf (isSameType (Identifier "_")) tokens
  let arrayNameVm = identifierToVm varNameToken symbolTable
  match tokens.Head with
  | Symbol s when s = '[' ->
    let expressionTokens, tokens = advanceUntilMatchingBracket (Symbol '[') (Symbol ']') tokens false
    let arrayExpressionVm = CompileExpression expressionTokens symbolTable
    let tokens = eatIf (isSameToken (Symbol '=')) tokens
    let rhsExpressionTokens, remainingTokens = advanceUntil (fun x -> x = (Symbol ';')) tokens false
    let rhsExpressionVm = CompileExpression rhsExpressionTokens symbolTable
    let remainingTokens = eatIf (isSameToken (Symbol ';')) remainingTokens
    $"{arrayNameVm}
{arrayExpressionVm}
{writeArithmetic ADD}
{rhsExpressionVm}
{writePop TEMP 0}
{writePop POINTER 1}
{writePush TEMP 0}
{writePop THAT 0}", remainingTokens
  | Symbol s when s = '=' ->
    let tokens = eatIf (isSameToken (Symbol '=')) tokens
    let rhsExpressionTokens, remainingTokens = advanceUntil (fun x -> x = (Symbol ';')) tokens false
    let rhsExpressionVm = CompileExpression rhsExpressionTokens symbolTable
    let remainingTokens = eatIf (isSameToken (Symbol ';')) remainingTokens
    let segment, index = getSegmentAndIndex varNameToken  symbolTable
    $"{rhsExpressionVm}
{writePop segment index}", remainingTokens
  | _ -> failwith("Unexpected token found when compiling let statement, expecting '[' or '=': " + (string tokens.Head))

let CompileDoStatement tokens symbolTable =
  let tokens = eatIf (isSameToken (Keyword "do")) tokens
  let subroutineCallTokens, tokens = advanceUntil (fun x -> x = (Symbol ';')) tokens false
  let subroutineCallVm, notNeeded = CompileTerm subroutineCallTokens symbolTable
  let tokens = eatIf (isSameToken (Symbol ';')) tokens
  $"{subroutineCallVm}
{writePop TEMP 0}", tokens

let CompileReturnStatement tokens subroutineIsVoid subroutineKind symbolTable = 
  let tokens = eatIf (isSameToken (Keyword "return")) tokens
  let expressionTokens, tokens = advanceUntil (fun x -> x = (Symbol ';')) tokens false
  let expressionVm =
    match expressionTokens with 
    | [] -> ""
    | _ -> CompileExpression expressionTokens symbolTable 
  let expressionVmFinal = 
    match expressionVm with
    | "" -> ""
    | _ -> expressionVm + "\n" 
  let tokens = eatIf (isSameToken (Symbol ';')) tokens
  let voidVm = 
    match subroutineIsVoid with
    | true -> (writePush CONST 0) + "\n"
    | false -> ""
  $"{expressionVmFinal}{voidVm}{writeReturn()}", tokens

let rec CompileStatements tokens funcIsVoid subroutineKind symbolTable = 
  let rec aux tokens vm =
    match tokens with
    | [] -> vm, tokens 
    | head::tail when head = Symbol '}' -> vm, tokens 
    | head::tail when head = (Keyword "let") -> 
      let letStatementVm, tokens = CompileLetStatement tokens symbolTable
      aux tokens (vm + "\n" + letStatementVm)
    | head::tail when head = (Keyword "do") ->
      let doStatementVm, tokens = CompileDoStatement tokens symbolTable
      aux tokens (vm + "\n" + doStatementVm)
    | head::tail when head = (Keyword "return") ->
      let returnStatementVm, tokens = CompileReturnStatement tokens funcIsVoid subroutineKind symbolTable
      aux tokens (vm + "\n" + returnStatementVm)
    | head::tail when head = (Keyword "if") ->
      let ifStatementVm, tokens = CompileIfStatement tokens funcIsVoid subroutineKind symbolTable
      aux tokens (vm + "\n" + ifStatementVm)
    | head::tail when head = (Keyword "while") ->
      let whileStatementVm, tokens = CompileWhileStatement tokens funcIsVoid subroutineKind symbolTable
      aux tokens (vm + "\n" + whileStatementVm)
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
{conditionTrueStatementsVm}
{writeGoto label2}
{writeLabel label1}
{conditionFalseStatementsVm}
{writeLabel label2}", tokens 
  | _ ->  
    $"{conditionVm}
{writeArithmetic NOT}
{writeIf label1}
{conditionTrueStatementsVm}
{writeLabel label1}", tokens 

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
{statementsVm}
{writeGoto label1}
{writeLabel label2}", tokens 

let CompileVarDecs tokens symbolTable =
  let mutable mutSymbolTable = symbolTable
  let mutable mutNOfLocals = 0
  match tokens with
  | [] -> [], mutNOfLocals, symbolTable
  | head::tail when head = Symbol '{' && tokens.Tail.Head = Symbol '}' -> [], mutNOfLocals, symbolTable
  | head::tail when head = Symbol '}' && tail = [] -> tokens, mutNOfLocals, symbolTable
  | _ -> 
    let patterns, remainingTokens = getConsecutivePatterns (fun x -> x = (Keyword "var")) (fun x -> x = (Symbol ';')) tokens
    let compileOne tokens = 
      let tokens = eatIf (isSameToken (Keyword "var")) tokens
      let typeToken, tokens = getNextTokenIf isTypeProgramStructure tokens
      let typeName = 
        match typeToken with
        | Keyword k ->  k
        | Identifier i -> i
        | _ -> failwith $"Expected a Keyword or an Identifier token, but received {typeToken}"
      let varNameToken, tokens = getNextTokenIf (isSameType (Identifier "_")) tokens
      let varName = getStringFromIdentifierToken varNameToken 
      mutSymbolTable <- SymbolTable.add varName typeName SymbolTable.Var mutSymbolTable
      mutNOfLocals <- mutNOfLocals + 1
      let identifierTokens = tokens |> List.filter (fun x -> match x with
                                                             | Identifier _ -> true
                                                             | _ -> false)
      for token in identifierTokens do
        let name = getStringFromIdentifierToken token 
        mutSymbolTable <- SymbolTable.add name typeName SymbolTable.Var mutSymbolTable
        mutNOfLocals <- mutNOfLocals + 1
    for pattern in patterns do
      compileOne pattern
    remainingTokens, mutNOfLocals,  mutSymbolTable


let CompileClassVarDecs tokens symbolTable = 
  let mutable mutSymbolTable = symbolTable
  let classVarDecPatterns, remainingTokens = getConsecutivePatterns (fun x -> x = (Keyword "static") || x = (Keyword "field")) (fun x -> x = (Symbol ';')) tokens
  let doOneDec listOfTokens =
    let staticOrField, listOfTokens = getNextTokenIf (isOneOfTokens [(Keyword "static"); (Keyword "field")]) listOfTokens 
    let typeToken, listOfTokens = getNextTokenIf isTypeProgramStructure listOfTokens
    let typeName = 
      match typeToken with
      | Keyword k ->  k
      | Identifier i -> i
      | _ -> failwith $"Expected a Keyword or an Identifier token, but received {typeToken}"
    let varNameToken, listOfTokens = getNextTokenIf  (isSameType (Identifier "_")) listOfTokens
    let varName = getStringFromIdentifierToken varNameToken 
    let varKind =
      match staticOrField with
      | Keyword "static" -> SymbolTable.Static
      | Keyword "field" -> SymbolTable.Field
      | _ -> failwith ("Incorrect token found where expected static or field: " + (string staticOrField))
    mutSymbolTable <- SymbolTable.add varName typeName varKind mutSymbolTable
    let identifierTokens = listOfTokens |> List.filter (fun x -> match x with 
                                                                 | Identifier _ -> true
                                                                 | _ -> false)
    for token in identifierTokens do
      let name = getStringFromIdentifierToken token 
      mutSymbolTable <- SymbolTable.add name typeName varKind mutSymbolTable
  match classVarDecPatterns with
  | [] -> remainingTokens, mutSymbolTable
  | _ ->
    for pattern in classVarDecPatterns do
      doOneDec pattern
    remainingTokens, mutSymbolTable


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
  let tokens, nOfVarDecs, symbolTable = CompileVarDecs tokens symbolTable
  let statementsVm, tokens = CompileStatements tokens subroutineIsVoid subroutineKind symbolTable
  let tokens = eatIf (isSameToken (Symbol '}')) tokens
  let symbolTable = SymbolTable.wipeSubroutineSymbols symbolTable
  statementsVm, nOfVarDecs, symbolTable
    
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
  let rec aux remainingTokens vm symbolTable =
    match remainingTokens with
    | head::tail when head = Keyword "constructor" || head = Keyword "function" || head = Keyword "method" -> 
      let oneDecVm, remainingTokens, symbolTable = doOneSubroutineDec remainingTokens symbolTable
      let vm = vm + "\n" + oneDecVm
      aux remainingTokens vm symbolTable
    | _ -> vm, remainingTokens, symbolTable
  aux tokens "" symbolTable


let CompileClass tokens =
 let symbolTable = SymbolTable.create()
 let tokens = eatIf (isSameToken (Keyword "class")) tokens
 let classNameToken, tokens = getNextTokenIf ((isSameType (Identifier "_"))) tokens
 CLASS_NAME <- getStringFromIdentifierToken classNameToken
 let tokens = eatIf (isSameToken (Symbol '{')) tokens
 let tokens, symbolTable = CompileClassVarDecs tokens symbolTable
 let classSubroutineDecs, tokens, symbolTable = CompileSubroutineDecs tokens symbolTable
 let tokens = eatIf (isSameToken (Symbol '}')) tokens
 removeBlankLines classSubroutineDecs


