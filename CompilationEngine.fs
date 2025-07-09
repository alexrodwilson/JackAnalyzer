module CompilationEngine
open Tokenizer


type KeywordOrIdentifier =
  | K of Keyword
  | I of Identifier

type ClassVarDec = 
  {
  StaticOrField: Keyword;
  Type: KeywordOrIdentifier;
  VarName: Identifier;
  OtherVarNames: Identifier list;
  }

type Class = 
  {
  ClassName : Identifier;
  ClassVarDecs: ClassVarDec list;
  SubroutineDecs: SubroutineDec list;
  }

and Parameter = 
  {
  Type: KeywordOrIdentifier;
  VarName: Identifier;
  }

and VarDec = 
  {
  Type: KeywordOrIdentifier;
  VarName: Identifier;
  OtherVarNames: Identifier list;
  }

and SubroutineBody = 
  {
  VarDecs: VarDec list;
  Statements: Statement list;
  } 
  
and SubroutineDec =
  {
  ConstructorFunctionOrMethod: Keyword;
  ReturnType: KeywordOrIdentifier;
  SubroutineName: Identifier;
  ParameterList: Parameter list;
  SubroutineBody: SubroutineBody;
  }

and LetStatement = 
  {
  VarName: Identifier;
  ArrayIndexExpression: Expression option;
  EqualsExpression: Expression;
  }

and IfStatement =
  {
  Condition: Expression;
  TrueStatements: Statement list;
  FalseStatements: (Statement list) option;
  }

and WhileStatement = 
  {
  Condition: Expression;
  Statements: Statement list;
  }

and DoStatement = 
  {
  SubroutineCall: SubroutineCall;
  }

and ReturnStatement = 
  {
  Expression: Expression option; 
  }

and Statement = 
  | LetStatement of LetStatement
  | WhileStatement of WhileStatement
  | DoStatement of DoStatement
  | ReturnStatement of ReturnStatement

and Expression =
  {
  Term: Term;
  OpsAndTerms: OpAndTerm list; 
  }

and Term = 
  | IntConst of IntConstant
  | StringConst of StringConstant
  | TrueFalseNullThis of Keyword
  | VarName of Identifier
  | ArrayExpression of ArrayExpression
  | BracketedExpression of Expression
  | UnaryOpAndTerm of OpAndTerm
  | SubroutineCall of SubroutineCall

and ArrayExpression =
  {
  VarName: Identifier;
  IndexExpression: Expression;
  }

and OpAndTerm =
  {
  UnaryOp: Symbol;
  Term: Term;
  }

and MethodCall =
  {
  MethodName: Identifier;
  ExpressionList: Expression list
  }

and FunctionCall = 
  {
  ClassOrVariableName: Identifier;
  FunctionName: Identifier;
  ExpressionList: Expression list;
  }

and SubroutineCall =
  | FunctionCall of FunctionCall
  | MethodCall of MethodCall

let deconstruct token = 
  match token with 
  | Symbol ch -> string ch
  | Identifier i -> i
  | Keyword k -> k
  | StringConstant s -> s
  | IntConstant i -> string i



let eat (expectedToken: Token)  (tokens: Token list)  =
    if tokens.Head = expectedToken then
      tokens.Tail
    else (failwith "bad token" ); tokens.Tail
  
let nextTokenAndState (tokens: Token list): (string * Token list) =
   (deconstruct tokens.Head, tokens.Tail)
(*
let CompileClassVarDecs tokens : (ClassVarDec list * Token list) = 
  ([], tokens)

let CompileSubroutineDecs tokens = 
  ([], tokens)

let CompileClass tokens = 
  let tokens  = eat (Keyword "class") tokens
  let className, tokens = nextTokenAndState tokens
  let tokens = eat (Symbol '{') tokens
  let classVarDecs, tokens = CompileClassVarDecs tokens
  let subroutineDecs, tokens = CompileSubroutineDecs tokens
  let tokens = eat (Symbol '}') tokens
  {  
  ClassName = className;
  ClassVarDecs = classVarDecs;
  SubroutineDecs = subroutineDecs;
  }
  *)
let CompileClassVarDecs tokens = 
  ("sausage", tokens)

let CompileSubroutineDecs tokens =
  ("more sausages", tokens)

let CompileClass tokens =
  let tokens = eat (Keyword "class") tokens
  let (className, tokens) = nextTokenAndState tokens
  let tokens = eat (Symbol '{') tokens
  let (classVarDec, tokens) = CompileClassVarDecs tokens
  let (subroutineDec, tokens) = CompileSubroutineDecs tokens
  let tokens = eat (Symbol '}') tokens
  $$"""<class>
  <keyword> class </keyword>
  <identifier> {{(string className)}} </identifier>
  <symbol> { </symbol>
  {{classVarDec}}
  {{subroutineDec}}
</class>"""




  






