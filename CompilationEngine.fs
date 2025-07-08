module CompilationEngine
open Tokenizer

type KeywordOrIdentifier =
  | Keyword of Keyword
  | Identifier of Identifier

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
  | IntConstant of IntConstant
  | StringConstant of StringConstant
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


  






