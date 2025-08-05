type Kind = 
  | Static
  | Field
  | Arg
  | Var
  
type SymbolInfo =
  { T: string; Kind: Kind; Index: int}

type SymbolTable =
  { ClassSymbols: Map<string, SymbolInfo>; 
    SubroutineSymbols: Map<string, SymbolInfo>
  }

let create () =
  { ClassSymbols = Map.empty;
    SubroutineSymbols = Map.empty
  }

let varCount kind st =
  0

let define name t kind st =
  let nOfKind = 1 + (varCount kind st)
  match kind with
  | Static | Field -> {st with ClassSymbols = (Map.add name {T = t; Kind = kind; Index = nOfKind} st.ClassSymbols)}
  | Arg | Var -> {st with SubroutineSymbols = (Map.add name {T = t; Kind = kind; Index = nOfKind} st.SubroutineSymbols)}



let st = create ()
printfn "%A" (define "foo" "int" Field st)
let st1 = create ()
printfn "%A" (define "bar" "int" Arg st1)
