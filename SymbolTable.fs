module SymbolTable

type Kind = 
  | Static
  | Field
  | Arg
  | Var
  | None
  
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
  let correctTable = 
   match kind with
   | Static | Field -> st.ClassSymbols
   | Arg | Var -> st.SubroutineSymbols
  correctTable
  |> Map.filter (fun _ v -> v.Kind = kind)
  |> Map.count

let add name t kind st =
  let nOfKind = varCount kind st
  match kind with
  | Static | Field -> {st with ClassSymbols = (Map.add name {T = t; Kind = kind; Index = nOfKind} st.ClassSymbols)}
  | Arg | Var -> {st with SubroutineSymbols = (Map.add name {T = t; Kind = kind; Index = nOfKind} st.SubroutineSymbols)}

let wipeSymbolTable st = 
  {st with SubroutineSymbols = Map.empty}

let functionOf name f st = 
  match name with
  | _ when st.SubroutineSymbols.ContainsKey name -> (st.SubroutineSymbols[name]) |> f
  | _ when st.ClassSymbols.ContainsKey name -> (st.ClassSymbols[name]) |> f
  | _ -> failwith ("SymbolTable does not contain the key: " + name)

let kindOf name st =
  match name with 
  | _ when st.SubroutineSymbols.ContainsKey name -> (st.SubroutineSymbols[name]).Kind
  | _ when st.ClassSymbols.ContainsKey name -> (st.ClassSymbols[name]).Kind
  | _ -> Kind.None

let typeOf name st = 
  functionOf name (fun x -> x.T) st

let indexOf name st = 
  functionOf name (fun x -> x.Index) st
(*
let st = create ()
let st1 = add "foo" "int" Field st
printfn "%A" st1
printfn ""
let st2 = add "bar" "int" Arg st1
printfn "%A" st2
printfn ""
let st3 = add "foo2" "string" Arg st2
printfn "%A" st3
printfn ""
let st4 = add "bar2" "Point" Static st3
printfn "%A" st4
printfn ""
let st5 = add "bar3" "Point" Static st4
printfn "%A" st5
printfn ""
let st6 = wipeSymbolTable st5
printfn "%A" st6
printfn ""
printfn "%A" (kindOf "foo" st6)
printfn ""
printfn "%A" (kindOf "bar" st5)
printfn ""
printfn "%A" (typeOf "foo2" st5)
printfn ""
printfn "%A" (typeOf "bar2" st5)
printfn ""
printfn "%A" (indexOf "bar2" st5)
printfn ""
printfn "%A" (indexOf "bar3" st5)
printfn ""
*)


