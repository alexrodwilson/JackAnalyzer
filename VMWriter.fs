module VMWriter

type  Segment = CONST | ARG | LOCAL | STATIC | THIS | THAT | POINTER | TEMP

type  Command = ADD | SUB | NEG | EQ | GT | LT | AND | OR | NOT

let SPACES_PER_INDENT = 9

let vmIndent s =
  let spaces = String.replicate SPACES_PER_INDENT " " 
  spaces + s

let segmentToString segment = 
  match segment with
  CONST | ARG | LOCAL | STATIC | THIS | THAT | POINTER | TEMP ->  (string segment).ToLower()

let commandToString  command = 
  match command  with
  | ADD | SUB | NEG | EQ | GT | LT | AND | OR | NOT ->  (string command).ToLower()

let writePush segment index = 
 vmIndent  $"push {segmentToString segment} {index}"

let writePop segment index = 
 match segment with
 | CONST -> failwith "Error: You cannot pop to the CONST segment"
 | _ -> vmIndent  $"pop {segmentToString segment} {index}"

let writeArithmetic command =
  vmIndent (commandToString command)

let writeLabel label  = 
  $"label {label}"

let writeGoto label =
  vmIndent $"goto {label}"

let writeIf label =
  vmIndent $"if-goto {label}"

let writeCall name nArgs =
  vmIndent $"call {name} {nArgs}"

let writeFunction name nVars = 
  $"function {name} {nVars}"

let writeReturn() =
  vmIndent "return"

printfn "%A" (writePush ARG 2)
