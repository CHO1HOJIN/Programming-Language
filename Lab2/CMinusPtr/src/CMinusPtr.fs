module CMinusPtr

open AST
open Types

// Evaluate expression into a value, under the given memory.
let rec evalExp (exp: Exp) (mem: Mem) : Val =
  match exp with
  | Num i -> Int i
  | True -> Bool true
  | False -> Bool false
  | LV x -> 
    match x with
    | Var x -> 
      let k = Map.tryFind x mem
      match k with
      | Some value -> value
      | None -> raise (UndefinedSemantics)
    | Deref x ->
      match evalExp x mem with
      | Val.Loc l -> Map.find l mem
      | _ -> raise (UndefinedSemantics)
  | AddrOf x -> Val.Loc x
  | Add (x, y) -> 
    match (evalExp x mem, evalExp y mem) with
    | (Val.Int i, Val.Int j) -> Val.Int (i + j)
    | _ -> raise (UndefinedSemantics)
  | Sub (x, y) -> 
    match (evalExp x mem, evalExp y mem) with
    | (Val.Int i, Val.Int j) -> Val.Int (i - j)
    | _ -> raise UndefinedSemantics
  | LessThan (x,y) -> 
    match (evalExp x mem, evalExp y mem) with
    | (Val.Int i, Val.Int j) -> if i < j then Bool true else Bool false
    | _ -> raise UndefinedSemantics
  | GreaterThan (x,y) -> 
    match (evalExp x mem, evalExp y mem) with
    | (Val.Int i, Val.Int j) -> if i > j then Bool true else Bool false
    | _ -> raise (UndefinedSemantics)
  | Equal (x,y) -> 
    match (evalExp x mem, evalExp y mem) with
    | (Val.Int i, Val.Int j) -> if i = j then Bool true else Bool false
    | (Val.Bool i, Val.Bool j) -> if i = j then Bool true else Bool false
    | (Val.Loc i, Val.Loc j) -> if i = j then Bool true else Bool false
    | _ -> raise UndefinedSemantics
  | NotEq (x,y) -> 
    match (evalExp x mem, evalExp y mem) with
    | (Val.Int i, Val.Int j) -> if i <> j then Bool true else Bool false
    | (Val.Bool i, Val.Bool j) -> if i <> j then Bool true else Bool false
    | (Val.Loc i, Val.Loc j) -> if i <> j then Bool true else Bool false
    | _ -> raise UndefinedSemantics
// Note: You may define more functions.

// Execute a statement and return the updated memory.
let rec exec (stmt: Stmt) (mem: Mem) : Mem =
  match stmt with
  | NOP -> mem // NOP does not change the memory.
  | Assign (lv, e) -> 
    match lv with
    | Var lv -> 
      Map.add lv (evalExp e mem) mem
    | Deref lv ->
      let x = evalExp e mem
      let l = evalExp lv mem
      match l with
      | Val.Loc l -> Map.add l x mem
      | _ -> raise UndefinedSemantics
  | Seq (s1, s2) -> 
    exec s2 (exec s1 mem)
  | If (cond, s1, s2: Stmt) -> 
    match evalExp cond mem with
    | Bool true -> exec s1 mem
    | Bool false -> exec s2 mem
    | _ -> raise UndefinedSemantics
  | While (cond, s) -> 
    match evalExp cond mem with
    | Bool k -> if k then exec (Seq (s, (While (cond, s)))) mem else mem
    | _ -> raise UndefinedSemantics

// The program starts execution with an empty memory. Do NOT fix this function.
let run (prog: Program) : Mem =
  exec prog Map.empty