module CMinus

open AST
open Types

raise UndefinedSemantics

// Evaluate expression into a value, under the given memory.
let rec evalExp (exp: Exp) (mem: Mem) : Val =
  match exp with
  | Num i -> Int i
  | True -> Bool true
  | False -> Bool false
  | Var s -> Map.find s mem
  | Add (x, y) -> 
    match (evalExp x mem, evalExp y mem) with
    | (Val.Int i, Val.Int j) -> Val.Int (i + j)
    | _ -> raise(UndefinedSemantics)
  | Sub (x, y) -> 
    match (evalExp x mem, evalExp y mem) with
    | (Val.Int i, Val.Int j) -> Val.Int (i - j)
    | _ -> raise(UndefinedSemantics)
  | LessThan (x,y) -> 
    match (evalExp x mem, evalExp y mem) with
    | (Val.Int i, Val.Int j) -> if i < j then Bool true else Bool false
    | _ -> raise(UndefinedSemantics)
  | GreaterThan (x,y) -> 
    match (evalExp x mem, evalExp y mem) with
    | (Val.Int i, Val.Int j) -> if i > j then Bool true else Bool false
    | _ -> raise(UndefinedSemantics)
  | Equal (x,y) -> 
    match (evalExp x mem, evalExp y mem) with
    | (Val.Int i, Val.Int j) -> if i = j then Bool true else Bool false
    | (Val.Bool i, Val.Bool j) -> if i = j then Bool true else Bool false
    | _ -> raise(UndefinedSemantics)
  | NotEq (x,y) -> 
    match (evalExp x mem, evalExp y mem) with
    | (Val.Int i, Val.Int j) -> if i <> j then Bool true else Bool false
    | (Val.Bool i, Val.Bool j) -> if i <> j then Bool true else Bool false
    | _ -> raise(UndefinedSemantics) // TODO: fill in the remaining cases.

// Note: You may define more functions.

// Execute a statement and return the updated memory.
let rec exec (stmt: Stmt) (mem: Mem) : Mem =
  match stmt with
  | NOP -> mem // NOP does not change the memory.
  | Assign (x, e) -> Map.add x (evalExp e mem) mem
  | Seq (s1, s2) -> 
    exec s2 (exec s1 mem)
  | If (cond, s1, s2) -> 
    match evalExp cond mem with
    | Bool k -> if k then (exec s1 mem) else (exec s2 mem)
    | _ -> raise(UndefinedSemantics)
  | While (cond, s) -> 
    match evalExp cond mem with
    | Bool k -> if k then exec (Seq (s, (While (cond, s)))) mem else mem 
    | _ -> raise(UndefinedSemantics)

// The program starts execution with an empty memory. Do NOT fix this function.
let run (prog: Program) : Mem =
  exec prog Map.empty