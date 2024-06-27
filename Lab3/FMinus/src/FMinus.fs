module FMinus

open AST
open Types

// Evaluate expression into a value, under the given environment.
let rec evalExp (exp: Exp) (env: Env) : Val =
  match exp with
  | Num i -> Int i
  | True -> Bool true
  | False -> Bool false
  | Var x -> 
    let k = Map.tryFind x env
    match k with
    | Some value -> value
    | None -> raise (UndefinedSemantics)
  | Neg e -> 
    match (evalExp e env) with
    | Int i -> Int (-1*i)
    | _ -> raise (UndefinedSemantics)
  | Add (x, y) -> 
    match (evalExp x env, evalExp y env) with
    | (Val.Int i, Val.Int j) -> Val.Int (i + j)
    | _ -> raise (UndefinedSemantics)
  | Sub (x, y) -> 
    match (evalExp x env, evalExp y env) with
    | (Val.Int i, Val.Int j) -> Val.Int (i - j)
    | _ -> raise (UndefinedSemantics)
  | LessThan (x,y) -> 
    match (evalExp x env, evalExp y env) with
    | (Val.Int i, Val.Int j) -> if i < j then Bool true else Bool false
    | _ -> raise (UndefinedSemantics)
  | GreaterThan (x,y) -> 
    match (evalExp x env, evalExp y env) with
    | (Val.Int i, Val.Int j) -> if i > j then Bool true else Bool false
    | _ -> raise (UndefinedSemantics)
  | Equal (x,y) -> 
    match (evalExp x env, evalExp y env) with
    | (Val.Int i, Val.Int j) -> if i = j then Bool true else Bool false
    | (Val.Bool i, Val.Bool j) -> if i = j then Bool true else Bool false
    | _ -> raise (UndefinedSemantics)
  | NotEq (x,y) -> 
    match (evalExp x env, evalExp y env) with
    | (Val.Int i, Val.Int j) -> if i <> j then Bool true else Bool false
    | (Val.Bool i, Val.Bool j) -> if i <> j then Bool true else Bool false
    | _ -> raise (UndefinedSemantics)
  | IfThenElse (e1, e2, e3) ->
    match (evalExp e1 env) with
    | Bool true -> (evalExp e2 env)
    | Bool false -> (evalExp e3 env)
    | _ -> raise (UndefinedSemantics)
  | LetIn (x, e1, e2) ->
    let nenv = Map.add x (evalExp e1 env) env
    evalExp e2 nenv
  | LetFunIn (f, x, e1, e2) ->
    let nenv = Map.add f (Func (x, e1, env)) env
    evalExp e2 nenv
  | LetRecIn (f, x, e1, e2) ->
    let nenv = Map.add f (RecFunc (f, x, e1, env)) env
    evalExp e2 nenv
  | Fun (x, e) ->
    Func (x, e, env)
  | App (e1, e2) ->
    let varg = evalExp e2 env
    match (evalExp e1 env) with
    | Func (x, eb, funenv) ->
      let nenv = Map.add x varg funenv
      evalExp eb nenv
    | RecFunc (f, x, eb, funenv) ->
      let nenv = Map.add x varg funenv
      let nnenv = Map.add f (RecFunc (f, x, eb, funenv)) nenv
      evalExp eb nnenv 
    | _ -> raise (UndefinedSemantics)

// Note: You may define more functions.

// The program starts execution with an empty environment. Do not fix this code.
let run (prog: Program) : Val =
  printfn "%A" prog
  evalExp prog Map.empty
