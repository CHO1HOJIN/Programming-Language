namespace FMinus

open AST

// Type.infer() must raise this if the input program seems to have a type error.
exception TypeError

// The types available in our F- language.
type Type =
  | Int
  | Bool
  | TyVar of string
  | Func of Type * Type

type TypeEnv = Map<string, Type>
type TypeEquation = (Type * Type) list
type Substitute =  (string * Type) list

module Type =

  let mutable equaltyVarlist = []
  let mutable tyVarNum = 0
  // Convert the given 'Type' to a string.
  let rec toString (typ: Type): string =
    match typ with
    | Int -> "int"
    | Bool -> "bool"
    | TyVar s -> s
    | Func (t1, t2) -> sprintf "(%s) -> (%s)" (toString t1) (toString t2)

  let rec tyVar_naming() : string =
    let name = sprintf "t%d" tyVarNum
    tyVarNum <- tyVarNum + 1
    name
  
  let rec occur_check (t1: Type) (t2: Type) : bool = //occurs check
    match t2 with
    | Int -> false
    | Bool -> false
    | TyVar s -> s = (toString t1)
    | Func (tx, ty) -> occur_check t1 tx || occur_check t1 ty
  
  let rec gen (env: TypeEnv) (e: Exp) (t: Type) : TypeEquation = 
    match e with
    | Num _ -> [(t, Int)]
    | True -> [(t, Bool)]
    | False -> [(t, Bool)]
    | Var x -> 
      match Map.tryFind x env with
      | Some t1 -> [(t, t1)]
      | None -> raise TypeError
    | Neg e1 -> gen env e1 t
    | Add (e1, e2) -> [(t, Int)]@(gen env e1 Int)@(gen env e2 Int)
    | Sub (e1, e2) -> [(t, Int)]@(gen env e1 Int)@(gen env e2 Int)
    | LessThan (e1, e2) -> [(t, Bool)]@(gen env e1 Int)@(gen env e2 Int)
    | GreaterThan (e1, e2) -> [(t, Bool)]@(gen env e1 Int)@(gen env e2 Int)
    | Equal (e1, e2) -> 
      let (t1, t2) = (TyVar (tyVar_naming ()), TyVar (tyVar_naming ()))
      equaltyVarlist <- equaltyVarlist @ [(toString t1, toString t2)]
      [(t, Bool)]@[(t1, t2)]@(gen env e1 t1)@(gen env e2 t2)
    | NotEq (e1, e2) ->
      let (t1, t2) = (TyVar (tyVar_naming ()), TyVar (tyVar_naming ()))
      equaltyVarlist <- equaltyVarlist @ [(toString t1, toString t2)]
      [(t, Bool)]@[(t1, t2)]@(gen env e1 t1)@(gen env e2 t2)
    | IfThenElse (e1, e2, e3) -> (gen env e1 Bool)@(gen env e2 t)@(gen env e3 t)
    | LetIn (x, e1, e2) -> 
      let t1 = TyVar (tyVar_naming())
      (gen env e1 t1)@(gen (Map.add x t1 env) e2 t)
    | LetFunIn (f, x, e1, e2) ->  
      let (t_arg, t_res) = (TyVar (tyVar_naming()), TyVar (tyVar_naming()))
      (gen (Map.add x t_arg env) e1 t_res)@(gen (Map.add f (Func (t_arg, t_res)) env) e2 t)
    | LetRecIn (f, x: string, e1, e2) ->  
      let (t_arg, t_res) = (TyVar (tyVar_naming()), TyVar (tyVar_naming()))
      let _env = gen (Map.add f (Func (t_arg, t_res)) (Map.add x t_arg env)) e1 t_res
      let __env = (gen (Map.add f (Func (t_arg, t_res)) env) e2 t)
      _env@__env
    | Fun (x, e) -> 
      let (t_arg, t_res) = (TyVar (tyVar_naming()), TyVar (tyVar_naming()))
      [(t, Func (t_arg, t_res))]@(gen (Map.add x t_arg env) e t_res)
    | App (e1, e2) -> 
      let t_arg = TyVar (tyVar_naming())
      (gen env e1 (Func (t_arg, t)))@(gen env e2 t_arg)

  let rec application (s: Substitute) (t: Type) : Type =
    match t with
    | Int -> Int
    | Bool -> Bool
    | TyVar k ->
      match List.tryFind (fun (x, y) -> x = k) s with
      | Some (_, t) -> t
      | None -> TyVar k
    | Func (t1, t2) -> Func (application s t1, application s t2)

  let merge (tx: string) (ty: Type) (s: Substitute) =
    (tx, ty)::(List.map (fun (x, y) -> (x, application [(tx, ty)] y)) s)

  let rec unify (t1: Type) (t2: Type) (s: Substitute) : Substitute =
    let rec extend (t1: Type) (t2: Type) (s: Substitute): Substitute =
      match (t1, t2) with
      | (Int, Int) | (Bool, Bool) -> s
      | (TyVar x, _) ->
        match t2 with
        | TyVar y -> if x = y then s else merge x t2 s
        | _ -> if occur_check (TyVar x) t2 then raise TypeError else merge x t2 s
      | (_, TyVar t) ->  extend (TyVar t) t1 s
      | (Func(tx1, ty1), Func(tx2, ty2)) -> unify ty1 ty2 (extend tx1 tx2 s)
      | _ -> raise TypeError
    extend (application s t1) (application s t2) s

  let rec solve (typeEquation: TypeEquation) (s: Substitute): Substitute = 
    let rec solve_inner (typeEquation: TypeEquation) (s: Substitute): Substitute =
      match typeEquation with
      | [] -> s
      | (t1, t2)::remain -> 
        let _s = unify t1 t2 s
        solve_inner remain _s
    solve_inner typeEquation s

  let rec equalvar_check (s: Substitute) : bool = 
    match equaltyVarlist with
    | [] -> true
    | (t1, t2)::remain -> 
      let (t1_tuple, t2_tuple) = (List.tryFind (fun (x, y) -> x = t1) s, List.tryFind (fun (x, y) -> x = t2) s)
      match (t1_tuple, t2_tuple) with
      | Some (x, Int), Some (y, Int) | Some (x, Bool), Some (y, Bool) -> 
        equaltyVarlist <- remain
        equalvar_check s
      | _ -> false

  // Return the inferred type of the input program.
  let infer (prog: Program) : Type =
    let typeEquation = gen Map.empty prog (TyVar "tp")
    let s = solve typeEquation []
    if (equalvar_check s) then
      match List.tryFind (fun (x, y) -> x = "tp") s with
      | Some (_, t) -> t
      | None -> raise TypeError
    else raise TypeError