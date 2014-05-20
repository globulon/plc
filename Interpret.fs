module Interpret

type expr = 
  | CstI of int
  | Var of string
  | Prim of string * expr * expr
  
///Reproduces chapter1 Interpreter
///provided that eval always return an expression

type binding = string * expr

type env = binding list

let rec lookup (k: string) = function
  | []                    -> failwith "unkown variable"
  | (n, e)::rest when n=k -> e : expr
  | _::rest               -> lookup k rest;;

  
let rec eval env = function
  | Var(x)            -> lookup x env
  | (CstI _) as c     -> c
  | Prim("+", e1, e2) -> match (eval env e1, eval env e2) with 
                            | ((CstI i1), (CstI i2)) -> CstI (i1 + i2) 
                            | _                      -> failwith "Adding non numbers"
  | Prim("*", e1, e2) -> match (eval env e1, eval env e2) with 
                            | ((CstI i1), (CstI i2)) -> CstI (i1 * i2) 
                            | _                      -> failwith "multiplying non numbers"
  | Prim("-", e1, e2) -> match (eval env e1, eval env e2) with 
                            | ((CstI i1), (CstI i2)) -> CstI (i1 - i2) 
                            | _                      -> failwith "differentiating non numbers"
  | _                 -> failwith "unknown primitive";;;  
  
  
  