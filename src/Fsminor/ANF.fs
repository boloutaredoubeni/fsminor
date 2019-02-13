//
//  Copyright 2019  Boloutare Doubeni
//
//    Licensed under the Apache License, Version 2.0 (the "License");
//    you may not use this file except in compliance with the License.
//    You may obtain a copy of the License at
//
//        http://www.apache.org/licenses/LICENSE-2.0
//
//    Unless required by applicable law or agreed to in writing, software
//    distributed under the License is distributed on an "AS IS" BASIS,
//    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//    See the License for the specific language governing permissions and
//    limitations under the License.
namespace Fsminor

open Fsminor
open Fsminor.Syntax
open Fsminor.Collections
open Fsminor.Typing

module ANormal =

    type ANF =
        | Unit
        | Int of int
        | Float of float
        | Neg of Id
        | Add of Id * Id
        | Sub of Id * Id
        | FNeg of Id
        | FAdd of Id * Id
        | FSub of Id * Id
        | FMul of Id * Id
        | FDiv of Id * Id
        | IfEq of Id * Id * ANF * ANF
        | IfLE of Id * Id * ANF * ANF
        | Let of (Id * Type) * ANF * ANF
        | Var of Id
        | LetRec of Fundef * ANF
        | App of Id * Id list
        | Tuple of Id list
        | LetTuple of (Id * Type) list * Id * ANF
        | Get of Id * Id
        | Put of Id * Id * Id
        | ExtArray of Id
        | ExtFunApp of Id * Id list

        member anf.FreeVariables () =
            match anf with
            | Unit
            | Int(_)
            | Float(_)
            | ExtArray(_) -> Set.empty
            | Neg(x)
            | FNeg(x) -> Set.singleton x
            | Add(x, y)
            | Sub(x, y)
            | FAdd(x, y)
            | FSub(x, y)
            | FMul(x, y)
            | FDiv(x, y)
            | Get(x, y) -> set [x; y]
            | IfEq(x, y, e1, e2)
            | IfLE(x, y, e1, e2) -> Set.add x (Set.add y (Set.union (e1.FreeVariables()) (e2.FreeVariables())))
            | Let((x, t), e1, e2) -> Set.union (e1.FreeVariables()) (Set.remove x (e2.FreeVariables()))
            | Var(x) -> Set.singleton x
            | LetRec({ name = (x, t); args = yts; body = e1 }, e2) ->
                let zs = Set.difference (e1.FreeVariables()) (Set.ofSeq (List.map fst yts)) in
                Set.difference (Set.union zs (e2.FreeVariables())) (Set.singleton x)
            | App(x, ys) -> Set.ofSeq (x :: ys)
            | Tuple(xs)
            | ExtFunApp(_, xs) -> Set.ofSeq xs
            | Put(x, y, z) -> Set.ofSeq [x; y; z]
            | LetTuple(xs, y, e) -> Set.add y (Set.difference (e.FreeVariables()) (Set.ofSeq(List.map fst xs)))
    and Fundef = { name : Id * Type; args : (Id * Type) list; body : ANF }


    let private insertLet (e, t) k =
        match e with
        | Var(x) -> k x
        | _ ->
            let x = Id.GenerateTmp t
            let e', t' = k x
            (Let((x, t), e, e'), t')

    let rec private run env t =
        match t with
        | Syntax.Unit _ -> Unit, Type.Unit
        | Syntax.Bool(b=b) -> Int(if b then 1 else 0), Type.Int
        | Syntax.Int(i=i) -> Int(i), Type.Int
        | Syntax.Float(f=d) -> Float(d), Type.Float
        | Syntax.Not(expr=e; meta=m) -> run env (Syntax.If(e, Syntax.Bool(false, m), Syntax.Bool(true, m), m))
        | Syntax.Neg(expr=e) ->
            insertLet (run env e) (fun x -> Neg(x), Type.Int)
        | Syntax.Add(expr=e1; expr1=e2) ->
            insertLet (run env e1) (fun x -> insertLet (run env e2) (fun y -> Add(x, y), Type.Int))
        | Syntax.Sub(expr=e1; expr1=e2) ->
            insertLet (run env e1) (fun x -> insertLet (run env e2) (fun y -> Sub(x, y), Type.Int))
        | Syntax.FNeg(expr=e) ->
            insertLet (run env e) (fun x -> FNeg(x), Type.Float)
        | Syntax.FAdd(expr=e1; expr1=e2) ->
            insertLet (run env e1) (fun x -> insertLet (run env e2) (fun y -> FAdd(x, y), Type.Float))
        | Syntax.FSub(expr=e1; expr1=e2) ->
            insertLet (run env e1) (fun x -> insertLet (run env e2) (fun y -> FSub(x, y), Type.Float))
        | Syntax.FMul(expr=e1; expr1=e2) ->
            insertLet (run env e1) (fun x -> insertLet (run env e2) (fun y -> FMul(x, y), Type.Float))
        | Syntax.FDiv(expr=e1; expr1=e2) ->
            insertLet (run env e1) (fun x -> insertLet (run env e2) (fun y -> FDiv(x, y), Type.Float))
        | Syntax.Eq (meta=m)
        | Syntax.LE (meta=m) as cmp ->
            run env (Syntax.If(cmp, Syntax.Bool(true, m), Syntax.Bool(false, m), m))
        | Syntax.If(expr=Syntax.Not(expr=e1); expr1=e2; expr2=e3; meta=m) -> run env (Syntax.If(e1, e3, e2, m))
        | Syntax.If(Syntax.Eq(expr=e1; expr1=e2), e3, e4, _m) ->
            insertLet
                (run env e1)
                (fun x ->
                    insertLet
                        (run env e2)
                        (fun y ->
                        let e3', t3 = run env e3
                        let e4', t4 = run env e4
                        (IfEq(x, y, e3', e4'), t3)))
        | Syntax.If(Syntax.LE(expr=e1; expr1=e2), e3, e4, _) ->
            insertLet
                (run env e1)
                (fun x ->
                    insertLet
                        (run env e2)
                        (fun y ->
                            let e3', t3 = run env e3 in
                            let e4', t4 = run env e4 in
                            (IfLE(x, y, e3', e4'), t3)))
        | Syntax.If(e1, e2, e3, m) -> run env (Syntax.If(Syntax.Eq(e1, Syntax.Bool(false, m), m), e3, e2, m))
        | Syntax.Let((x, t), e1, e2, m) ->
            let e1', t1 = run env e1 in
            let e2', t2 = run (Map.add x t env) e2 in
            (Let((x, t), e1', e2'), t2)
        | Syntax.Var(x, _) when Map.containsKey x env -> (Var(x), Map.find x env)
        | Syntax.Var(x, _) ->
            match Map.find x <| TypeChecker.GetGlobalEnv () with
            | Type.Array(_) as t -> ExtArray x, t
            | _ -> failwith (Printf.sprintf "external variable %O does not have an array type" x)
        | Syntax.LetRec({ Syntax.name = (x, t); Syntax.args = yts; Syntax.body = e1 }, e2, _) ->
            let env' = Map.add x t env in
            let e2', t2 = run env' e2 in
            let e1', t1 = run (Map.addSeq yts env') e1 in
            LetRec({ name = (x, t); args = yts; body = e1' }, e2'), t2
        | Syntax.App(Syntax.Var(f, _), e2s, _) when not (Map.containsKey f env) ->
            match Map.find f <| TypeChecker.GetGlobalEnv () with
            | Type.Fun(_, t) ->
                let rec bind xs = function
                    | [] -> ExtFunApp(f, xs), t
                    | e2 :: e2s ->
                        insertLet (run env e2)
                            (fun x -> bind (xs @ [x]) e2s)
                bind [] e2s
            | _ -> failwithf "assert false"
        | Syntax.App(e1, e2s, _) ->
            match run env e1 with
            | _, Type.Fun(_, t) as g_e1 ->
                insertLet
                    g_e1
                    (fun f ->
                      let rec bind xs = function
                        | [] -> App(f, xs), t
                        | e2 :: e2s ->
                            insertLet (run env e2)
                              (fun x -> bind (xs @ [x]) e2s) in
                      bind [] e2s)
            | _ -> failwithf "assert false"
        | Syntax.Tuple(es, _) ->
            let rec bind xs ts = function
                | [] -> (Tuple(xs), Type.Tuple(ts))
                | e :: es ->
                    let _, t as g_e = run env e in
                    insertLet g_e
                      (fun x -> bind (xs @ [x]) (ts @ [t]) es) in
            bind [] [] es
        | Syntax.LetTuple(xts, e1, e2, m) ->
            insertLet
                (run env e1)
                (fun y ->
                    let e2', t2 = run (Map.addSeq xts env) e2 in
                    LetTuple(xts, y, e2'), t2)
        | Syntax.Array(e1, e2, m) ->
            insertLet
                (run env e1)
                (fun x ->
                    let _, t2 as g_e2 = run env e2 in
                    insertLet
                        g_e2
                        (fun y ->
                            let l =
                                match t2 with
                                | Type.Float -> Id "create_float_array"
                                | _ -> Id "create_array" in
                            ExtFunApp(l, [x; y]), Type.Array(t2)))
        | Syntax.Get(expr=e1; expr1=e2) ->
            match run env e1 with
            | _, Type.Array(t) as g_e1 ->
                insertLet g_e1 (fun x -> insertLet (run env e2) (fun y -> Get(x, y), t))
            | _ -> failwithf "assert false"
        | Syntax.Put(e1, e2, e3, _) ->
            insertLet
                (run env e1)
                (fun x ->
                    insertLet
                        (run env e2)
                        (fun y ->
                            insertLet
                                (run env e3)
                                (fun z -> (Put(x, y, z), Type.Unit))))
        | Syntax.PrintInt(expr=e) ->
          insertLet
            (run env e)
            (fun x -> (ExtFunApp(Id "print_int", [x]), Type.Unit))
        | Syntax.PrintFloat(expr=e) ->
          insertLet
            (run env e)
            (fun x -> (ExtFunApp(Id "print_float", [x]), Type.Unit))


    type Expr<'T> with
        member expr.ToANF () =
            fst (run (Map.empty) expr)
