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

module rec Typing =

    exception TypingException of Type * Type
    exception UnificationException of Type * Type

    let internal (==) e1 e2 = LanguagePrimitives.PhysicalEquality e1 e2

    type TypeChecker () =
        static let mutable extenv = Map.empty
        static member GetGlobalEnv () = extenv
        static member SetGlobalEnv env = extenv <- env
        static member Run (e: Expr<_>) =
            extenv <- Map.empty;
            do
                try
                    Type.Unit.Unify (e.TypeCheck(Map.empty))
                with
                | UnificationException _ -> failwith "top level does not have type unit"
            extenv <- Map.map (fun _ (t: Type) -> t.Deref()) extenv
            e.Deref()


    type Type with
        member typ.Deref() =
            match typ with
            | Type.Fun(t1s, t2) -> Type.Fun(List.map (fun (typ: Type) -> typ.Deref())  t1s, (fun (typ: Type) -> typ.Deref())  t2)
            | Type.Tuple(ts) -> Type.Tuple(List.map (fun (typ: Type) -> typ.Deref()) ts)
            | Type.Array(t) -> Type.Array(t.Deref())
            | Type.Var({ contents = None } as r) ->
                Printf.eprintf "uninstantiated type variable detected; assuming int@."
                r := Some(Type.Int);
                Type.Int
            | Type.Var({ contents = Some(t) } as r) ->
                let t' = t
                do r := Some(t')
                t'
            | t -> t
    type Id with
        member i.Deref (typ: Type) = (i, typ.Deref ())

    type Expr<'T> with
        member expr.Deref () =
            match expr with
            | Not(e, r) -> Not(e.Deref(), r)
            | Neg(e, r) -> Neg(e.Deref(), r)
            | Add(e1, e2, r) -> Add(e1.Deref(), e2.Deref(), r)
            | Sub(e1, e2, r) -> Sub(e1.Deref(), e2.Deref(), r)
            | Eq(e1, e2, r) -> Eq(e1.Deref(), e2.Deref(), r)
            | LE(e1, e2, r) -> LE(e1.Deref(), e2.Deref(), r)
            | FNeg(e, r) -> FNeg(e.Deref(), r)
            | FAdd(e1, e2, r) -> FAdd(e1.Deref(), e2.Deref(), r)
            | FSub(e1, e2, r) -> FSub(e1.Deref(), e2.Deref(), r)
            | FMul(e1, e2, r) -> FMul(e1.Deref(), e2.Deref(), r)
            | FDiv(e1, e2, r) -> FDiv(e1.Deref(), e2.Deref(), r)
            | If(e1, e2, e3, r) -> If(e1.Deref(), e2.Deref(), e3.Deref(), r)
            | Let((x, t), e1, e2, r) -> Let(x.Deref(t), e1.Deref(), e2.Deref(), r)
            | LetRec({ name = (x, t); args = yts; body = e1} as fundef, e2, r) ->
                LetRec({ fundef with name = x.Deref(t); args = List.map (fun (i: Id, t) -> i.Deref(t)) yts; body = e1.Deref() }, e2.Deref(), r)
            | App(e, es, r) -> App(e.Deref(), List.map (fun (e: Expr<_>) -> e.Deref()) es, r)
            | Tuple(es, r) -> Tuple(List.map (fun (e: Expr<_>) -> e.Deref()) es, r)
            | LetTuple(xts, e1, e2, r) -> LetTuple(List.map (fun (i: Id, t) -> i.Deref(t)) xts, e1.Deref(), e2.Deref(), r)
            | Array(e1, e2, r) -> Array(e1.Deref(), e2.Deref(), r)
            | Get(e1, e2, r) -> Get(e1.Deref(), e2.Deref(), r)
            | Put(e1, e2, e3, r) -> Put(e1.Deref(), e2.Deref(), e3.Deref(), r)
            | PrintInt (e, r) -> PrintInt(e.Deref(), r)
            | PrintFloat (e, r) -> PrintFloat(e.Deref(), r)
            | e -> e

    let rec occur r1 r2 =
        match r2 with
        | Type.Fun(t2s, t2) -> List.exists (occur r1) t2s || occur r1 t2
        | Type.Tuple(t2s) -> List.exists (occur r1) t2s
        | Type.Array(t2) -> occur r1 t2
        | Type.Var(r2) when r1 == r2 -> true
        | Type.Var({ contents = None }) -> false
        | Type.Var({ contents = Some(t2) }) -> occur r1 t2
        | _ -> false

    type Type with
        member t1.Unify t2 =
            match (t1, t2) with
            | Type.Unit, Type.Unit
            | Type.Bool, Type.Bool
            | Type.Int, Type.Int
            | Type.Float, Type.Float -> ()
            | Type.Fun(t1s, t1'), Type.Fun(t2s, t2') ->
                try
                    List.iter2 (fun (t1: Type) t2 -> t1.Unify t2) t1s t2s
                with
                | _ -> raise (UnificationException(t1, t2))
                t1'.Unify t2'
            | Type.Tuple(t1s), Type.Tuple(t2s) ->
                try
                    List.iter2 (fun (t1: Type) t2 -> t1.Unify t2) t1s t2s
                with
                | _ -> raise (UnificationException(t1, t2))
            | Type.Array(t1), Type.Array(t2) -> t1.Unify t2
            | Type.Var(r1), Type.Var(r2) when r1 == r2 -> ()
            | Type.Var({ contents = Some(t1') }), _ -> t1'.Unify t2
            | _, Type.Var({ contents = Some(t2') }) -> t1.Unify t2'
            | Type.Var({ contents = None } as r1), _ ->
                if occur r1 t2 then raise (UnificationException(t1, t2));
                r1 := Some(t2)
            | _, Type.Var({ contents = None } as r2) ->
                if occur r2 t1 then raise (UnificationException(t1, t2));
                r2 := Some(t1)
            | _, _ -> raise (UnificationException(t1, t2))

    type Expr<'T> with
        member expr.TypeCheck env =
            try
                match expr with
                | Unit _-> Type.Unit
                | Bool(_) -> Type.Bool
                | Int(_) -> Type.Int
                | Float(_) -> Type.Float
                | Not(e, r) ->
                    Type.Bool.Unify (e.TypeCheck env);
                    Type.Bool
                | Neg(e, r) ->
                    Type.Int.Unify (e.TypeCheck env);
                    Type.Int
                | Add(e1, e2, r)
                | Sub(e1, e2, r) ->
                    Type.Int.Unify (e1.TypeCheck env);
                    Type.Int.Unify (e2.TypeCheck env);
                    Type.Int
                | FNeg(e, r) ->
                    Type.Float.Unify (e.TypeCheck env);
                    Type.Float
                | FAdd(e1, e2, r)
                | FSub(e1, e2, r)
                | FMul(e1, e2, r)
                | FDiv(e1, e2, r) ->
                    Type.Float.Unify (e1.TypeCheck env);
                    Type.Float.Unify (e2.TypeCheck env);
                    Type.Float
                | Eq(e1, e2, r)
                | LE(e1, e2, r) ->
                    (e1.TypeCheck env).Unify (e2.TypeCheck env);
                    Type.Bool
                | If(e1, e2, e3, r) ->
                    (e1.TypeCheck env).Unify Type.Bool;
                    let t2 = e2.TypeCheck env in
                    let t3 = e2.TypeCheck env in
                    t2.Unify t3;
                    t2
                | Let((x, t), e1, e2, r) ->
                    t.Unify (e1.TypeCheck env);
                    e2.TypeCheck (Map.add x t env)
                | Var(x, _) when Map.containsKey x env -> Map.find x env
                | Var(x, _) when Map.containsKey x <| TypeChecker.GetGlobalEnv() -> Map.find x <| TypeChecker.GetGlobalEnv()
                | Var(x, _) ->
                    Printf.eprintf "free variable %O assumed as external@." x;
                    let t = Type.Generate () in
                    TypeChecker.SetGlobalEnv(Map.add x t <| TypeChecker.GetGlobalEnv());
                    t
                | LetRec({ name = (x, t); args = yts; body = e1 }, e2, r) ->
                    let env = Map.add x t env in
                    t.Unify (Type.Fun(List.map snd yts, e1.TypeCheck (Map.addSeq yts env)));
                    e2.TypeCheck env
                | App(e, es, r) ->
                    let t = Type.Generate () in
                    (e.TypeCheck env).Unify (Type.Fun(List.map (fun (e: Expr<_>) -> e.TypeCheck env) es, t));
                    t
                | Tuple(es, r) -> Type.Tuple(List.map (fun (e: Expr<_>) -> e.TypeCheck env) es)
                | LetTuple(xts, e1, e2, r) ->
                    (Type.Tuple(List.map snd xts)).Unify (e1.TypeCheck env);
                    e2.TypeCheck (Map.addSeq xts env)
                | Array(e1, e2, r) ->
                    (e1.TypeCheck env).Unify Type.Int;
                    Type.Array(e2.TypeCheck env)
                | Get(e1, e2, r) ->
                    let t = Type.Generate () in
                    Type.Array(t).Unify (e1.TypeCheck env);
                    Type.Int.Unify (e2.TypeCheck env);
                    t
                | Put(e1, e2, e3, r) ->
                    let t = e3.TypeCheck env in
                    Type.Array(t).Unify (e1.TypeCheck env);
                    Type.Int.Unify (e2.TypeCheck env);
                    Type.Unit
                | PrintInt (e, r) ->
                    let t = e.TypeCheck env
                    Type.Int.Unify t
                    Type.Unit
                | PrintFloat (e, r) ->
                    let t = e.TypeCheck env
                    Type.Float.Unify t
                    Type.Unit
              with
              | UnificationException(t1, t2) ->
                eprintfn "Error for %O %O" t1 t2
                raise (TypingException(t1.Deref(), t2.Deref()))
