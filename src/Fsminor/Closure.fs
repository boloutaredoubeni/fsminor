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

open Fsminor.ANormal

module Closure =

  open Fsminor.Collections
  type closure = { entry : Label; actual_fv : Id list }
  type t =
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
      | IfEq of Id * Id * t * t
      | IfLE of Id * Id * t * t
      | Let of (Id * Type) * t * t
      | Var of Id
      | MakeCls of (Id * Type) * closure * t
      | AppCls of Id * Id list
      | AppDir of Label * Id list
      | Tuple of Id list
      | LetTuple of (Id * Type) list * Id * t
      | Get of Id * Id
      | Put of Id * Id * Id
      | ExtArray of Label
  type fundef = { name : Label * Type;
                  args : (Id * Type) list;
                  formal_fv : (Id * Type) list;
                  body : t }
  type prog = Prog of fundef list * t

  let rec fv (t: t): Set<Id> =
      match t with
      | Unit | Int(_) | Float(_) | ExtArray(_) -> Set.empty
      | Neg(x) | FNeg(x) -> Set.singleton x
      | Add(x, y) | Sub(x, y) | FAdd(x, y) | FSub(x, y) | FMul(x, y) | FDiv(x, y) | Get(x, y) -> set [x; y]
      | IfEq(x, y, e1, e2)| IfLE(x, y, e1, e2) -> Set.add x (Set.add y (Set.union (fv e1) (fv e2)))
      | Let((x, t), e1, e2) -> Set.union (fv e1) (Set.remove x (fv e2))
      | Var(x) -> Set.singleton x
      | MakeCls((x, t), { entry = l; actual_fv = ys }, e) -> Set.remove x (Set.union (set ys) (fv e))
      | AppCls(x, ys) -> set (x :: ys)
      | AppDir(_, xs) | Tuple(xs) -> set xs
      | LetTuple(xts, y, e) -> Set.add y (Set.difference (fv e) (set(List.map fst xts)))
      | Put(x, y, z) -> set [x; y; z]

  let mutable toplevel = []

  let rec g env known t =
      match t with
      | ANormal.Unit -> Unit
      | ANormal.Int(i) -> Int(i)
      | ANormal.Float(d) -> Float(d)
      | ANormal.Neg(x) -> Neg(x)
      | ANormal.Add(x, y) -> Add(x, y)
      | ANormal.Sub(x, y) -> Sub(x, y)
      | ANormal.FNeg(x) -> FNeg(x)
      | ANormal.FAdd(x, y) -> FAdd(x, y)
      | ANormal.FSub(x, y) -> FSub(x, y)
      | ANormal.FMul(x, y) -> FMul(x, y)
      | ANormal.FDiv(x, y) -> FDiv(x, y)
      | ANormal.IfEq(x, y, e1, e2) -> IfEq(x, y, g env known e1, g env known e2)
      | ANormal.IfLE(x, y, e1, e2) -> IfLE(x, y, g env known e1, g env known e2)
      | ANormal.Let((x, t), e1, e2) -> Let((x, t), g env known e1, g (Map.add x t env) known e2)
      | ANormal.Var(x) -> Var(x)
      | ANormal.LetRec({ ANormal.name = (x, t); ANormal.args = yts; ANormal.body = e1 }, e2) ->
        let toplevel_backup = toplevel in
        let env' = Map.add x t env in
        let known' = Set.add x known in
        let e1' = g (Map.addSeq yts env') known' e1 in
        let zs = Set.difference (fv e1') (set(List.map fst yts)) in
        let known', e1' =
          if Set.isEmpty zs then known', e1' else
          (eprintf "free variable(s) %s found in function %O@." (string zs) x;
           eprintf "function %O cannot be directly applied in fact@." x;
           toplevel <- toplevel_backup;
           let e1' = g (Map.addSeq yts env') known e1 in
           known, e1') in
        let zs = Set.toList (Set.difference (fv e1') (Set.add x (set(List.map fst yts)))) in
        let zts = List.map (fun z -> (z, Map.find z env')) zs in
        toplevel <- { name = (Label(x), t); args = yts; formal_fv = zts; body = e1' } :: toplevel;
        let e2' = g env' known' e2 in
        if Set.contains x (fv e2') then
          MakeCls((x, t), { entry = Label(x); actual_fv = zs }, e2')
        else
          (eprintf "eliminating closure(s) %O@." x;
           e2')
      | ANormal.App(x, ys) when Set.contains x known ->
        eprintf "directly applying %O@." x;
        AppDir(Label(x), ys)
      | ANormal.App(f, xs) -> AppCls(f, xs)
      | ANormal.Tuple(xs) -> Tuple(xs)
      | ANormal.LetTuple(xts, y, e) -> LetTuple(xts, y, g (Map.addSeq xts env) known e)
      | ANormal.Get(x, y) -> Get(x, y)
      | ANormal.Put(x, y, z) -> Put(x, y, z)
      | ANormal.ExtArray(x) -> ExtArray(Label(x))
      | ANormal.ExtFunApp(x, ys) ->
        let lbl = Id ("min_caml_" + x.Name)
        AppDir(Label(lbl), ys)

  let f (e: ANF): prog =
    toplevel <- [];
    let e' = g Map.empty Set.empty e in
    Prog(List.rev toplevel, e')
