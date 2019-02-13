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

open Fsminor.Collections
open Fsminor.ANormal

module Alpha =
  let private find x env = Option.defaultValue x (Map.tryFind x env)
  let rec run (env: Map<Id, _>) (t: ANF) =
      match t with
      | Unit -> Unit
      | Int(i) -> Int(i)
      | Float(d) -> Float(d)
      | Neg(x) -> Neg(find x env)
      | Add(x, y) -> Add(find x env, find y env)
      | Sub(x, y) -> Sub(find x env, find y env)
      | FNeg(x) -> FNeg(find x env)
      | FAdd(x, y) -> FAdd(find x env, find y env)
      | FSub(x, y) -> FSub(find x env, find y env)
      | FMul(x, y) -> FMul(find x env, find y env)
      | FDiv(x, y) -> FDiv(find x env, find y env)
      | IfEq(x, y, e1, e2) -> IfEq(find x env, find y env, run env e1, run env e2)
      | IfLE(x, y, e1, e2) -> IfLE(find x env, find y env, run env e1, run env e2)
      | Let((x, t), e1, e2) ->
          let x' = Id.GenerateId x.Name in
          Let((x', t), run env e1, run (Map.add x x' env) e2)
      | Var(x) -> Var(find x env)
      | LetRec({ name = (x, t); args = yts; body = e1 }, e2) ->
          let env = Map.add x (Id.GenerateId x.Name) env in
          let ys = List.map fst yts in
          let env' = Map.addSeq2 ys (List.map (fun (i: Id) -> Id.GenerateId i.Name) ys) env in
          LetRec({ name = (find x env, t);
                 args = List.map (fun (y, t) -> (find y env', t)) yts;
                 body = run env' e1 },
               run env e2)
      | App(x, ys) -> App(find x env, List.map (fun y -> find y env) ys)
      | Tuple(xs) -> Tuple(List.map (fun x -> find x env) xs)
      | LetTuple(xts, y, e) ->
          let xs = List.map fst xts in
          let env' = Map.addSeq2 xs (List.map (fun (i: Id) -> Id.GenerateId i.Name) xs) env in
          LetTuple(List.map (fun (x, t) -> (find x env', t)) xts,
                 find y env,
                 run env' e)
      | Get(x, y) -> Get(find x env, find y env)
      | Put(x, y, z) -> Put(find x env, find y env, find z env)
      | ExtArray(x) -> ExtArray(x)
      | ExtFunApp(x, ys) -> ExtFunApp(x, List.map (fun y -> find y env) ys)

  type ANF with
    member anf.AlphaConversion() = run Map.empty anf
