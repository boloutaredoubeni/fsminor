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

module Inline =

  open Fsminor.ANormal

  let mutable threshold = 0
  let rec size t =
      match t with
      | IfEq(_, _, e1, e2)
      | IfLE(_, _, e1, e2)
      | Let(_, e1, e2)
      | LetRec({ body = e1 }, e2) -> 1 + size e1 + size e2
      | LetTuple(_, _, e) -> 1 + size e
      | _ -> 1

  let rec g env t =
      match t with
      | IfEq(x, y, e1, e2) -> IfEq(x, y, g env e1, g env e2)
      | IfLE(x, y, e1, e2) -> IfLE(x, y, g env e1, g env e2)
      | Let(xt, e1, e2) -> Let(xt, g env e1, g env e2)
      | LetRec({ name = (x, t); args = yts; body = e1 }, e2) ->
          let env = if size e1 > threshold then env else Map.add x (yts, e1) env in
          LetRec({ name = (x, t); args = yts; body = g env e1}, g env e2)
      | App(x, ys) when Map.containsKey x env ->
          let (zs, e) = Map.find x env in
          eprintf "inlining %O@." x;
          let env' =
              List.fold2
                (fun env' (z, t) y -> Map.add z y env')
                Map.empty
                zs
                ys in
          Alpha.run env' e
      | LetTuple(xts, y, e) -> LetTuple(xts, y, g env e)
      | e -> e

  let f (e: ANF): ANF = g Map.empty e
