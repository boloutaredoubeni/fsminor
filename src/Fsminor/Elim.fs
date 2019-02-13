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

module Elim =

    open Fsminor.ANormal

    let rec effect t =
        match t with
        | Let(_, e1, e2) | IfEq(_, _, e1, e2) | IfLE(_, _, e1, e2) -> effect e1 || effect e2
        | LetRec(_, e) | LetTuple(_, _, e) -> effect e
        | App _ | Put _ | ExtFunApp _ -> true
        | _ -> false

    let rec f (t: ANF): ANF =
        match t with
        | IfEq(x, y, e1, e2) -> IfEq(x, y, f e1, f e2)
        | IfLE(x, y, e1, e2) -> IfLE(x, y, f e1, f e2)
        | Let((x, t), e1, e2) -> (* letの場合 *)
            let e1' = f e1 in
            let e2' = f e2 in
            if effect e1' || Set.contains x (e2'.FreeVariables()) then
                Let((x, t), e1', e2')
            else
                eprintf "eliminating variable %O@." x;
                e2'
        | LetRec({ name = (x, t); args = yts; body = e1 }, e2) ->
            let e2' = f e2 in
            if Set.contains x (e2'.FreeVariables()) then
                LetRec({ name = (x, t); args = yts; body = f e1 }, e2')
            else
                eprintf "eliminating function %O@." x;
                e2'
        | LetTuple(xts, y, e) ->
            let xs = List.map fst xts in
            let e' = f e in
            let live = e'.FreeVariables() in
            if List.exists (fun x -> Set.contains x live) xs then
                LetTuple(xts, y, e')
            else
                eprintf "eliminating variables %s@." (string xs);
                e'
        | e -> e
