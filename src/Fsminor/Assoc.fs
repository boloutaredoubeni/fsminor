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

module Assoc =

    type ANF with
        member anf.Assoc () =
            match anf with
            | IfEq(x, y, e1, e2) -> IfEq(x, y, e1.Assoc(), e2.Assoc())
            | IfLE(x, y, e1, e2) -> IfLE(x, y, e1.Assoc(), e2.Assoc())
            | Let(xt, e1, e2) ->
                let rec insert t =
                    match t with
                    | Let(yt, e3, e4) -> Let(yt, e3, insert e4)
                    | LetRec(fundefs, e) -> LetRec(fundefs, insert e)
                    | LetTuple(yts, z, e) -> LetTuple(yts, z, insert e)
                    | e -> Let(xt, e, e2.Assoc()) in
                insert (e1.Assoc())
            | LetRec({ name = xt; args = yts; body = e1 }, e2) ->
                LetRec({ name = xt; args = yts; body = e1.Assoc() }, e2.Assoc())
            | LetTuple(xts, y, e) -> LetTuple(xts, y, e.Assoc())
            | e -> e
