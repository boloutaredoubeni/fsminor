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

module ConstFold =

    open Fsminor.ANormal

    let memi x env =
        match Map.tryFind x env with
        | Some (Int _) -> true
        | Some _
        | None -> false
    let memf x env =
        match Map.tryFind x env with
        | Some (Float _) -> true
        | Some _
        | None -> false
    let memt x env =
        match Map.tryFind x env with
        | Some (Tuple _) -> true
        | Some _
        | None -> false

    let findi x env =
        match Map.tryFind x env with
        | Some (Int i) -> i
        | _ -> failwith "Not found"
    let findf x env =
        match Map.tryFind x env with
        | Some (Float i) -> i
        | _ -> failwith "Not found"
    let findt x env =
        match Map.tryFind x env with
        | Some (Tuple i) -> i
        | _ -> failwith "Not found"
    let rec g env t =
        match t with
        | Var(x) when memi x env -> Int(findi x env)
        | Var(x) when memf x env -> Float(findf x env)
        | Var(x) when memt x env -> Tuple(findt x env)
        | Neg(x) when memi x env -> Int(-(findi x env))
        | Add(x, y) when memi x env && memi y env -> Int(findi x env + findi y env) (* 足し算のケース *)
        | Sub(x, y) when memi x env && memi y env -> Int(findi x env - findi y env)
        | FNeg(x) when memf x env -> Float(- (findf x env))
        | FAdd(x, y) when memf x env && memf y env -> Float(findf x env + findf y env)
        | FSub(x, y) when memf x env && memf y env -> Float(findf x env - findf y env)
        | FMul(x, y) when memf x env && memf y env -> Float(findf x env * findf y env)
        | FDiv(x, y) when memf x env && memf y env -> Float(findf x env / findf y env)
        | IfEq(x, y, e1, e2) when memi x env && memi y env -> if findi x env = findi y env then g env e1 else g env e2
        | IfEq(x, y, e1, e2) when memf x env && memf y env -> if findf x env = findf y env then g env e1 else g env e2
        | IfEq(x, y, e1, e2) -> IfEq(x, y, g env e1, g env e2)
        | IfLE(x, y, e1, e2) when memi x env && memi y env -> if findi x env <= findi y env then g env e1 else g env e2
        | IfLE(x, y, e1, e2) when memf x env && memf y env -> if findf x env <= findf y env then g env e1 else g env e2
        | IfLE(x, y, e1, e2) -> IfLE(x, y, g env e1, g env e2)
        | Let((x, t), e1, e2) ->
            let e1' = g env e1 in
            let e2' = g (Map.add x e1' env) e2 in
            Let((x, t), e1', e2')
        | LetRec({ name = x; args = ys; body = e1 }, e2) ->
            LetRec({ name = x; args = ys; body = g env e1 }, g env e2)
        | LetTuple(xts, y, e) when memt y env ->
            List.fold2
                (fun e' xt z -> Let(xt, Var(z), e'))
                (g env e)
                xts
                (findt y env)
        | LetTuple(xts, y, e) -> LetTuple(xts, y, g env e)
        | e -> e

    let f: (ANF -> ANF) = g Map.empty
