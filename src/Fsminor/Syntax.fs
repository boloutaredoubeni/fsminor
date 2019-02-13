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

module Syntax =

    open Fsminor.Text

    type Expr<'T> =
        | Unit of meta: 'T
        | Bool of b: bool * meta: 'T
        | Int of i: int* meta: 'T
        | Float of f: float * meta: 'T
        | Not of expr: Expr<'T> * meta: 'T
        | Neg of expr: Expr<'T> * meta: 'T
        | Add of expr: Expr<'T> * expr1: Expr<'T> * meta: 'T
        | Sub of expr: Expr<'T> * expr1: Expr<'T> * meta: 'T
        | FNeg of expr: Expr<'T> * meta: 'T
        | FAdd of expr: Expr<'T> * expr1: Expr<'T> * meta: 'T
        | FSub of expr: Expr<'T> * expr1: Expr<'T> * meta: 'T
        | FMul of expr: Expr<'T> * expr1: Expr<'T> * meta: 'T
        | FDiv of expr: Expr<'T> * expr1: Expr<'T> * meta: 'T
        | Eq of expr: Expr<'T> * expr1: Expr<'T> * meta: 'T
        | LE of expr: Expr<'T> * expr1: Expr<'T> * meta: 'T
        | If of expr: Expr<'T> * expr1: Expr<'T> * expr2: Expr<'T> * meta: 'T
        | Let of def: (Id * Type) * expr1: Expr<'T> * expr2: Expr<'T> * meta: 'T
        | Var of id: Id * meta: 'T
        | LetRec of def: Fundef<'T> * expr1: Expr<'T> * meta: 'T
        | App of expr: Expr<'T> * Expr<'T> list * meta: 'T
        | Tuple of expr: Expr<'T> list * meta: 'T
        | LetTuple of def: (Id * Type) list * expr1: Expr<'T> * expr2: Expr<'T> * meta: 'T
        | Array of expr: Expr<'T> * expr1: Expr<'T> * meta: 'T
        | Get of expr: Expr<'T> * expr1: Expr<'T> * meta: 'T
        | Put of expr: Expr<'T> * expr1: Expr<'T> * expr2: Expr<'T> * meta: 'T
        | PrintInt of expr: Expr<'T> * meta: 'T
        | PrintFloat of expr: Expr<'T> * meta: 'T

        member ast.Meta =
            match ast with
            | Unit (meta=meta)
            | Bool (meta=meta)
            | Int  (meta=meta)
            | Float   (meta=meta)
            | Not (meta=meta)
            | Neg  (meta=meta)
            | Add  (meta=meta)
            | Sub   (meta=meta)
            | FNeg  (meta=meta)
            | FAdd  (meta=meta)
            | FSub (meta=meta)
            | FMul (meta=meta)
            | FDiv (meta=meta)
            | Eq (meta=meta)
            | LE (meta=meta)
            | If (meta=meta)
            | Let (meta=meta)
            | Var (meta=meta)
            | LetRec (meta=meta)
            | App  (meta=meta)
            | Tuple  (meta=meta)
            | LetTuple  (meta=meta)
            | Array (meta=meta)
            | Get (meta=meta)
            | Put (meta=meta)
            | PrintInt (meta=meta)
            | PrintFloat (meta=meta)-> meta

    and Fundef<'T> =
        { name : Id * Type; args : (Id * Type) list; body : Expr<'T>; meta: 'T }
        with member f.Meta = f.meta

    let inline meta t = (^TMeta : (member Meta: Range)(t))
