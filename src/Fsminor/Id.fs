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

open System

module private Id =
    let mutable internal counter = 0

type Id =
    private
    | Id of string

    static member Init () = Id.counter <- 0

    static member Create name = Id name

    static member GenerateId s =
        Id.counter <- Id.counter + 1
        Id (sprintf "%s.%d" s Id.counter)

    static member OfType typ =
        match typ with
        | Unit -> "u"
        | Bool -> "b"
        | Int -> "i"
        | Float -> "d"
        | Fun _ -> "f"
        | Tuple _ -> "t"
        | Array _ -> "a"
        | Var _ -> "_"

    static member GenerateTmp typ =
        Id.counter <- Id.counter + 1
        let tmp = Id.OfType typ
        Id (sprintf "T%s%d" tmp Id.counter)

    member i.Name =
        let (Id(name)) = i
        name

type Label = Label of Id
