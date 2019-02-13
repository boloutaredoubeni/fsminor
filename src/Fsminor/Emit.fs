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
open Fsminor
open LLVMSharp


module LLVM =


    type LLVMType =
        | VoidType
        | IntType of size: int
        | FloatType
        | FuncType of LLVMType list * LLVMType
        | StructType of LLVMType list
        | ArrayType of LLVMType
        | PointerType of LLVMType

    type Type with
        member t.Type =
          match t with
          | Unit -> VoidType
          | Bool -> IntType 1
          | Float -> FloatType
          | Fun (pt, rt) -> FuncType (List.map (fun (t: Type) -> t.Type) pt, rt.Type)
          | Tuple ts -> StructType(List.map (fun (t: Type) -> t.Type) ts)
          | Array t ->  ArrayType (t.Type)

    type Op =
        | FNeg
        | IAdd
        | ISub
        | FAdd
        | FSub
        | FMul
        | FDiv
        | IEq
        | ILe

    type LLVMValue =
        | NullValue
        | ConstInt of size: int * i: int
        | ConstFloat of f: float
        | Unary of Op * Id
        | Binary of Op * Id * Id
        | If of Id * LLVMValue * LLVMValue
        | Let of Id * LLVMValue * LLVMValue
        | Var of Id
        | ClosureValue of Id * Closure.closure * LLVMValue

    type Closure.t with
      member cls.Codegen =
        match cls with
        | Closure.Unit -> NullValue
        | Closure.Int i -> ConstInt (32, i)
        | Closure.Float f -> ConstFloat f
        | Closure.Neg v -> Unary (FNeg, v)
        | Closure.Add (l, r) -> Binary(IAdd, l, r)
        | Closure.Sub (l, r) -> Binary(ISub, l , r)
        | Closure.FNeg f -> Unary(FNeg, f)
        | Closure.FAdd (l, r) -> Binary(FAdd, l, r)
        | Closure.FSub (l, r) -> Binary(FSub, l , r)
        | Closure.FMul (l, r) -> Binary(FMul, l , r)
        | Closure.FDiv(l, r) -> Binary(FDiv, l , r)
        | Closure.IfEq (l, r, t, f) ->
            let var = Id.GenerateId "llvm"
            Let(
                var,
                Binary(IEq, l, r),
                If(var, t.Codegen, f.Codegen))
        | Closure.IfLE (l, r, t, f) ->
            let var = Id.GenerateId "llvm"
            Let(
                var,
                Binary(ILe, l, r),
                If(var, t.Codegen, f.Codegen))
        | Closure.Let ((v, _), e1, e2) -> Let(v, e1.Codegen, e2.Codegen)
        | Closure.Var v -> Var v



type Codegen(moduleName) =
    let mutable disposed = false
    let llvmmodule = LLVM.ModuleCreateWithName(moduleName)
    let builder = LLVM.CreateBuilder()
    let context = LLVM.ContextCreate()
    interface IDisposable with
        member __.Dispose() =
          if disposed then
            LLVM.DisposeModule(llvmmodule)
            LLVM.DisposeBuilder(builder)
            LLVM.ContextDispose(context)
            disposed <- true


module Emit =

    open System.IO
    open LLVMSharp
    open Fsminor.Closure

    let f (oc: TextWriter) (prog: prog) =
        printfn "%O" prog
