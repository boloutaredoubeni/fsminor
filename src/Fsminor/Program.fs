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

open System.IO
open Fsminor
open Fsminor.Typing
open Fsminor.ANormal
open Fsminor.Alpha
open Fsminor.Assoc
open Fsminor.Beta
open Microsoft.FSharp.Text.Lexing
open Argu
open Fsminor.Syntax

let mutable limit = 1000

let rec iter n e =
    do eprintfn "iteration %d." n
    if n = 0 then
        e
    else
        let e' =
            e
            |> (fun (anf: ANF) -> anf.BetaReduction())
            |> (fun (anf: ANF) -> anf.Assoc())
            |> Inline.f
            |> ConstFold.f
            |> Elim.f
        if e = e' then
            e
        else iter (n - 1) e'

let lexbuf outchan l =
    do
        Id.Init()
        TypeChecker.SetGlobalEnv Map.empty
    Parser.expr Lexer.token l
    |> TypeChecker.Run
    |> (fun (expr: Expr<_>) -> expr.ToANF())
    |> (fun (anf: ANF) -> anf.AlphaConversion())
    |> iter limit
    |> Closure.f
    |> Emit.f outchan

let string (s: string): unit = lexbuf (System.Console.Out) (LexBuffer<_>.FromString s)

let file (f: string): unit =
    use inchan = new StreamReader (f)
    use outchan = new StreamWriter(f + ".s")
    lexbuf outchan (LexBuffer<_>.FromTextReader inchan)

type CLIArguments =
    | [<Unique>]
      Inline of int
    | [<Unique>]
      Iter of int
    | [<MainCommand; ExactlyOnce; Last>]
      Files of string list

    interface IArgParserTemplate with
        member arg.Usage =
            match arg with
            | Inline _ -> "maximum size of functions inlined"
            | Iter _ -> "maximum number of optimizations iterated"
            | Files(_) -> "file names"


[<EntryPoint>]
let main argv =
    let parser = ArgumentParser.Create<CLIArguments>(programName="fsminorc")
    let results = parser.Parse(argv)
    do
        Option.iter (fun i -> Inline.threshold <- i) <| results.TryGetResult(Inline)
        Option.iter (fun i -> limit <- i) <| results.TryGetResult(Iter)
    let files = results.GetResult(Files)
    Seq.iter
        (ignore << file)
        files

    0 // return an integer exit code
