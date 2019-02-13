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
namespace Fsminor.Text

open Microsoft.FSharp.Text.Lexing

type Range =
  { start: Position; finish: Position }

  static member Zero = { start = Position.Empty; finish = Position.Empty}

  static member (+) ({ start=p1 }, { finish=p2 }) = { start=p1; finish=p2 }

  override r.ToString() =
    let (|Position|) (p: Position) = (p.FileName, p.Line, p.Column)
    match r with
    | { start=(Position(file, l1, c1)); finish=Position(_, l2, c2) } ->
        sprintf "file=%s, start=(line=%d, col=%d), end=(line=%d, col=%d)" file l1 c1 l2 c2

[<AutoOpen>]
module Range =

  type LexBuffer<'T> with
    member l.Range () = { start=l.StartPos; finish=l.EndPos}

    member lexbuf.NewLine () =
      // FIXME: this doesn't work the way i thought it did
      printfn "line was %O" lexbuf.StartPos
      lexbuf.StartPos <- lexbuf.StartPos.NextLine
      printfn "line is %O" lexbuf.EndPos

module Lexing =
    let lexeme (l: LexBuffer<_>) = LexBuffer<_>.LexemeString(l)
    let newline (lexbuf: LexBuffer<_>) =
      printfn "line was %d" lexbuf.StartPos.pos_lnum
      lexbuf.StartPos <- lexbuf.StartPos.NextLine
      printfn "line is %d" lexbuf.StartPos.pos_lnum
