module ParserExtensions

module TexParser1 =
    let parse (f: int -> int) lexbuf = TexParser.start lexbuf