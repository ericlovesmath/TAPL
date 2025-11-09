module Lexer : module type of Chomp.Lexer
module Parser : Chomp.Parser with type pos = Lexer.pos and type token = Lexer.token

val program_p : Types.program Parser.t
