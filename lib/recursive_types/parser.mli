module Lexer : module type of Chomp.Lexer
module Parser : Chomp.Parser with type pos = Lexer.pos and type token = Lexer.token

val t_p : Types.t Parser.t
val ty_p : Types.ty Parser.t
