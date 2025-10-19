(* TODO: Can I avoid these long names *)
include Chomp.Parser with type pos = Lexer.pos and type token = Lexer.token

val t_p : Types.t t
val ty_p : Types.ty t
