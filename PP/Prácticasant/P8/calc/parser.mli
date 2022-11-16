type token =
  | EOF
  | MAS
  | MENOS
  | POR
  | DIVIDIDO
  | IGUAL
  | PARENTIZQ
  | PARENTDCH
  | CONSTANTE of (float)
  | COMANDO of (string)
  | VARIABLE of (string)
  | FUNCION of (string)

val s :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Instruccion.instruccion
