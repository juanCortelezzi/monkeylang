type 'a program = { statements : 'a list }

type parser =
  { lexer : Lexer.t
  ; current_token : Token.t
  ; peek_token : Token.t
  }

let next_token parser =
  let lexer, peek_token = Lexer.next_token parser.lexer in
  { current_token = parser.peek_token; peek_token; lexer }
;;

let parser_new lexer =
  let lexer, current_token = Lexer.next_token lexer in
  let lexer, peek_token = Lexer.next_token lexer in
  { lexer; current_token; peek_token }
;;

type identifier =
  { token : Token.t
  ; value : string
  }

type expression = { aoeu : int }

type statement =
  | Let of
      { ident : identifier
      ; expr : expression
      }
