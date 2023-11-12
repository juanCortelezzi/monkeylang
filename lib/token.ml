open Base

type token_kind =
  (* Delimiters *)
  | Comma
  | Semicolon
  | LParen
  | RParen
  | LBrace
  | RBrace
  | EOF
  (* operators *)
  | Assign
  | Plus
  | Minus
  | Bang
  | Asterisk
  | Slash
  | LT
  | GT
  | Eq
  | NotEq
  (* keywords *)
  | Let
  | Function
  | Ident of string
  | True
  | False
  | If
  | Else
  | Return
  (* Data types *)
  | Int of int
  | Illegal of string
[@@deriving equal, compare, sexp_of]

type t = { kind : token_kind }

let keywords string =
  match string with
  | "fn" -> Some Function
  | "let" -> Some Let
  | "true" -> Some True
  | "false" -> Some False
  | "if" -> Some If
  | "else" -> Some Else
  | "return" -> Some Return
  | _ -> None
;;

let lookup_ident string =
  match keywords string with
  | Some kind -> kind
  | None -> Ident string
;;

let to_string token =
  match token.kind with
  | Comma -> "Comma"
  | Semicolon -> "Semicolon"
  | LParen -> "LParen"
  | RParen -> "RParen"
  | LBrace -> "LBrace"
  | RBrace -> "RBrace"
  | EOF -> "EOF"
  | Assign -> "Assign"
  | Plus -> "Plus"
  | Minus -> "Minus"
  | Bang -> "Bang"
  | Asterisk -> "Asterisk"
  | Slash -> "Slash"
  | LT -> "LT"
  | GT -> "GT"
  | Eq -> "Eq"
  | NotEq -> "NotEq"
  | Let -> "Let"
  | Function -> "Function"
  | True -> "True"
  | False -> "False"
  | If -> "If"
  | Else -> "Else"
  | Return -> "Return"
  | Ident string -> "Ident(`" ^ string ^ "`)"
  | Int int -> "Int(`" ^ Int.to_string int ^ "`)"
  | Illegal string -> "Illegal(`" ^ string ^ "`)"
;;
