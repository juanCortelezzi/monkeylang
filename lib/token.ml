open Base

type token_kind =
  | EOF
  | Assign
  | Plus
  | Comma
  | Semicolon
  | LParen
  | RParen
  | LBrace
  | RBrace
  | Function
  | Let
  | Illegal of string
  | Ident of string
  | Int of int
[@@deriving equal, compare, sexp_of]

type t = { kind : token_kind }

let keywords string =
  match string with
  | "fn" -> Some Function
  | "let" -> Some Let
  | _ -> None
;;

let lookup_ident string =
  match keywords string with
  | Some kind -> kind
  | None -> Ident string
;;

let to_string token =
  match token.kind with
  | EOF -> "EOF"
  | Assign -> "Assign"
  | Plus -> "Plus"
  | Comma -> "Comma"
  | Semicolon -> "Semicolon"
  | LParen -> "LParen"
  | RParen -> "RParen"
  | LBrace -> "LBrace"
  | RBrace -> "RBrace"
  | Function -> "Function"
  | Let -> "Let"
  | Illegal string -> "Illegal(`" ^ string ^ "`)"
  | Ident string -> "Ident(`" ^ string ^ "`)"
  | Int int -> "Int(`" ^ Int.to_string int ^ "`)"
;;
