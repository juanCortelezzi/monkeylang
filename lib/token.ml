type token_kind =
  | Illegal
  | EOF
  | Ident
  | Int
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
[@@deriving compare, equal, sexp_of]

type t =
  { kind : token_kind
  ; literal : string
  }

let keywords string =
  match string with
  | "fn" -> Some Function
  | "let" -> Some Let
  | _ -> None
;;

let lookup_ident string =
  match keywords string with
  | Some kind -> kind
  | None -> Ident
;;

