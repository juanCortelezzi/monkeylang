open Base

type t =
  { input : string
  ; position : int
  ; read_position : int
  ; ch : char option
  }

let print lexer =
  let s = "Lexer:\n" in
  let s = s ^ "input: `" ^ lexer.input ^ "`\n" in
  let s = s ^ "position: " ^ Int.to_string lexer.position ^ "\n" in
  let s = s ^ "read_pos: " ^ Int.to_string lexer.read_position ^ "\n" in
  let s =
    let ch =
      match lexer.ch with
      | Some c -> Char.to_string c
      | None -> "_"
    in
    s ^ "ch: `" ^ ch ^ "`\n"
  in
  Stdlib.print_endline s;
  lexer
;;

let advance lexer =
  let input_len = String.length lexer.input in
  if lexer.read_position >= input_len
  then { lexer with ch = None }
  else (
    let ch = String.get lexer.input lexer.read_position in
    let position = lexer.read_position in
    let read_position = lexer.read_position + 1 in
    { lexer with position; read_position; ch = Some ch })
;;

let char_to_literal c =
  match c with
  | Some s -> String.of_char s
  | None -> ""
;;

let is_letter c = Char.is_alpha c || Char.equal c '_'

(* Although a simple wrapper for now, it might be extended later. *)
let is_digit c = Char.is_digit c

let rec skip_whitespace lexer =
  match lexer.ch with
  | Some ch when Char.is_whitespace ch -> skip_whitespace (advance lexer)
  | _ -> lexer
;;

let read_while ~f lexer =
  let pos = lexer.position in
  let rec loop lexer =
    match lexer.ch with
    | Some ch when f ch -> loop (advance lexer)
    | _ -> lexer
  in
  let new_lexer = loop lexer in
  let ident = String.sub new_lexer.input ~pos ~len:(new_lexer.position - pos) in
  new_lexer, ident
;;

let next_token l =
  let open Token in
  let l = skip_whitespace l in
  match l.ch with
  | Some '=' -> advance l, { kind = Assign; literal = char_to_literal l.ch }
  | Some ';' -> advance l, { kind = Semicolon; literal = char_to_literal l.ch }
  | Some '(' -> advance l, { kind = LParen; literal = char_to_literal l.ch }
  | Some ')' -> advance l, { kind = RParen; literal = char_to_literal l.ch }
  | Some ',' -> advance l, { kind = Comma; literal = char_to_literal l.ch }
  | Some '+' -> advance l, { kind = Plus; literal = char_to_literal l.ch }
  | Some '{' -> advance l, { kind = LBrace; literal = char_to_literal l.ch }
  | Some '}' -> advance l, { kind = RBrace; literal = char_to_literal l.ch }
  | Some ch when is_letter ch ->
    let lexer, ident = read_while ~f:is_letter l in
    lexer, { kind = lookup_ident ident; literal = ident }
  | Some ch when is_digit ch ->
    let lexer, ident = read_while ~f:is_digit l in
    lexer, { kind = Int; literal = ident }
  | Some _ -> advance l, { kind = Illegal; literal = char_to_literal l.ch }
  | None -> advance l, { kind = EOF; literal = "" }
;;

let create input = advance { input; position = 0; read_position = 0; ch = None }

let%test_unit "test_next_token" =
  let open Token in
  let input = "=+(){},;" in
  let tokens =
    [ { kind = Assign; literal = "=" }
    ; { kind = Plus; literal = "+" }
    ; { kind = LParen; literal = "(" }
    ; { kind = RParen; literal = ")" }
    ; { kind = LBrace; literal = "{" }
    ; { kind = RBrace; literal = "}" }
    ; { kind = Comma; literal = "," }
    ; { kind = Semicolon; literal = ";" }
    ; { kind = EOF; literal = "" }
    ]
  in
  let rec loop index lexer =
    let new_lexer, token = next_token lexer in
    let expected_token = List.nth tokens index in
    match expected_token with
    | Some t ->
      [%test_result: token_kind] token.kind ~expect:t.kind;
      [%test_result: string] token.literal ~expect:t.literal;
      loop (index + 1) new_lexer
    | None -> ()
  in
  create input |> loop 0 |> ignore
;;

let%test_unit "adsfads" =
  let open Token in
  let input =
    {|let five = 5;
let ten = 10;
let add = fn(x, y) {
x + y;
};
let result = add(five, ten);
|}
  in
  let tokens =
    [ { kind = Let; literal = "let" }
    ; { kind = Ident; literal = "five" }
    ; { kind = Assign; literal = "=" }
    ; { kind = Int; literal = "5" }
    ; { kind = Semicolon; literal = ";" }
    ; { kind = Let; literal = "let" }
    ; { kind = Ident; literal = "ten" }
    ; { kind = Assign; literal = "=" }
    ; { kind = Int; literal = "10" }
    ; { kind = Semicolon; literal = ";" }
    ; { kind = Let; literal = "let" }
    ; { kind = Ident; literal = "add" }
    ; { kind = Assign; literal = "=" }
    ; { kind = Function; literal = "fn" }
    ; { kind = LParen; literal = "(" }
    ; { kind = Ident; literal = "x" }
    ; { kind = Comma; literal = "," }
    ; { kind = Ident; literal = "y" }
    ; { kind = RParen; literal = ")" }
    ; { kind = LBrace; literal = "{" }
    ; { kind = Ident; literal = "x" }
    ; { kind = Plus; literal = "+" }
    ; { kind = Ident; literal = "y" }
    ; { kind = Semicolon; literal = ";" }
    ; { kind = RBrace; literal = "}" }
    ; { kind = Semicolon; literal = ";" }
    ; { kind = Let; literal = "let" }
    ; { kind = Ident; literal = "result" }
    ; { kind = Assign; literal = "=" }
    ; { kind = Ident; literal = "add" }
    ; { kind = LParen; literal = "(" }
    ; { kind = Ident; literal = "five" }
    ; { kind = Comma; literal = "," }
    ; { kind = Ident; literal = "ten" }
    ; { kind = RParen; literal = ")" }
    ; { kind = Semicolon; literal = ";" }
    ; { kind = EOF; literal = "" }
    ]
  in
  let rec loop index lexer =
    let new_lexer, token = next_token lexer in
    let expected_token = List.nth tokens index in
    match expected_token with
    | Some t ->
      [%test_result: token_kind] token.kind ~expect:t.kind;
      [%test_result: string] token.literal ~expect:t.literal;
      loop (index + 1) new_lexer
    | None -> ()
  in
  create input |> loop 0 |> ignore
;;
