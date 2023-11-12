open Base

type t =
  { input : string
  ; position : int
  ; read_position : int
  ; ch : char option
  }

let to_string lexer =
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
  s
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
  | Some '=' -> advance l, { kind = Assign }
  | Some ';' -> advance l, { kind = Semicolon }
  | Some '(' -> advance l, { kind = LParen }
  | Some ')' -> advance l, { kind = RParen }
  | Some ',' -> advance l, { kind = Comma }
  | Some '+' -> advance l, { kind = Plus }
  | Some '{' -> advance l, { kind = LBrace }
  | Some '}' -> advance l, { kind = RBrace }
  | Some ch when is_letter ch ->
    let lexer, ident = read_while ~f:is_letter l in
    lexer, { kind = lookup_ident ident }
  | Some ch when is_digit ch ->
    let lexer, ident = read_while ~f:is_digit l in
    lexer, { kind = Int (Int.of_string ident) }
  | Some _ -> advance l, { kind = Illegal (char_to_literal l.ch) }
  | None -> advance l, { kind = EOF }
;;

let create input = advance { input; position = 0; read_position = 0; ch = None }

let%test_unit "test_next_token_with_tokens" =
  let open Token in
  let input = "=+(){},;" in
  let tokens =
    [ { kind = Assign }
    ; { kind = Plus }
    ; { kind = LParen }
    ; { kind = RParen }
    ; { kind = LBrace }
    ; { kind = RBrace }
    ; { kind = Comma }
    ; { kind = Semicolon }
    ; { kind = EOF }
    ]
  in
  let rec loop index lexer =
    let new_lexer, token = next_token lexer in
    let expected_token = List.nth tokens index in
    match expected_token with
    | Some t ->
      [%test_result: token_kind] token.kind ~expect:t.kind;
      loop (index + 1) new_lexer
    | None -> ()
  in
  create input |> loop 0 |> ignore
;;

let%test_unit "test_next_token_with_code" =
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
    [ { kind = Let }
    ; { kind = Ident "five" }
    ; { kind = Assign }
    ; { kind = Int 5 }
    ; { kind = Semicolon }
    ; { kind = Let }
    ; { kind = Ident "ten" }
    ; { kind = Assign }
    ; { kind = Int 10 }
    ; { kind = Semicolon }
    ; { kind = Let }
    ; { kind = Ident "add" }
    ; { kind = Assign }
    ; { kind = Function }
    ; { kind = LParen }
    ; { kind = Ident "x" }
    ; { kind = Comma }
    ; { kind = Ident "y" }
    ; { kind = RParen }
    ; { kind = LBrace }
    ; { kind = Ident "x" }
    ; { kind = Plus }
    ; { kind = Ident "y" }
    ; { kind = Semicolon }
    ; { kind = RBrace }
    ; { kind = Semicolon }
    ; { kind = Let }
    ; { kind = Ident "result" }
    ; { kind = Assign }
    ; { kind = Ident "add" }
    ; { kind = LParen }
    ; { kind = Ident "five" }
    ; { kind = Comma }
    ; { kind = Ident "ten" }
    ; { kind = RParen }
    ; { kind = Semicolon }
    ; { kind = EOF }
    ]
  in
  let rec loop index lexer =
    let new_lexer, token = next_token lexer in
    let expected_token = List.nth tokens index in
    match expected_token with
    | Some t ->
      [%test_result: token_kind] token.kind ~expect:t.kind;
      loop (index + 1) new_lexer
    | None -> ()
  in
  create input |> loop 0 |> ignore
;;
