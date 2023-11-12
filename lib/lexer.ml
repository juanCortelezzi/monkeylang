open Base

type t =
  { input : string
  ; position : int
  ; read_position : int
  ; ch : char option
  }

let is_letter c = Char.is_alpha c || Char.equal c '_'

(* Although a simple wrapper for now, it might be extended later. *)
let is_digit c = Char.is_digit c

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

let string_get_opt s i =
  if i >= String.length s then None else Some (String.unsafe_get s i)
;;

let peek_char lexer = string_get_opt lexer.input lexer.read_position

let peek_char_is ~value lexer : bool =
  match peek_char lexer with
  | Some ch -> Char.equal ch value
  | None -> false
;;

let advance lexer =
  let ch = string_get_opt lexer.input lexer.read_position in
  match ch with
  | None -> { lexer with ch = None }
  | Some _ ->
    let position = lexer.read_position in
    let read_position = lexer.read_position + 1 in
    { lexer with position; read_position; ch }
;;

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
  | Some ch ->
    (match ch with
     | '=' when peek_char_is l ~value:'=' -> l |> advance |> advance, { kind = Eq }
     | '=' -> advance l, { kind = Assign }
     | ';' -> advance l, { kind = Semicolon }
     | '(' -> advance l, { kind = LParen }
     | ')' -> advance l, { kind = RParen }
     | ',' -> advance l, { kind = Comma }
     | '+' -> advance l, { kind = Plus }
     | '{' -> advance l, { kind = LBrace }
     | '}' -> advance l, { kind = RBrace }
     | '-' -> advance l, { kind = Minus }
     | '!' when peek_char_is l ~value:'=' -> l |> advance |> advance, { kind = NotEq }
     | '!' -> advance l, { kind = Bang }
     | '/' -> advance l, { kind = Slash }
     | '*' -> advance l, { kind = Asterisk }
     | '<' -> advance l, { kind = LT }
     | '>' -> advance l, { kind = GT }
     | ch when is_letter ch ->
       let lexer, ident = read_while ~f:is_letter l in
       lexer, { kind = lookup_ident ident }
     | ch when is_digit ch ->
       let lexer, ident = read_while ~f:is_digit l in
       lexer, { kind = Int (Int.of_string ident) }
     | ch -> advance l, { kind = Illegal (String.of_char ch) })
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

let%test_unit "test_next_token_with_code_2" =
  let open Token in
  let input =
    {|let five = 5;
let ten = 10;
let add = fn(x, y) {
  x + y;
};
let result = add(five, ten);
!-/*5;
5 < 10 > 5;
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
    ; { kind = Bang }
    ; { kind = Minus }
    ; { kind = Slash }
    ; { kind = Asterisk }
    ; { kind = Int 5 }
    ; { kind = Semicolon }
    ; { kind = Int 5 }
    ; { kind = LT }
    ; { kind = Int 10 }
    ; { kind = GT }
    ; { kind = Int 5 }
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

let%test_unit "test_next_token_with_code_3" =
  let open Token in
  let input =
    {|let five = 5;
let ten = 10;
let add = fn(x, y) {
  x + y;
};

let result = add(five, ten);
!-/*5;
5 < 10 > 5;

if (5 < 10) {
  return true;
} else {
  return false;
}
10 == 10;
10 != 9;
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
    ; { kind = Bang }
    ; { kind = Minus }
    ; { kind = Slash }
    ; { kind = Asterisk }
    ; { kind = Int 5 }
    ; { kind = Semicolon }
    ; { kind = Int 5 }
    ; { kind = LT }
    ; { kind = Int 10 }
    ; { kind = GT }
    ; { kind = Int 5 }
    ; { kind = Semicolon }
    ; { kind = If }
    ; { kind = LParen }
    ; { kind = Int 5 }
    ; { kind = LT }
    ; { kind = Int 10 }
    ; { kind = RParen }
    ; { kind = LBrace }
    ; { kind = Return }
    ; { kind = True }
    ; { kind = Semicolon }
    ; { kind = RBrace }
    ; { kind = Else }
    ; { kind = LBrace }
    ; { kind = Return }
    ; { kind = False }
    ; { kind = Semicolon }
    ; { kind = RBrace }
    ; { kind = Int 10 }
    ; { kind = Eq }
    ; { kind = Int 10 }
    ; { kind = Semicolon }
    ; { kind = Int 10 }
    ; { kind = NotEq }
    ; { kind = Int 9 }
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
