open Base

type t =
  { input : string
  ; position : int
  ; read_position : int
  ; ch : char option
  }

let print lexer =
  let s = "" in
  let s = s ^ "position: " ^ Int.to_string lexer.position ^ "\n" in
  let s = s ^ "read_pos: " ^ Int.to_string lexer.read_position ^ "\n" in
  let s =
    s
    ^ "ch: "
    ^ (match lexer.ch with
       | Some c -> Char.to_string c
       | None -> "_")
    ^ "\n"
  in
  Stdlib.print_endline s;
  lexer
;;

let read_char lexer =
  let input_len = String.length lexer.input in
  if lexer.read_position >= input_len
  then { lexer with ch = None }
  else (
    let ch = String.get lexer.input lexer.read_position in
    let position = lexer.read_position in
    let read_position = lexer.read_position + 1 in
    { lexer with position; read_position; ch = Some ch })
;;

let next_token lexer =
  let open Token in
  let token =
    match lexer.ch with
    | Some '=' -> { kind = Assign; literal = "=" }
    | Some ';' -> { kind = Semicolon; literal = ";" }
    | Some '(' -> { kind = LParen; literal = "(" }
    | Some ')' -> { kind = RParen; literal = ")" }
    | Some ',' -> { kind = Comma; literal = "," }
    | Some '+' -> { kind = Plus; literal = "+" }
    | Some '{' -> { kind = LBrace; literal = "{" }
    | Some '}' -> { kind = RBrace; literal = "}" }
    | _ -> { kind = EOF; literal = "" }
  in
  read_char lexer, token
;;

let create input = read_char { input; position = 0; read_position = 0; ch = None }

let%test_unit "next_token" =
  let open Token in
  let input = "=_(){},;" in
  let tokens =
    [ { kind = Assign; literal = "=" }
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
  create input |> print |> loop 0 |> ignore
;;
