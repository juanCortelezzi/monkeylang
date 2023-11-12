type t

(** Creates a lexer based on the input code as a string. *)
val create : string -> t

(** Returns the new lexer with the updated state and the next token in the
    code. *)
val next_token : t -> t * Token.t

(** Returns a string representation of the Lexer and its state. *)
val to_string : t -> string
