open Core

type 'a t [@@deriving sexp]

module Option : sig
  type 'a value = 'a t
  type 'a t

  val is_none : 'a t -> bool
  val to_option : 'a t -> 'a value option

  module Optional_syntax :
    Optional_syntax_intf.S1 with type 'a t := 'a t with type 'a value = 'a value
end
