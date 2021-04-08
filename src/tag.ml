open Core

type 'a t = int [@@deriving sexp]

module Option = struct
  module T = struct
    type 'a value = 'a t
    type 'a t = int

    let is_none t = t = 0
    let unsafe_value t = t
    let to_option t = if is_none t then None else Some (unsafe_value t)
  end

  include T

  module Optional_syntax = struct
    include T

    module Optional_syntax = struct
      let is_none = is_none
      let unsafe_value = unsafe_value
    end
  end
end
