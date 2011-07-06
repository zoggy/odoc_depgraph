module type T = sig val g : int -> int end
module Make = functor (P:T) ->
  struct
    let f x = P.g x
    module O = struct let h x = f x end
  end
;;
