module M = M3.Make (struct let g x = x + 1 end);;
let foo = M.O.h;;
