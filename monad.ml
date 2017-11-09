open Core.Core_list;;

module type Monad_interf = sig
  type 'a m;;

  val pure:  'a -> 'a m;;
  val bind: 'a m -> ('a -> 'b m) -> 'b m;;
  val (>>=): 'a m -> ('a -> 'b m) -> 'b m;;
  val (>>): 'a m -> 'b m -> 'b m;;
end
;;

module type Monad_utils = sig
  type 'a m;;

  val join: 'a m m -> 'a m;;
  val ap: ('a -> 'b) m -> 'a m -> 'b m;;
  val liftM: ('a -> 'b) -> 'a m -> 'b m;;
  val liftM2: ('a1 -> 'a2 -> 'r) -> 'a1 m -> 'a2 m -> 'r m;;
end
;;

module Monad_extend(M: Monad_interf)
       : (Monad_utils with type 'a m := 'a M.m) = struct
  open M;;

  let join amm = amm >>= (fun am -> am);;

  let ap fm am =
    am >>= (fun a -> fm >>= (fun f -> pure (f a)));;

  let liftM f = ap (pure f);;

  let liftM2 f am bm =
    am >>= (fun a -> bm >>= (fun b -> pure (f a b)));;
end
;;


(* Monad Option and Utils *)
module Monad_option: Monad_interf = struct
  type 'a m = 'a option;;

  let pure a = Some a;;

  let bind am f =
    match am with
    | None   -> None
    | Some a -> f a
  ;;

  let (>>=) = bind;;

  let (>>) am bm = am >>= (fun _ -> bm);;
end
;;

module Monad_option_utils = Monad_extend(Monad_option);;


(* Monad List and Utils *)
module Monad_list: Monad_interf = struct
  type 'a m = 'a list;;

  let pure a = [a];;

  let bind am f =
    List.concat (List.map f am);;

  let (>>=) = bind;;

  let (>>) am bm = am >>= (fun _ -> bm);;
end
;;

module Monad_list_utils = Monad_extend(Monad_list);;
