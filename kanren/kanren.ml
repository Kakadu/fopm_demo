module Term = struct
  module Var = struct
    type t = {tag: Obj.t; index: int}

    let reify _reifier {index} = index
  end
end

module Env = struct
  type t = unit

  let the_tag = Obj.repr [|1|]

  (* let () = Format.printf "the_tag = %d\n%!" Obj.(magic @@ the_tag) *)

  let var () x =
    let r = Obj.repr x in
    if Obj.is_block r && Obj.size r = 2 && Obj.field r 0 == the_tag then
      Some (Obj.magic x)
    else None

  let last_one = ref 0

  let fresh () =
    incr last_one;
    let ans = Term.Var.{tag= the_tag; index= !last_one} in
    assert (var () ans <> None);
    ans
end

module Logic : sig
  type ('a, 'b) injected
  type 'a logic = Var of int | Value of 'a

  val lift : 'a -> ('a, 'a) injected
  val inj : ('a, 'b) injected -> ('a, 'b logic) injected
  val fresh : (('a, 'b) injected -> 'c) -> 'c

  module type T1 = sig
    type 'a t

    val fmap : ('a -> 'b) -> 'a t -> 'b t
  end

  module Fmap : functor (T : T1) -> sig
    val distrib : ('a, 'b) injected T.t -> ('a T.t, 'b T.t) injected

    val reify :
         (Env.t -> ('a, 'b) injected -> 'b)
      -> Env.t
      -> ('a T.t, ('b T.t logic as 'r)) injected
      -> 'r

    val cleanup :
         Env.t
      -> (('a T.t, 'b T.t logic) injected as 'i)
      -> ('a, 'b) injected T.t option
  end
end = struct
  type ('a, 'b) injected = 'a
  type 'a logic = Var of int | Value of 'a

  let inj = Fun.id
  let lift = Fun.id

  let fresh f =
    let v = Env.fresh () in
    f (Obj.magic v)

  module type T1 = sig
    type 'a t

    val fmap : ('a -> 'b) -> 'a t -> 'b t
  end

  module Fmap (T : T1) = struct
    external distrib : ('a, 'b) injected T.t -> ('a T.t, 'b T.t) injected
      = "%identity"

    let rec reify r env x =
      match Env.var env x with
      | Some v ->
          let i = Term.Var.reify (reify r env) v in
          Var i
      | None -> Value (T.fmap (r env) x)

    let rec cleanup () x =
      match Env.var () x with Some v -> None | None -> Some (T.fmap Fun.id x)
  end
end

module Alg = struct
  type 'a t = Zero | Succ of 'a

  module F = Logic.Fmap (struct
    type nonrec 'a t = 'a t

    let fmap f = function Zero -> Zero | Succ x -> Succ (f x)
  end)

  type ground = ground t
  type logic = logic t Logic.logic
  type injected = (ground, logic) Logic.injected

  let zero : injected = Logic.inj @@ F.distrib Zero
  let succ p : injected = Logic.inj @@ F.distrib (Succ p)
  let rec reify h n = F.reify reify h n
end

module Test0 = struct
  open Alg

  let z = Alg.zero
  let one : Alg.injected = succ zero
  let two : Alg.injected = succ (succ zero)
  let gt2 : Alg.injected = Logic.fresh (fun q -> succ (succ q))

  let%test _ = reify () z = Value Zero
  let%test _ = reify () two = Value (Succ (Value (Succ (Value Zero))))
  let%test _ = reify () gt2 = Value (Succ (Value (Succ (Var 1))))
end

module PeanoPatterns : sig
  open Pattern0

  val __ : ('a, 'a -> 'b, 'b) t
  val __any : ('a, 'b, 'b) t
  val zero : (Alg.injected, 'a, 'a) t
  val succ : (Alg.injected, 'a, 'b) t -> (Alg.injected, 'a, 'b) t
end = struct
  open Pattern0

  let __ = Pattern0.__
  let __any = Pattern0.__any

  let zero =
    T
      (fun ctx loc x k ->
        match Alg.F.cleanup () x with
        | None | Some (Succ _) -> fail loc "succ"
        | Some Zero -> incr_matched ctx; k )

  let succ (T next) =
    T
      (fun ctx loc (x : Alg.injected) k ->
        match Alg.F.cleanup () x with
        | None | Some Zero -> fail loc "succ"
        | Some (Succ x0) -> incr_matched ctx; next ctx loc x0 k )
end

module Test1 = struct
  open Pattern0
  module PP = PeanoPatterns

  let%test _ = parse PP.zero () Test0.z true
  let%test _ = parse PP.(succ zero) () Test0.one true
  let%test _ = parse PP.(succ @@ succ zero) () Test0.two true

  let%test _ =
    PP.(parse (succ @@ succ __) ()) Test0.gt2 (fun (_ : Alg.injected) -> true)

  let%test _ = PP.(parse (succ __any) ()) Test0.gt2 true

  (* Next tests should fail *)
  let%test _ =
    PP.(parse (succ __any) ()) Test0.z false ~on_error:(fun () -> true)
end
