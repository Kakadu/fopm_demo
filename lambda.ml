let%test _ = true


type lam = Var of string | App of lam * lam | Abs of string * lam

module Lambda : sig
  open Pattern0

  val __ : ('a, 'a -> 'b, 'b) t
  val abs : (string, 'a, 'b) t -> (lam, 'b, 'c) t -> (lam, 'a, 'c) t
  val app : (lam, 'a, 'b) t -> (lam, 'b, 'c) t -> (lam, 'a, 'c) t
  val var : (string, 'a, 'b) t -> (lam, 'a, 'b) t
  val name : string -> (string, 'a, 'a) t

  val alt : ('a, 'b, 'c) t -> ('a, 'b, 'c) t -> ('a, 'b, 'c) t
  (** Same as [alt] *)
  val ( ||| ) : ('a, 'b, 'c) t -> ('a, 'b, 'c) t -> ('a, 'b, 'c) t

  val map : ('a, 'b, 'c) t -> f:('d -> 'b) -> ('a, 'd, 'c) t
  val ( >>| ) : ('a, 'b, 'c) t -> ('d -> 'b) -> ('a, 'd, 'c) t
  (** Same as [map] *)

  val parse :
    ('a, 'b, 'c) t -> location -> ?on_error:(unit -> 'c) -> 'a -> 'b -> 'c
  (** Matches a value against a pattern. *)
end = struct
  include Pattern0

  let cst ~to_string ?(equal = Stdlib.( = )) v =
    T
      (fun ctx loc x k ->
        if equal x v then (incr_matched ctx; k) else fail loc (to_string v) )

  let name v = cst ~to_string:(Printf.sprintf "%S") v

  let var (T f0) =
    T
      (fun ctx loc x k ->
        match x with
        | Var x0 ->
            incr_matched ctx;
            let k = f0 ctx loc x0 k in
            k
        | _ -> fail loc "var" )

  let app (T f0) (T f1) =
    T
      (fun ctx loc x k ->
        match x with
        | App (x0, x1) ->
            incr_matched ctx;
            let k = f0 ctx loc x0 k in
            let k = f1 ctx loc x1 k in
            k
        | _ -> fail loc "app" )

  let abs (T f0) (T f1) =
    T
      (fun ctx loc x k ->
        match x with
        | Abs (x0, x1) ->
            incr_matched ctx;
            let k = f0 ctx loc x0 k in
            let k = f1 ctx loc x1 k in
            k
        | _ -> fail loc "abs" )

  (* various convenience functions *)

  let alt (T f1) (T f2) =
    T
      (fun ctx loc x k ->
        let backup = save_context ctx in
        try f1 ctx loc x k
        with e1 -> (
          let m1 = save_context ctx in
          restore_context ctx backup;
          try f2 ctx loc x k
          with e2 ->
            let m2 = save_context ctx in
            if m1 >= m2 then (restore_context ctx m1; raise e1) else raise e2 )
        )

  let ( ||| ) = alt

  let many (T f) =
    T
      (fun ctx loc l k ->
        k (ListLabels.map l ~f:(fun x -> f ctx loc x (fun x -> x))) )

  let map (T func) ~f = T (fun ctx loc x k -> func ctx loc x (f k))
  let ( >>| ) t f = map t ~f
end

open Lambda

let%test _ = parse (var __) () (Var "asdf") (fun name -> name = "asdf")

let%test _ = parse (app (var __) (var __)) () (App (Var "a", Var "b"))
  (fun l r -> (l="a") && r="b")


let%test _ = parse ((var (name "a") ||| (var (name "b")))) () (Var "a")
  true
let%test _ = parse ((var (name "a") ||| (var (name "b")))) () (Var "b")
  true
