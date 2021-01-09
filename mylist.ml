module Lambda : sig
  open Pattern0

  val __ : ('a, 'a -> 'b, 'b) t

  val nil : (_ list, 'a, 'a) t
  val ( ^:: ) : ('a, 'b, 'c) t -> ('a list, 'c, 'd) t -> ('a list, 'b, 'd) t
  val cons : ('a, 'b, 'c) t -> ('a list, 'c, 'd) t -> ('a list, 'b, 'd) t

  val name : string -> (string, 'a, 'a) t

  val alt : ('a, 'b, 'c) t -> ('a, 'b, 'c) t -> ('a, 'b, 'c) t
  (** Same as [alt] *)
  val ( ||| ) : ('a, 'b, 'c) t -> ('a, 'b, 'c) t -> ('a, 'b, 'c) t

  val map : ('a, 'b, 'c) t -> f:('d -> 'b) -> ('a, 'd, 'c) t
  val ( >>| ) : ('a, 'b, 'c) t -> ('d -> 'b) -> ('a, 'd, 'c) t
  (** Same as [map] *)

  val map_result : ('a, 'b, 'c) t -> f:('c -> 'd) -> ('a, 'b, 'd) t
  val map1 : ('a, 'v1 ->        'b, 'c) t -> f:('v1 ->        'v) -> ('a, 'v -> 'b, 'c) t
  val map0 : ('a,               'b, 'c) t -> f:               'v  -> ('a, 'v -> 'b, 'c) t

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


  let nil =
    T (fun ctx loc x k ->
      match x with
      | [] -> incr_matched ctx; k
      | _  -> fail loc "[]")


  let ( ^:: ) (T f0) (T f1) =
    T (fun ctx loc x k ->
      match x with
      | x0::x1 ->
        incr_matched ctx;
        let k = f0 ctx loc x0 k in
        let k = f1 ctx loc x1 k in
        k
      | _ -> fail loc "::")

  let cons = (^::)

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
  let map_result (T func) ~f = T (fun ctx loc x k -> f (func ctx loc x k))
  let map1 (T func) ~f = T (fun ctx loc x k -> func ctx loc x (fun a   -> k (f a  )))
  let map0 (T func) ~f = T (fun ctx loc x k -> func ctx loc x (           k  f     ))

end

open Lambda


let rec mylength acc xs : int =
  let (c: (string list, int -> 'a, 'a) Pattern0.t) =
    cons (name "1") __ |> map1 ~f:(mylength (acc+1))
  in
  let (n: (string list, int -> 'a, 'a) Pattern0.t) =
    nil |> map0 ~f:0
  in

  parse (c ||| n ) () xs (fun x () -> x) ()


let () = Printf.printf "%d\n"  (mylength 0 ["1"])     (* Prints zero which is wrong *)

let%test _ =
  mylength 0 ["1"; "1"; "1"] = 3

(*
let%test _ = parse (var __) () (Var "asdf") (fun name -> name = "asdf")

let%test _ = parse (app (var __) (var __)) () (App (Var "a", Var "b"))
  (fun l r -> (l="a") && r="b")


let%test _ = parse ((var (name "a") ||| (var (name "b")))) () (Var "a")
  true
let%test _ = parse ((var (name "a") ||| (var (name "b")))) () (Var "b")
  true
 *)
