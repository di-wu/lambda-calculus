type id = string

type expression =
    | Variable of id (* x *)
    | Abstraction of {
        variable   : id;         (* variable, x *)
        definition : expression; (* function definition, M *)
    } (* (λx.M) *)
    | Application of {
        funct    : expression; (* function, M *)
        argument : expression; (* argument, N *)
    } (* (M N) *)

(* Returns a list of free variables in the given expression.
(* Examples *)
non_bound (Variable "x")
== ["x"]
non_bound (Abstraction {variable = "x"; definition = (Variable "y")})
== ["y"]
*)
let rec non_bound = function
    | Variable name ->
        [name];
    | Abstraction { variable; definition } ->
        List.filter (fun y -> variable <> y) (non_bound definition)
    | Application { funct; argument } ->
        (non_bound funct) @ (non_bound argument)
