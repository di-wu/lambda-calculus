module Context = Map.Make (String)

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
