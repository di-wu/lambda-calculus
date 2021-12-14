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

(* Returns a list of free variables in the given expression. *)
let rec non_bound = function
    | Variable name ->
        [name];
    | Abstraction { variable; definition } ->
        List.filter (fun y -> variable <> y) (non_bound definition)
    | Application { funct; argument } ->
        (non_bound funct) @ (non_bound argument)

(* Returns a new variable name: v{counter} *)
let new_name =
    let x = ref 0 in
    fun () ->
        let counter = !x in
        incr x;
        "v" ^ (string_of_int counter)

(* Substitutes occurrences of var with value in expr. *)
let rec substitute expr var value =
    match expr with
        | Variable name ->
            if name = var then value else expr
        | Abstraction { variable; definition } ->
            if var = variable then
                Abstraction { variable; definition }
            else if not (List.mem variable (non_bound value)) then
                Abstraction {
                    variable;
                    definition = substitute definition var value;
                }
            else (* α-conversion, (λx.M[x]) → (λy.M[y]) *)
                let v = new_name() in
                let e = substitute definition variable (Variable v) in
                Abstraction {
                    variable   = v;
                    definition = substitute e var value;
                }
        | Application { funct; argument } ->
            Application {
                funct    = substitute funct var value;
                argument = substitute argument var value;
            }

(* Replaces the bound variables with the argument expression in the body of the abstraction (β-reduction). *)
let rec reduce expr =
    match expr with
        | Abstraction { variable; definition } ->
            Abstraction {
                variable;
                definition = reduce definition;
            };
        | Application {
            funct = Abstraction { variable; definition };
            argument;
        } -> substitute definition variable argument;
        | Application { funct; argument } ->
            let funct' = reduce funct in
            if funct' != funct then Application {
                funct = funct';
                argument;
            } else Application {
                funct;
                argument = reduce argument;
            };
        | _ -> expr; (* Nothing to reduce... *)
