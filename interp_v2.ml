(* open Core *)

(* -------- TYPES ---------------- *)

type exprC = 
    | NumC of int
    | IdC of string
    | AppC of exprC * exprC
    | PlusC of exprC * exprC
    | MultC of exprC * exprC
    | LamC of string * exprC

type value =
    | NumV of int
    | ClosV of string * exprC * env

and binding = 
    | Bind of string * value

and env = binding list

let mt_env = []
let extend_env = List.cons

(* ------------HELPERS----------- *)

let is_lamC = function
    | LamC _ -> true
    | _ -> false

let is_numV = function
    | NumV _ -> true
    | _ -> false

let get_numV = function
    | NumV n -> n
    | _ -> failwith "n is not a numV"

let closV_arg = function
    | ClosV(arg, body, env) -> arg
    | _ -> failwith "Not a ClosV"

let closV_body = function
    | ClosV(arg, body, env) -> body
    | _ -> failwith "Not a ClosV"

let closV_env = function
    | ClosV(arg, body, env) -> env
    | _ -> failwith "Not a ClosV"

let bind_name = function
    | Bind(name, value) -> name

let bind_value = function
    | Bind(name, value) -> value

let print_value = function
    | NumV v -> Printf.printf "%d \n" v
    | ClosV(arg, body, env) -> 
        (* Printf.printf "Argument : (%s)\nBody : (%s)\nEnvironment : (%s)\n" arg body env  *)
        (* Can't print exprC, I need to define a custom print function *)
        Printf.printf "Argument : (%s)\n" arg

(* ------------MAIN HELPERS ----------- *)

let rec lookup x env =
    match env with
    | [] -> failwith "Name not found"
    | head :: tail -> 
        if x = (bind_name head) then
            bind_value head
        else
            lookup x tail

let num_plus l r =
    if is_numV l && is_numV r then
        NumV( get_numV l + get_numV r)
    else
        failwith "One argument was not a number"

let num_mult l r =
    if is_numV l && is_numV r then
        NumV( get_numV l * get_numV r)
    else
        failwith "One argument was not a number"

(* ------------INTERPRETOR ----------- *)

let rec interp expr env = 
    match expr with
    | NumC n -> NumV n
    | IdC n -> lookup n env
    | AppC(f, a) ->
        let f_val = interp f env in
        let expr = closV_body f_val in
        let bind = Bind(closV_arg f_val, interp a env) in
        let env = extend_env bind (closV_env f_val) in
        interp expr env
    | PlusC(l, r) -> num_plus (interp l env) (interp r env)
    | MultC(l, r) -> num_mult (interp l env) (interp r env)
    | LamC(a, b) -> ClosV(a, b, env)

let test1 = 
    let test = PlusC( NumC 24, AppC( LamC( "x", NumC 5), NumC 22)) in
    let fds = mt_env in
    print_value (interp test fds) 

let test2 = 
    let test = AppC( LamC( "a", AppC( 
                                    LamC( "b", 
                                        PlusC(IdC "a", IdC "b"))
                                    , NumC 4))
                    , NumC 3) in
    let fds = mt_env in
    print_value (interp test fds) 

let test3 = 
    let test = AppC( LamC( "sq", MultC(NumC 5, NumC 5)), NumC 3) in 
    let fds = mt_env in
    print_value (interp test fds)

let test4 = 
    let test = AppC( LamC( "sq", AppC( 
                                    LamC( "x", 
                                        MultC(IdC "x", IdC "x"))
                                    , NumC 4))
                    , NumC 3) in
    let fds = mt_env in
    print_value (interp test fds)


