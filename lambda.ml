open Syntax;;

let rec pretty_printer = (function
    Var x -> x
|   Lambda(x, t) -> "(λ" ^ x ^ ". " ^ (pretty_printer t) ^ ")"
|   Apply(t1, t2) -> (pretty_printer t1) ^ " " ^ (pretty_printer t2)) 

let freevar term =
    let rec fv bound free = (function
        Var x ->
            if (List.mem x bound) then free
            else 
                if (List.mem x free) then free
                else x::free
    |   Lambda(x, t) ->
            fv (x::bound) free t
    |   Apply(t1, t2) ->
            let f1 = fv bound free t1 in
            let f2 = fv bound free t2 in
            List.fold_left (fun x y -> 
                if (List.mem y f2) then x
                else y::x) f2 f1)
    in
    fv [] [] term

let lookup v c =
    let rec f i v = (function
        x::l ->  if x = v then i
                    else f (i + 1) v l
    |   []      ->  failwith "Look Up Error")
    in
    f 0 v c

let rec removevar context = (function
    Var x ->
        (Index (lookup x context))
|   Lambda(x, t) -> 
        let nt = removevar (x::context) t in
        (Fun nt)
|   Apply(t1, t2) -> 
        let nt1 = (removevar context t1) in
        let nt2 = (removevar context t2) in
        (App (nt1, nt2)))

let restorevar context nameless = 
    let n = List.length context in
    let rec rv c = (function
        Index i -> 
            (Var (List.nth c i))
    |   Fun nt  ->  
            let var = "_" ^ (Char.escaped (Char.chr ((Char.code 'a') + (List.length c) - n))) in
            (Lambda (var, (rv (var::c) nt)))
    |   App(nt1, nt2) ->
            (Apply ((rv c nt1), (rv c nt2))))
    in
    rv context nameless

let rec shift c d = (function
    Index i ->  if i < c then (Index i)
                else (Index (i + d))
|   Fun nt  ->
        (Fun (shift (c + 1) d nt))
|   App(nt1, nt2) ->
        (App ((shift c d nt1), (shift c d nt2))))

let rec subst j s = (function
    Index i ->  if i = j then s else (Index i)
|   Fun nt  ->  (Fun (subst (j + 1) (shift 0 1 s) nt))
|   App(nt1, nt2) -> (App ((subst j s nt1), (subst j s nt2))))

let beta_reduction term =
    let rec br path term =
        if (List.mem term path) || (List.length path) > 100 then (term, path)
        else
            match term with
                Index i -> (Index i, path)
            |   Fun  nt ->
                    let (t, p) = br (term::path) nt in
                    ((Fun t), p)
            |   App(nt1, nt2) ->
                    let (v, p1)    = br (term::path) nt2 in
                    let (nt11, p2) = br p1 nt1 in
                    (match nt11 with
                        Fun nt ->
                            br p2 (shift 0 (-1) (subst 0 (shift 0 1 v) nt))
                    |   _ ->
                            ((App (nt11, v)), p2) )
    in
    fst (br [] term)

let normalizer term =
    let context  = freevar term in
    let nameless = removevar context term in
    restorevar context (beta_reduction nameless)
