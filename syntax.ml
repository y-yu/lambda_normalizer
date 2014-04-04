type lambda_term =
    Var    of varname
|   Lambda of varname * lambda_term
|   Apply  of lambda_term * lambda_term
and
    varname = string

type nameless_term =
    Index of int
|   Fun   of nameless_term
|   App   of nameless_term * nameless_term
