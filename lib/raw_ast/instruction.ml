type var = Variable.t

type expr = Expression.t

type t =
  | Initialization of var * expr
  | Assignment of string * expr
  | Branching of {predicate: expr; true_branch: t list; false_branch: t list}
  | Loop of {predicate: expr; body: t list}
  | Return of expr
