type expression_chain = Value.Expression.chain_t

type t =
  | Initialization of Value.Variable.t * expression_chain
  | Assignment of string * expression_chain
  | Branching of
      {predicate: expression_chain; true_branch: t list; false_branch: t list}
  | Loop of {predicate: expression_chain; body: t list}
  | Return of expression_chain
