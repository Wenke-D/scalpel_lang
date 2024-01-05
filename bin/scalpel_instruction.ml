type expression_chain = Scalpel_value.expression_chain

type definition =
  | Initialization of Scalpel_value.symbol * expression_chain
  | Assignment of string * expression_chain
  | Branching of
      { predicate: expression_chain
      ; true_branch: definitions
      ; false_branch: definitions }
  | Loop of {predicate: expression_chain; body: definitions}

and definitions = definition list
