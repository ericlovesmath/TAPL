include module type of Types

(** [Exercise 6.1.5] Converts to DeBruijn indices *)
val remove_names : context -> Untyped_lambda_calculus.t -> t option

(** [Exercise 6.1.5] Converts from DeBruijn indices *)
val restore_names : context -> t -> Untyped_lambda_calculus.t option

(** [Exercise 7] Evaluates nameless lambda expression with shift/subst *)
val eval : t -> t
