(** Mutablity modifier for variable, attribute and method *)
type t =
  | Mutable
      (** A variable than can be updated at anytime. A method will update the
          state of the object the *)
  | Frozen
      (** A variable that can't be updated since runtime. A method will not
          update the state of the object. *)
  | Static
      (** A variable that can't be updated since compile time. A method will not
          update the state of the object since the compile time if the object is
          static. *)
