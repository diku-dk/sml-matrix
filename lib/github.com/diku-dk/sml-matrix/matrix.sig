(** Operations on polymorphic matrices.

The Matrix structure defines polymorphic, immutable matrices, implemented with a
combination of pull-arrays and memoization.

*)

signature MATRIX = sig
  type 'a t

  val fromListList   : 'a list list -> 'a t
  val fromVectorList : 'a vector list -> 'a t
  val dimensions     : 'a t -> int * int
  val nCols          : 'a t -> int
  val nRows          : 'a t -> int
  val sub            : 'a t * int * int -> 'a
  val map            : ('a -> 'b) -> 'a t -> 'b t
  val map2           : ('a * 'b -> 'c) -> 'a t -> 'b t -> 'c t
  val listlist       : 'a t -> 'a list list
  val transpose      : 'a t -> 'a t
  val inv            : real t -> real t
  val memoize        : 'a t -> 'a t
  val tabulate       : int * int * (int*int -> 'a) -> 'a t
  val pp             : int -> ('a -> string) -> 'a t -> string
  val ppv            : int -> ('a -> string) -> 'a vector -> string

  val row            : int -> 'a t -> 'a vector
  val col            : int -> 'a t -> 'a vector

  val dot_gen        : ('a * 'a -> 'a) -> ('a * 'a -> 'a) -> 'a -> 'a vector -> 'a vector -> 'a
  val matmul_gen     : ('a * 'a -> 'a) -> ('a * 'a -> 'a) -> 'a -> 'a t -> 'a t -> 'a t
  val matvecmul_gen  : ('a * 'a -> 'a) -> ('a * 'a -> 'a) -> 'a -> 'a t -> 'a vector -> 'a vector

  val dot            : real vector -> real vector -> real
  val matmul         : real t -> real t -> real t
  val matvecmul      : real t -> real vector -> real vector

  val unitmat        : int -> int t
  val onesmat        : int -> int -> int t

  val iota           : int -> int vector
  val ones           : int -> int vector
  val diag           : 'a t -> 'a vector
  val solve          : real t -> real vector -> real vector

  val concatVertical : 'a t -> 'a t -> 'a t option
end

(**

[type 'a t] Type of polymorphic matrices.

[fromListList l] converts the regular list `l` of lists (lists of the same
length) into a matrix. Raises Fail if the list is non-regular.

[fromVectorList l] converts the regular list `l` of vectors (vectors of the same
length) into a matrix. Raises Fail if the list is non-regular.

[solve A b] solves Ax=b and returns x.

*)
