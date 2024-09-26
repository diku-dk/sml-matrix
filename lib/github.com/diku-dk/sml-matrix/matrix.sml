
structure Matrix :> MATRIX = struct

  fun die s = raise Fail ("Matrix: " ^ s)

  type 'a t = {rows:int,cols:int,get: (int * int) -> 'a}

  fun tabulate (r,c,f) : 'a t =
      if r >=0 andalso c >= 0 then {rows=r,cols=c,get=f}
      else die "tabulate: number of rows and number of columns must be positive or zero"

  fun fromVectorList (a: 'a vector list) : 'a t =
      let val v = Vector.fromList a
          val rows = Vector.length v
          val cols =
              case Vector.foldl (fn (r,SOME n) =>
                                    let val c = Vector.length r
                                    in if c = n then SOME n
                                       else die "fromVectorList: regularity required"
                                    end
                                  | (r,NONE) => SOME(Vector.length r)) NONE v of
                  SOME n => n
                | NONE => 0
      in {rows=rows,
          cols=cols,
          get=fn(r,c) => Vector.sub(Vector.sub(v,r),c)}
      end

  fun fromListList (a: 'a list list) : 'a t =
      fromVectorList (map Vector.fromList a)

  fun memoize ({rows,cols,get}:'a t) : 'a t =
      let val arr = Vector.tabulate (rows*cols, fn i =>
                                                  let val r = i div cols
                                                      val c = i mod cols
                                                  in get (r,c)
                                                  end)
      in {rows=rows,cols=cols,get=fn (r,c) => Vector.sub (arr,r*cols+c)}
      end

  fun for (n,f) =
      if n > 0 then
        let val n = n-1
        in (f n; for(n,f))
        end
      else ()

  fun materialize ({rows,cols,get}:'a t) (arr: 'a array) : 'a t =
      if Array.length arr <> rows*cols then die "materialize: incompatiple array target size"
      else (for (rows, fn r => for (cols, fn c => Array.update(arr,r*cols+c,get(r,c))));
            {rows=rows,cols=cols,get=fn (r,c) => Array.sub (arr,r*cols+c)})

  fun dimensions ({rows,cols,...}: 'a t) = (rows,cols)
  fun nRows ({rows,...}: 'a t) = rows
  fun nCols ({cols,...}: 'a t) = cols
  fun sub ({rows,cols,get}:'a t, r, c) =
      if r >= rows orelse r < 0 then raise Subscript
      else if c >= cols orelse c < 0 then raise Subscript
      else get (r,c)
  fun map (f: 'a -> 'b) ({cols,rows,get} : 'a t) : 'b t =
      {cols=cols,rows=rows,get=f o get}
  fun map2 (f: 'a * 'b -> 'c)
           ({cols=colsA,rows=rowsA,get=getA}: 'a t)
           ({cols=colsB,rows=rowsB,get=getB}: 'b t) : 'c t =
      if colsA <> colsB orelse rowsA <> rowsB then raise Subscript
      else {rows=rowsA,cols=colsA,get=fn i => f (getA i, getB i)}

  fun transpose ({cols,rows,get}:'a t) : 'a t =
      {cols=rows,rows=cols,get = get o (fn (i,j) => (j,i))}

  fun listlist ({cols,rows,get}: 'a t) : 'a list list =
      let fun row r c acc = if c < 0 then acc
                            else row r (c-1) (get(r,c)::acc)
          fun col r acc = if r < 0 then acc
                          else col (r-1) (row r (cols-1) nil :: acc)
      in col (rows-1) nil
      end

  fun take (i:int) ({rows,cols,get}:'a t) : 'a t =
      if i > rows orelse i < 0 then die "take:incompatible number"
      else {rows=i,cols=cols,get=get}

  fun drop (i:int) ({rows,cols,get}:'a t) : 'a t =
      if i > rows orelse i < 0 then die "take:incompatible number"
      else {rows=rows-i,cols=cols,get=fn(r,c) => get(r+i,c)}

  fun gauss_jordan (A: real t) : real t =
      let val n = nRows A
          val m = nCols A
          val arr = Array.array(n*m, 0.0)
          val A = materialize A arr
          fun loop i =
              if i >= n then A
              else
                let val irow = take 1 A
                    val v1 = sub (irow,0,i)
                    val irow = memoize (map (fn x => x/v1) irow)
                    val Ap = memoize (drop 1 A) (* copy *)
                    val () = for (n-1, fn r =>
                                          let val scale = sub(Ap,r,i)
                                          in for (m,fn c =>
                                                       let val x = sub(irow,0,c)
                                                           val y = sub(Ap,r,c)
                                                           val v = y - scale * x
                                                       in Array.update(arr,r*m+c,v)
                                                       end)
                                          end)
                    val () = for (m,fn c => Array.update(arr,(n-1)*m+c,sub(irow,0,c)))
                in loop (i+1)
                end
      in loop 0
      end

  fun inv (A: real t) : real t =
      let val n = nRows A
      in if n <> nCols A then die "inv: NxN matrix required"
         else
           let (* Pad the matrix with the identity matrix *)
               val B = {rows=n,cols=2*n,
                        get=fn (r,c) =>
                               if c < n then sub(A,r,c)
                               else if c = n+r then 1.0 else 0.0}
               val C = gauss_jordan B
               (* Drop the identity matrix at the front *)
           in transpose (drop n (transpose C))
           end
      end

  fun pad n s =
      if size s >= n then s
      else pad n (" " ^ s)

  fun pp (n:int) (p: 'a -> string) (a:'a t) : string =
      let val {rows,cols,get} = map p a
          fun row r c acc = if c < 0 then acc
                            else row r (c-1) (pad n (get (r,c))::acc)
          fun col r acc = if r < 0 then acc
                          else col (r-1) (String.concatWith " " (row r (cols-1) nil) :: acc)
      in String.concatWith "\n" (col (rows-1) nil)
      end

  fun row (r:int) ({rows,cols,get}:'a t) : 'a vector =
      Vector.tabulate(cols,fn i => get(r,i))

  fun col (c:int) (a:'a t) = row c (transpose a)

  fun dot_gen (f: 'a * 'a -> 'a) (g: 'a * 'a -> 'a) (ne:'a) (a:'a vector) (b:'a vector) : 'a =
      if Vector.length a <> Vector.length b then die "dot: incompatible lengths"
      else
        let val v = Vector.mapi (fn (i,x) =>
                                    let val y = Vector.sub(b,i)
                                    in f(x,y)
                                    end) a
        in Vector.foldl g ne v
        end

  fun matmul_gen (f:'a * 'a -> 'a) (g:'a * 'a -> 'a) (ne:'a) (a:'a t) (b:'a t) : 'a t =
      if nCols a <> nRows b then die "matmul: incompatible matrixes"
      else let val rows = nRows a
               val cols = nCols b
               fun get (r,c) =
                   let val x = row r a
                       val y = col c b
                   in dot_gen f g ne x y
                   end
           in {rows=rows,cols=cols,get=get}
           end

  fun matvecmul_gen (f:'a * 'a -> 'a) (g:'a * 'a -> 'a) (ne:'a) (a:'a t) (b:'a vector) : 'a vector =
      if nCols a <> Vector.length b then die "matvecmul: incompatible matrix and vector"
      else Vector.tabulate (nRows a, fn r => dot_gen f g ne (row r a) b)

  fun ppv (n:int) (p:'a -> string) (a:'a vector) : string =
      let val v = Vector.map (pad n o p) a
      in String.concatWith " " (Vector.foldr op:: nil v)
      end

  fun dot a b = dot_gen op* op+ 0.0 a b
  fun matvecmul a v = matvecmul_gen op* op+ 0.0 a v
  fun matmul a b = matmul_gen op* op+ 0.0 a b

  (* solves Ax=b *)
  fun solve (X: real t) (b: real vector): real vector =
    matvecmul (matmul (inv (matmul (transpose X) X)) (transpose X)) b

  fun unitmat (n:int) : int t = {rows=n,cols=n,get=fn(r,c) => if r=c then 1 else 0}

  fun onesmat (rows:int) (cols:int) : int t = {rows=rows,cols=cols,get=fn _ => 1}

  fun iota n = Vector.tabulate(n,fn x => x)

  fun ones n = Vector.tabulate(n,fn _ => 1)

  fun diag ({rows,cols,get} : 'a t) : 'a vector =
      if rows <> cols then die "diag: square matrix required"
      else Vector.tabulate(rows,fn x => get(x,x))

  fun concatVertical { rows = rows1, cols = cols1, get = get1 }
                     { rows = rows2, cols = cols2, get = get2 } =
    if cols1 = cols2 then
        let fun get (i, j) = if i < rows1 then
                                 get1 (i, j)
                             else
                                 get2 (i - rows1, j)
        in SOME { rows = rows1 + rows2, cols = cols1, get = get }
        end
    else NONE

end
