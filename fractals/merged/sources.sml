structure Array =
   struct
      open Array

      fun foldi (a, b, f) = Array.foldli f b a

      fun fold (a, b, f) = Array.foldl f b a

      fun foreach (a, f) = Array.app f a

      fun foreachi (a, f) = appi f a

      fun forall (a, f) = Array.all f a

      val new = array

      fun modify (a, f) = Array.modify f a
   end
(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
type int = Int.t
type word = Word.t

signature HASH_TABLE =
   sig
      type ('a, 'b) t

      val fold: ('a, 'b) t * 'c * ('a * 'b * 'c -> 'c) -> 'c
      val foreach: ('a, 'b) t * ('a * 'b -> unit) -> unit
      val insert: ('a, 'b) t * 'a * 'b -> unit
      val lookup: ('a, 'b) t * 'a -> 'b
      val lookupOrInsert: ('a, 'b) t * 'a * (unit -> 'b) -> 'b
      val new: {equals: 'a * 'a -> bool,
		hash: 'a -> word,
		size: int} -> ('a, 'b) t
      val peek: ('a, 'b) t * 'a -> 'b option
   end

(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
structure HashTable: HASH_TABLE =
struct

structure Buckets =
   struct
      datatype ('a, 'b) t =
	 Cons of {hash: word,
		  key: 'a,
		  rest: ('a, 'b) t,
		  value: 'b}
       | Nil

      val empty = Nil

      val fold: ('a, 'b) t * 'c * (word * 'a * 'b * 'c -> 'c) -> 'c =
	 fn (bucket, c, f) =>
	 let
	    fun loop (bucket, c) =
	       case bucket of
		  Cons {hash, key, rest, value} =>
		     loop (rest, f (hash, key, value, c))
		| Nil => c
	 in
	    loop (bucket, c)
	 end

      val foreach: ('a, 'b) t * (word * 'a * 'b -> unit) -> unit =
	 fn (bucket, f) =>
	 fold (bucket, (), fn (w, a, b, ()) => f (w, a, b))
   end

datatype ('a, 'b) t =
   T of {buckets: ('a, 'b) Buckets.t array ref,
	 equals: 'a * 'a -> bool,
	 hash: 'a -> word,
	 mask: word ref,
	 numItems: int ref}
   
fun new {equals, hash, size} =
   let
      val numBuckets = 0w4 * Word.roundUpToPowerOfTwo (Word.fromInt size)
      val mask: word = numBuckets - 0w1
   in
      T {buckets = ref (Array.new (Word.toInt numBuckets, Buckets.empty)),
	 equals = equals,
	 hash = hash,
	 numItems = ref 0,
	 mask = ref mask}
   end

fun size (T {numItems, ...}) = !numItems

fun index (w: word, mask: word): int =
   Word.toInt (Word.andb (w, mask))
   
fun resize (T {buckets, hash, mask, ...}, size: int, newMask: word): unit =
   let
      val newBuckets = Array.new (size, Buckets.empty)
   in
      Array.foreach (!buckets, fn r =>
		     Buckets.foreach (r, fn (hash, key, value) =>
				      let
					 val j = index (hash, newMask)
				      in
					 Array.update
					 (newBuckets, j,
					  Buckets.Cons
					  {hash = hash,
					   key = key,
					   rest = Array.sub (newBuckets, j),
					   value = value})
				      end))
      ; buckets := newBuckets
      ; mask := newMask
   end
   	       
fun maybeGrow (s as T {buckets, mask, numItems, ...}): unit =
   let
      val n = Array.length (!buckets)
   in
      if !numItems * 4 >= n
	 then resize (s,
		      n * 2,
		      (* The new mask depends on growFactor being 2. *)
		      Word.orb (0w1, Word.<< (!mask, 0w1)))
      else ()
   end

fun peekGen (T {buckets = ref buckets, equals, hash, mask, ...}, a, no, yes) =
   let
      val w = hash a
      val j = index (w, !mask)
      val b = Array.sub (buckets, j)
      val rec peek =
	 fn Buckets.Nil => no (w, j, b)
	  | Buckets.Cons {hash, key, rest, value} =>
	       if hash = w andalso equals (key, a)
		  then yes value
	       else peek rest
   in
      peek b
   end

fun peek (t, a) = peekGen (t, a, fn _ => NONE, SOME)

fun lookupOrInsert (table as T {buckets, numItems, ...}, key, makeValue) =
   let
      fun no (hash, j, bucket) =
	 let
	    val value = makeValue ()
	    val _ = Int.inc numItems
	    val _ = Array.update (!buckets, j,
				  Buckets.Cons {hash = hash,
						key = key,
						rest = bucket,
						value = value})
	    val _ = maybeGrow table
	 in
	    value
	 end
      fun yes value = value
   in
      peekGen (table, key, no, yes)
   end

fun insert (t, a, b) =
   ignore (lookupOrInsert (t, a, fn () => b))

fun lookup (t, a) =
   lookupOrInsert (t, a, fn () => raise Fail "HashTable.insert")

fun fold (T {buckets, ...}, c, f) =
   Array.fold (!buckets, c, fn (bucket, c) =>
	       Buckets.fold (bucket, c, fn (_, a, b, c) => f (a, b, c)))

fun foreach (T {buckets, ...}, f) =
   Array.foreach (!buckets, fn b =>
		  Buckets.foreach (b, fn (_, a, b) => f (a, b)))

end
(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
type int = Int.t

signature INSERTION_SORT =
   sig
      (* The comparison function ('a * 'a -> bool) for should be the <= funtion,
       * not just <.
       * This is necessary to handle duplicate elements.
       *)
      val sort: 'a array * ('a * 'a -> bool) -> unit
   end

(* Copyright (C) 1999-2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
structure InsertionSort: INSERTION_SORT =
struct

open Array

(* Based on page 108 of Programming Pearls, by Bentley. *)
fun sort (a: 'a array, op <= : 'a * 'a -> bool): unit =
   let
      fun x i = sub (a, i)
      val _ =
	 Int.for
	 (1, Array.length a, fn i =>
	  let
	     val t = x i
	     fun sift (j: int) =
		if j > 0
		   then
		      let
			 val j' = j - 1
			 val z = x j'
		      in
			 if z <= t
			    then j
			 else (update (a, j, z)
			       ; sift j')
		      end
		else j
	     val _ = update (a, sift i, t)
	  in ()
	  end)
   in
      ()
   end

end
structure Int =
   struct
      open Int

      type t = int

      fun for (start, stop, f) =
	 let
	    fun loop i = if i = stop then () else (f i; loop (i + 1))
	 in
	    loop start
	 end
      
      fun forDown (lo, hi, f) =
	 let
	    fun loop i =
	       let
		  val i = i - 1
	       in
		  if i < lo
		     then ()
		  else (f i; loop i)
	       end
	 in
	    loop hi
	 end

      fun exists (start: t, stop: t, f: t -> bool): bool =
	 let
	    fun loop i = i < stop andalso (f i orelse loop (i + 1))
	 in
	    loop start
	 end

      fun forall (start, stop, f) = not (exists (start, stop, not o f))

      fun fold (start: t, stop: t, a: 'a, f: t * 'a -> 'a): 'a =
	 let
	    fun loop (i: t, a: 'a) =
	       if i >= stop
		  then a
	       else loop (i + 1, f (i, a))
	 in loop (start, a)
	 end

      fun dec r = r := !r - 1
      fun inc r = r := !r + 1
   end

val for = Int.for
structure List =
   struct
      open List

      type 'a t = 'a list

      fun peek (l, f) = List.find f l
	 
      fun fold (l, b, f) = List.foldl f b l

      fun foreach (l, f) = List.app f l

      fun forall (l, f) = List.all f l

      fun appendRev (l1, l2) = fold (l1, l2, op ::)
	 
      fun removeFirst (l, f) =
	 let
	    fun loop (l, ac) =
	       case l of
		  [] => raise Fail "removeFirst"
		| x :: l =>
		     if f x
			then appendRev (ac, l)
		     else loop (l, x :: ac)
	 in
	    loop (l, [])
	 end
   end
(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
signature QUICK_SORT =
   sig
      (* The comparison function ('a * 'a -> bool) for should be the <= funtion,
       * not just <.
       * This is necessary to handle duplicate elements.
       *)
      (* sortArray mutates the array it is passed and returns the same array *)
      val sortArray: 'a array * ('a * 'a -> bool) -> 'a array
   end

(* Copyright (C) 1999-2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
structure QuickSort: QUICK_SORT =
struct

open Array

type int = Int.int

val rand = Word.toIntX o MLton.Random.rand

fun randInt (lo, hi) = lo + Int.mod (rand(), hi - lo + 1)

(* quicksort based on section 10.2 of Programming Pearls, by Bentley.
 * It does repeated partitioning until the segment size is less than the cutoff.
 * Then, it does an insertion sort over the whole array to fix up the unsorted
 * segments.
 *)
fun 'a sortArray (a: 'a array, op <= : 'a * 'a -> bool): 'a array =
   if 0 = Array.length a
      then a
   else
      let
	 fun x i = sub (a, i)
	 fun swap (i, j) =
	    let
	       val t = x i
	       val () = update (a, i, x j)
	       val () = update (a, j, t)
	    in
	       ()
	    end
	 val cutoff = 20
	 fun qsort (l: int, u: int): unit =
	    if Int.<= (u - l, cutoff)
	       then ()
	    else
	       let
		  val _ = swap (l, randInt (l, u))
		  val t = x l
		  (* Partition based on page 115. *)
		  fun loop (i, j) =
		     let
			fun loopUp i =
			   let
			      val i = i + 1
			   in
			      (* The sentinel guarantees that x i is OK. *)
			      if t <= x i
				 then i
			      else loopUp i
			   end
			val i = loopUp i
			fun loopDown j =
			   let
			      val j = j - 1
			   in
			      if x j <= t
				 then j
			      else loopDown j
			   end
			val j = loopDown j
		     in
			if j < i
			   then (i, j)
			else (swap (i, j); loop (i, j))
		     end
		  val (i, j) = loop (l, u + 1)
		  val () = swap (l, j)
		  val () = qsort (l, j - 1)
		  val () = qsort (i, u)
	       in
		  ()
	       end
	 (* Put a maximal element at the end to use as a sentinel. *)
	 val (m, _) =
	    Array.foldi
	    (a, (0, Array.sub (a, 0)), fn (i, xi, (m, xm)) =>
	     if xi <= xm
		then (m, xm)
	     else (i, xi))
	 val last = length a - 1
	 val () = swap (m, last)
	 val _ = qsort (0, last - 1)
	 val _ = InsertionSort.sort (a, op <=)
      in
	 a
      end
   
end
structure String =
   struct
      open String

      type t = string

      fun fold (s, b, f) = CharVector.foldl f b s

      fun map (s, f) = String.map f s
	 
      val tabulate = CharVector.tabulate
	 
      fun toUpper s = map (s, Char.toUpper)

      fun hash s =
	 fold (s, 0w0, fn (c, h) =>
	       Word.<< (h, 0w5) + h + 0w720 + Word.fromChar c)
   end	 

structure Word =
   struct
      open Word
	 
      type t = word

      val fromChar = fromInt o Char.ord

      fun log2 (w: t): t =
	 if w = 0w0
	    then raise Fail "Word.log2 0"
	 else
	    let
	       fun loop (n, s, ac): word =
		  if n = 0w1
		     then ac
		  else
		     let
			val (n, ac) =
			   if n >= << (0w1, s)
			      then (>> (n, s), ac + s)
			   else (n, ac)
		     in
			loop (n, >> (s, 0w1), ac)
		     end
	    in
	       loop (w, 0w16, 0w0)
	    end

      fun roundDownToPowerOfTwo (w: t) = << (0w1, log2 w)

      fun roundUpToPowerOfTwo w =
	 let
	    val w' = roundDownToPowerOfTwo w
	 in
	    if w = w'
	       then w
	    else w' * 0w2
	 end
   end
(* array-qsort.sml
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * Structure for in-place sorting of polymorphic arrays.
 * Uses an engineered version of quicksort due to 
 * Bentley and McIlroy.
 *
 *)

structure ArrayQSort : ARRAY_SORT =
  struct

    structure A = Array

    type 'a array = 'a A.array

    val sub = Unsafe.Array.sub
    val update = Unsafe.Array.update

    fun isort (array, start, n, cmp) = let
          fun item i = sub(array,i)
          fun swap (i,j) = let 
                val tmp = sub(array,i)
                in update(array,i,sub(array,j)); update(array,j,tmp) end
          fun vecswap (i,j,0) = ()
            | vecswap (i,j,n) = (swap(i,j);vecswap(i+1,j+1,n-1))
          fun insertSort (start, n) = let
                val limit = start+n
                fun outer i =
                      if i >= limit then ()
                      else let
                        fun inner j =
                              if j = start then outer(i+1)
                              else let
                                val j' = j - 1
                                in
                                  if cmp(item j',item j) = GREATER
                                    then (swap(j,j'); inner j')
                                    else outer(i+1)
                                end
                        in inner i end
                in
                  outer (start+1)
                end
          in insertSort (start, n); array end

    fun sortRange (array, start, n, cmp) = let
          fun item i = sub(array,i)
          fun swap (i,j) = let 
                val tmp = sub(array,i)
                in update(array,i,sub(array,j)); update(array,j,tmp) end
          fun vecswap (i,j,0) = ()
            | vecswap (i,j,n) = (swap(i,j);vecswap(i+1,j+1,n-1))
          fun insertSort (start, n) = let
                val limit = start+n
                fun outer i =
                      if i >= limit then ()
                      else let
                        fun inner j =
                              if j = start then outer(i+1)
                              else let
                                val j' = j - 1
                                in
                                  if cmp(item j',item j) = GREATER
                                    then (swap(j,j'); inner j')
                                    else outer(i+1)
                                end
                        in inner i end
                in
                  outer (start+1)
                end

          fun med3(a,b,c) = let
		val a' = item a and b' = item b and c' = item c
		in
		  case (cmp(a', b'),cmp(b', c'))
		   of (LESS, LESS) => b
		    | (LESS, _) => (
			case cmp(a', c') of LESS => c | _ => a)
		    | (_, GREATER) => b
                    | _ => (case cmp(a', c') of LESS => a | _ => c)
		  (* end case *)
		end

          fun getPivot (a,n) = 
                if n <= 7 then a + n div 2
                else let
                  val p1 = a
                  val pm = a + n div 2
                  val pn = a + n - 1
                  in
                    if n <= 40 then med3(p1,pm,pn)
                    else let
                      val d = n div 8
                      val p1 = med3(p1,p1+d,p1+2*d)
                      val pm = med3(pm-d,pm,pm+d)
                      val pn = med3(pn-2*d,pn-d,pn)
                      in
                        med3(p1,pm,pn)
                      end
                  end
          
          fun quickSort (arg as (a, n)) = let
                fun bottom limit = let
                      fun loop (arg as (pa,pb)) =
                            if pb > limit then arg
                            else case cmp(item pb,item a) of
                              GREATER => arg
                            | LESS => loop (pa,pb+1)
                            | _ => (swap arg; loop (pa+1,pb+1))
                      in loop end
      
                fun top limit = let
                      fun loop (arg as (pc,pd)) =
                            if limit > pc then arg
                            else case cmp(item pc,item a) of
                              LESS => arg
                            | GREATER => loop (pc-1,pd)
                            | _ => (swap arg; loop (pc-1,pd-1))
                      in loop end

                fun split (pa,pb,pc,pd) = let
                      val (pa,pb) = bottom pc (pa,pb)
                      val (pc,pd) = top pb (pc,pd)
                      in
                        if pb > pc then (pa,pb,pc,pd)
                        else (swap(pb,pc); split(pa,pb+1,pc-1,pd))
                      end

                val pm = getPivot arg
                val _ = swap(a,pm)
                val pa = a + 1
                val pc = a + (n-1)
                val (pa,pb,pc,pd) = split(pa,pa,pc,pc)
                val pn = a + n
                val r = Int.min(pa - a, pb - pa)
                val _ = vecswap(a, pb-r, r)
                val r = Int.min(pd - pc, pn - pd - 1)
                val _ = vecswap(pb, pn-r, r)
                val n' = pb - pa
                val _ = if n' > 1 then sort(a,n') else ()
                val n' = pd - pc
                val _ = if n' > 1 then sort(pn-n',n') else ()
                in () end

          and sort (arg as (_, n)) = if n < 7 then insertSort arg 
                                     else quickSort arg
          in sort (start,n) end

    fun sort cmp array = sortRange(array, 0, A.length array, cmp)

    fun sorted cmp array = let
          val len = A.length array
          fun s (v,i) = let
                val v' = sub(array,i)
                in
                  case cmp(v,v') of
                    GREATER => false
                  | _ => if i+1 = len then true else s(v',i+1)
                end
          in
            if len = 0 orelse len = 1 then true
            else s(sub(array,0),1)
          end

  end (* ArraySort *)

(* array-sort-sig.sml
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * Signature for in-place sorting of polymorphic arrays
 *
 *)

signature ARRAY_SORT =
  sig

    type 'a array

    val sort   : ('a * 'a -> order) -> 'a array -> unit
    val sorted : ('a * 'a -> order) -> 'a array -> bool

  end (* ARRAY_SORT *)

(* hash-string.sml
 *
 * COPYRIGHT (c) 1992 by AT&T Bell Laboratories
 *)

structure HashString : sig

    val hashString : string -> word

  end = struct

    fun charToWord c = Word.fromInt(Char.ord c)

  (* A function to hash a character.  The computation is:
   *
   *   h = 33 * h + 720 + c
   *)
    fun hashChar (c, h) = Word.<<(h, 0w5) + h + 0w720 + (charToWord c)

(* NOTE: another function we might try is h = 5*h + c, which is used
 * in STL.
 *)

    fun hashString s = CharVector.foldl hashChar 0w0 s
	  
  end (* HashString *)
(* hash-table-rep.sml
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.
 * COPYRIGHT (c) 1996 AT&T Research.
 *
 * This is the internal representation of hash tables, along with some
 * utility functions.  It is used in both the polymorphic and functor
 * hash table implementations.
 *
 * AUTHOR:  John Reppy
 *	    AT&T Bell Laboratories
 *	    Murray Hill, NJ 07974
 *	    jhr@research.att.com
 *)

structure HashTableRep : sig

    datatype ('a, 'b) bucket
      = NIL
      | B of (word * 'a * 'b * ('a, 'b) bucket)

    type ('a, 'b) table = ('a, 'b) bucket array

    val alloc : int -> ('a, 'b) table
	(* allocate a table of at least the given size *)

    val growTable : (('a, 'b) table * int) -> ('a, 'b) table
	(* grow a table to the specified size *)

    val growTableIfNeeded : (('a, 'b) table ref * int) -> bool
	(* conditionally grow a table; the second argument is the number
	 * of items currently in the table.
	 *)

    val clear : ('a, 'b) table -> unit
	(* remove all items *)

    val listItems  : (('a, 'b) table * int ref) -> 'b list
    val listItemsi : (('a, 'b) table * int ref) -> ('a * 'b) list


    val appi : ('a * 'b -> 'c) -> ('a, 'b) table -> unit
    val app : ('a -> 'b) -> ('c, 'a) table -> unit

    val mapi : ('a * 'b -> 'c) -> ('a, 'b) table -> ('a, 'c) table
    val map : ('a -> 'b) -> ('c, 'a) table -> ('c, 'b) table

    val foldi : ('a * 'b * 'c -> 'c) -> 'c -> ('a, 'b) table -> 'c
    val fold : ('a * 'b -> 'b) -> 'b -> ('c, 'a) table -> 'b

    val modify  : ('b -> 'b) -> ('a, 'b) table -> unit
    val modifyi : (('a * 'b) -> 'b) -> ('a, 'b) table -> unit

    val filteri : ('a * 'b -> bool) -> ('a, 'b) table -> int
    val filter : ('a -> bool) -> ('b,'a) table -> int

    val copy : ('a, 'b) table -> ('a, 'b) table

    val bucketSizes : ('a, 'b) table -> int list

  end = struct

    datatype ('a, 'b) bucket
      = NIL
      | B of (word * 'a * 'b * ('a, 'b) bucket)

    type ('a, 'b) table = ('a, 'b) bucket array

    fun index (i, sz) = Word.toIntX(Word.andb(i, Word.fromInt sz - 0w1))

  (* find smallest power of 2 (>= 32) that is >= n *)
    fun roundUp n = let
	  fun f i = if (i >= n) then i else f(i * 2)
	  in
	    f 32
	  end

  (* Create a new table; the int is a size hint and the exception
   * is to be raised by find.
   *)
    fun alloc sizeHint = Array.array(roundUp sizeHint, NIL)

  (* grow a table to the specified size *)
    fun growTable (table, newSz) = let
	  val newArr = Array.array (newSz, NIL)
	  fun copy NIL = ()
	    | copy (B(h, key, v, rest)) = let
		val indx = index (h, newSz)
		in
		  Array.update (newArr, indx,
		    B(h, key, v, Array.sub(newArr, indx)));
		  copy rest
		end
	  in
	    Array.app copy table;
	    newArr
	  end

  (* conditionally grow a table; return true if it grew. *)
    fun growTableIfNeeded (table, nItems) = let
	    val arr = !table
	    val sz = Array.length arr
	    in
	      if (nItems >= sz)
		then (table := growTable (arr, sz+sz); true)
		else false
	    end

  (* remove all items *)
    fun clear table = Array.modify (fn _ => NIL) table

  (* return a list of the items in the table *)
    fun listItems (table, nItems) = let
	  fun f (_, l, 0) = l
	    | f (~1, l, _) = l
	    | f (i, l, n) = let
		fun g (NIL, l, n) = f (i-1, l, n)
		  | g (B(_, k, v, r), l, n) = g(r, v::l, n-1)
		in
		  g (Array.sub(table, i), l, n)
		end
	  in
	    f ((Array.length table) - 1, [], !nItems)
	  end (* listItems *)
    fun listItemsi (table, nItems) = let
	  fun f (_, l, 0) = l
	    | f (~1, l, _) = l
	    | f (i, l, n) = let
		fun g (NIL, l, n) = f (i-1, l, n)
		  | g (B(_, k, v, r), l, n) = g(r, (k, v)::l, n-1)
		in
		  g (Array.sub(table, i), l, n)
		end
	  in
	    f ((Array.length table) - 1, [], !nItems)
	  end (* listItems *)

  (* Apply a function to the entries of the table *)
    fun appi f table = let
	  fun appF NIL = ()
	    | appF (B(_, key, item, rest)) = (f (key, item); appF rest)
	  in
	    Array.app appF table
	  end (* appi *)
    fun app f table = let
	  fun appF NIL = ()
	    | appF (B(_, key, item, rest)) = (f item; appF rest)
	  in
	    Array.app appF table
	  end (* app *)

  (* Map a table to a new table that has the same keys *)
    fun mapi f table = let
	  fun mapF NIL = NIL
	    | mapF (B(hash, key, item, rest)) =
		B(hash, key, f (key, item), mapF rest)
	  val newTbl = Array.tabulate (
		Array.length table,
		fn i => mapF (Array.sub(table, i)))
	  in
	    newTbl
	  end (* transform *)

  (* Map a table to a new table that has the same keys *)
    fun map f table = let
	  fun mapF NIL = NIL
	    | mapF (B(hash, key, item, rest)) = B(hash, key, f item, mapF rest)
	  val newTbl = Array.tabulate (
		Array.length table,
		fn i => mapF (Array.sub(table, i)))
	  in
	    newTbl
	  end (* map *)

    fun foldi f init table = let
	  fun foldF (NIL, accum) = accum
	    | foldF (B(hash, key, item, rest), accum) =
		foldF(rest, f(key, item, accum))
	  in
	    Array.foldl foldF init table
	  end
    fun fold f init table = let
	  fun foldF (NIL, accum) = accum
	    | foldF (B(hash, key, item, rest), accum) =
		foldF(rest, f(item, accum))
	  in
	    Array.foldl foldF init table
	  end

  (* modify the hash-table items in place *)
    fun modify f table = let
	  fun modifyF NIL = NIL
	    | modifyF (B(hash, key, item, rest)) = B(hash, key, f item, modifyF rest)
	  in
	    Array.modify modifyF table
	  end
    fun modifyi f table = let
	  fun modifyF NIL = NIL
	    | modifyF (B(hash, key, item, rest)) =
		B(hash, key, f(key, item), modifyF rest)
	  in
	    Array.modify modifyF table
	  end

  (* remove any hash table items that do not satisfy the given
   * predicate.  Return the number of items left in the table.
   *)
    fun filteri pred table = let
	  val nItems = ref 0
	  fun filterP NIL = NIL
	    | filterP (B(hash, key, item, rest)) = if (pred(key, item))
		then (
		  nItems := !nItems+1;
		  B(hash, key, item, filterP rest))
		else filterP rest
	  in
	    Array.modify filterP table;
	    !nItems
	  end (* filteri *)
    fun filter pred table = let
	  val nItems = ref 0
	  fun filterP NIL = NIL
	    | filterP (B(hash, key, item, rest)) = if (pred item)
		then (
		  nItems := !nItems+1;
		  B(hash, key, item, filterP rest))
		else filterP rest
	  in
	    Array.modify filterP table;
	    !nItems
	  end (* filter *)

  (* Create a copy of a hash table *)
    fun copy table =
	  Array.tabulate (Array.length table, fn i => Array.sub(table, i));

  (* returns a list of the sizes of the various buckets.  This is to
   * allow users to gauge the quality of their hashing function.
   *)
    fun bucketSizes table = let
	  fun len (NIL, n) = n
	    | len (B(_, _, _, r), n) = len(r, n+1)
	  in
	    Array.foldr (fn (b, l) => len(b, 0) :: l) [] table
	  end

  end (* HashTableRep *)
(* hash-table-sig.sml
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.
 *
 * The signature of the polymorphic hash table structure.
 *
 * AUTHOR:  John Reppy
 *	    AT&T Bell Laboratories
 *	    Murray Hill, NJ 07974
 *	    jhr@research.att.com
 *)

signature HASH_TABLE =
  sig

    type ('a, 'b) hash_table
	(* type of hash table mapping 'a to 'b *)

    val mkTable : (('a -> word) * (('a * 'a) -> bool)) -> (int * exn)
	  -> ('a,'b) hash_table
	(* Given a hashing function and an equality predicate, create a new table;
	 * the int is a size hint and the exception is to be raised by find.
	 *)

    val clear : ('a, 'b) hash_table -> unit
	(* remove all elements from the table *)

    val insert : ('a, 'b) hash_table -> ('a * 'b) -> unit
	(* Insert an item.  If the key already has an item associated with it,
	 * then the old item is discarded.
	 *)

    val inDomain : ('a, 'b) hash_table -> 'a -> bool
	(* return true, if the key is in the domain of the table *)

    val lookup : ('a, 'b) hash_table -> 'a -> 'b
	(* Find an item, the table's exception is raised if the item doesn't exist *)

    val find : ('a, 'b) hash_table -> 'a -> 'b option
	(* Look for an item, return NONE if the item doesn't exist *)

    val remove : ('a, 'b) hash_table -> 'a -> 'b
	(* Remove an item, returning the item.  The table's exception is raised if
	 * the item doesn't exist.
	 *)

    val numItems : ('a, 'b) hash_table ->  int
	(* Return the number of items in the table *)

    val listItems  : ('a, 'b) hash_table -> 'b list
    val listItemsi : ('a, 'b) hash_table -> ('a * 'b) list
	(* Return a list of the items (and their keys) in the table *)

    val app  : ('b -> unit) -> ('a, 'b) hash_table -> unit
    val appi : (('a * 'b) -> unit) -> ('a, 'b) hash_table -> unit
	(* Apply a function to the entries of the table *)

    val map  : ('b -> 'c) -> ('a, 'b) hash_table -> ('a, 'c) hash_table
    val mapi : (('a * 'b) -> 'c) -> ('a, 'b) hash_table -> ('a, 'c) hash_table
	(* Map a table to a new table that has the same keys *)

    val fold  : (('b *'c) -> 'c) -> 'c -> ('a, 'b) hash_table -> 'c
    val foldi : (('a * 'b * 'c) -> 'c) -> 'c -> ('a, 'b) hash_table -> 'c
	(* Fold a function over the elements of a table *)

    val modify  : ('b -> 'b) -> ('a, 'b) hash_table -> unit
    val modifyi : (('a * 'b) -> 'b) -> ('a, 'b) hash_table -> unit
	(* modify the hash-table items in place *)

    val filter  : ('b -> bool) -> ('a, 'b) hash_table -> unit
    val filteri : (('a * 'b) -> bool) -> ('a, 'b) hash_table -> unit
	(* remove any hash table items that do not satisfy the given
	 * predicate.
	 *)

    val copy : ('a, 'b) hash_table -> ('a, 'b) hash_table
	(* Create a copy of a hash table *)

    val bucketSizes : ('a, 'b) hash_table -> int list
	(* returns a list of the sizes of the various buckets.  This is to
	 * allow users to gauge the quality of their hashing function.
	 *)

  end (* HASH_TABLE *)
(* hash-table.sml
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.
 *
 * Polymorphic hash tables.
 *
 * AUTHOR:  John Reppy
 *	    AT&T Bell Laboratories
 *	    Murray Hill, NJ 07974
 *	    jhr@research.att.com
 *)

structure HashTable : HASH_TABLE =
  struct

    structure HTRep = HashTableRep

    datatype ('a, 'b) hash_table = HT of {
	hash_fn : 'a -> word,
	eq_pred : ('a * 'a) -> bool,
	not_found : exn,
	table : ('a, 'b) HTRep.table ref,
	n_items : int ref
      }

    fun index (i, sz) = Word.toIntX(Word.andb(i, Word.fromInt sz - 0w1))

  (* find smallest power of 2 (>= 32) that is >= n *)
    fun roundUp n = let
	  fun f i = if (i >= n) then i else f(i * 2)
	  in
	    f 32
	  end

  (* Create a new table; the int is a size hint and the exception
   * is to be raised by find.
   *)
    fun mkTable (hash, eq) (sizeHint, notFound) = HT{
	    hash_fn = hash,
	    eq_pred = eq,
	    not_found = notFound,
	    table = ref (HTRep.alloc sizeHint),
	    n_items = ref 0
	  }

  (* remove all elements from the table *)
    fun clear (HT{table, n_items, ...}) = (HTRep.clear(!table); n_items := 0)

  (* Insert an item.  If the key already has an item associated with it,
   * then the old item is discarded.
   *)
    fun insert (tbl as HT{hash_fn, eq_pred, table, n_items, ...}) (key, item) = let
	  val arr = !table
	  val sz = Array.length arr
	  val hash = hash_fn key
	  val indx = index (hash, sz)
	  fun look HTRep.NIL = (
		Array.update(arr, indx,
		  HTRep.B(hash, key, item, Array.sub(arr, indx)));
		n_items := !n_items + 1;
		HTRep.growTableIfNeeded (table, !n_items);
		HTRep.NIL)
	    | look (HTRep.B(h, k, v, r)) = if ((hash = h) andalso eq_pred(key, k))
		then HTRep.B(hash, key, item, r)
		else (case (look r)
		   of HTRep.NIL => HTRep.NIL
		    | rest => HTRep.B(h, k, v, rest)
		  (* end case *))
	  in
	    case (look (Array.sub (arr, indx)))
	     of HTRep.NIL => ()
	      | b => Array.update(arr, indx, b)
	  end

  (* return true, if the key is in the domain of the table *)
    fun inDomain (HT{hash_fn, eq_pred, table, ...}) key = let
	  val arr = !table
	  val hash = hash_fn key
	  val indx = index (hash, Array.length arr)
	  fun look HTRep.NIL = false
	    | look (HTRep.B(h, k, v, r)) = 
		((hash = h) andalso eq_pred(key, k)) orelse look r
	  in
	    look (Array.sub (arr, indx))
	  end

  (* find an item, the table's exception is raised if the item doesn't exist *)
    fun lookup (HT{hash_fn, eq_pred, table, not_found, ...}) key = let
	  val arr = !table
	  val sz = Array.length arr
	  val hash = hash_fn key
	  val indx = index (hash, sz)
	  fun look HTRep.NIL = raise not_found
	    | look (HTRep.B(h, k, v, r)) = if ((hash = h) andalso eq_pred(key, k))
		then v
		else look r
	  in
	    look (Array.sub (arr, indx))
	  end

  (* look for an item, return NONE if the item doesn't exist *)
    fun find (HT{hash_fn, eq_pred, table, ...}) key = let
	  val arr = !table
	  val sz = Array.length arr
	  val hash = hash_fn key
	  val indx = index (hash, sz)
	  fun look HTRep.NIL = NONE
	    | look (HTRep.B(h, k, v, r)) = if ((hash = h) andalso eq_pred(key, k))
		then SOME v
		else look r
	  in
	    look (Array.sub (arr, indx))
	  end

  (* Remove an item.  The table's exception is raised if
   * the item doesn't exist.
   *)
    fun remove (HT{hash_fn, eq_pred, not_found, table, n_items}) key = let
	  val arr = !table
	  val sz = Array.length arr
	  val hash = hash_fn key
	  val indx = index (hash, sz)
	  fun look HTRep.NIL = raise not_found
	    | look (HTRep.B(h, k, v, r)) = if ((hash = h) andalso eq_pred(key, k))
		then (v, r)
		else let val (item, r') = look r in (item, HTRep.B(h, k, v, r')) end
	  val (item, bucket) = look (Array.sub (arr, indx))
	  in
	    Array.update (arr, indx, bucket);
	    n_items := !n_items - 1;
	    item
	  end (* remove *)

  (* Return the number of items in the table *)
   fun numItems (HT{n_items, ...}) = !n_items

  (* return a list of the items in the table *)
    fun listItems (HT{table = ref arr, n_items, ...}) =
	  HTRep.listItems (arr, n_items)
    fun listItemsi (HT{table = ref arr, n_items, ...}) =
	  HTRep.listItemsi (arr, n_items)

  (* Apply a function to the entries of the table *)
    fun appi f (HT{table, ...}) = HTRep.appi f (! table)
    fun app f (HT{table, ...}) = HTRep.app f (! table)

  (* Map a table to a new table that has the same keys and exception *)
    fun mapi f (HT{hash_fn, eq_pred, table, n_items, not_found}) = HT{
	    hash_fn = hash_fn, eq_pred = eq_pred,
	    table = ref(HTRep.mapi f (! table)),
	    n_items = ref(!n_items),
	    not_found = not_found
	  }

  (* Map a table to a new table that has the same keys and exception *)
    fun map f (HT{hash_fn, eq_pred, table, n_items, not_found}) = HT{
	    hash_fn = hash_fn, eq_pred = eq_pred,
	    table = ref(HTRep.map f (! table)),
	    n_items = ref(!n_items),
	    not_found = not_found
	  }

  (* Fold a function over the entries of the table *)
    fun foldi f init (HT{table, ...}) = HTRep.foldi f init (! table)
    fun fold f init (HT{table, ...}) = HTRep.fold f init (! table)

  (* modify the hash-table items in place *)
    fun modifyi f (HT{table, ...}) = HTRep.modifyi f (!table)
    fun modify f (HT{table, ...}) = HTRep.modify f (!table)

  (* remove any hash table items that do not satisfy the given
   * predicate.
   *)
    fun filteri pred (HT{table, n_items, ...}) =
	  n_items := HTRep.filteri pred (! table)
    fun filter pred (HT{table, n_items, ...}) = 
	  n_items := HTRep.filter pred (! table)

  (* Create a copy of a hash table *)
    fun copy (HT{hash_fn, eq_pred, table, n_items, not_found}) =HT{
	    hash_fn = hash_fn, eq_pred = eq_pred,
	    table = ref(HTRep.copy (! table)), n_items = ref(!n_items),
	    not_found = not_found
	  }

  (* returns a list of the sizes of the various buckets.  This is to
   * allow users to gauge the quality of their hashing function.
   *)
    fun bucketSizes (HT{table, ...}) = HTRep.bucketSizes(! table)

  end (* HashTable *)
(* Interpreting uJava, a class-based OO language like Java, but no
   static fields or methods, no method overloading, no interfaces, no
   packages.  sestoft@dina.kvl.dk * 1999-04-17, 2001-04-18 *)

app load ["Absyn", "Env", "Sto"];

open Absyn Env Sto;

(* ------------------------------------------------------------ *)

(* Semantic values used in the interpreter *)

type location = int                     (* locations in the store sto *)
type href = int                         (* locations in the heap h    *)

datatype value =
    Int of int
  | Null
  | Ref of href

and hvalue = 
    Object of object
  | String of string

and method = Method of
    { args : (typ * string) list,
      res  : typ option,
      body : stmt }

and class = Class of
    { name    : string, 
      super   : string option, 
      fields  : (typ * string) list,
      methods : (string, method) env } 

withtype object = 
    { class  : string, 
      fields : (string, location) env }

type env = (string, location) Env.env 

type sto = value Sto.sto                (* indexed by int locations  *)

type heap = hvalue Sto.sto              (* indexed by int references *)

type prog = (string, class) Env.env

(* ------------------------------------------------------------ *)

exception Type of string                (* evaluation type failure *)

exception NullPointer                   (* evaluation null deref   *)

(* ------------------------------------------------------------ *)

local 
    val nextloc = ref 0
in
    fun newloc () = !nextloc before nextloc := !nextloc + 1
end

fun bindvar (env : env) (sto : sto) (x : string) (v : value) : env * sto =
    let val loc = newloc ()
    in (bind1 env (x, loc), setsto sto loc v) end;

fun allocvar env sto (typ, x) : env * sto =
    bindvar env sto x (case typ of 
                           TypI   => Int 0
                         | TypO _ => Null)

fun allocvars sto (txs : (typ * string) list) : env * sto =
    let fun loop []           (env0, sto0) = (env0, sto0)
          | loop (tx1 :: txr) (env0, sto0) = 
            loop txr (allocvar env0 sto0 tx1)
    in loop txs (Env.empty, sto) end;

fun bindpars sto (txs : (typ * string) list) (vs : value list) : env * sto =
    let fun loop []              []         (env0, sto0) = (env0, sto0)
          | loop ((_,x1) :: txr) (v1 :: vr) (env0, sto0) = 
            loop txr vr (bindvar env0 sto0 x1 v1)
          | loop _ _ _ = raise Type "argument length mismatch"
    in loop txs vs (Env.empty, sto) end;

fun getobj (h : heap) (obj : value) : object =
    case obj of 
        Ref loc => 
            (case getsto h loc of
                 Object object => object 
               | _ => raise Type "attempt to use string as object")
      | Null  => raise NullPointer
      | Int i => raise Type "attempt to use integer as reference";
    
fun getobjenv (h : heap) (obj : value) : env = 
    #fields (getobj h obj)

fun getfieldloc h obj fld : location = 
    lookup (getobjenv h obj) fld

fun setfield sto h obj fld v : sto = 
    setsto sto (getfieldloc h obj fld) v

fun lookupmethod (prog : prog) h (obj : value) mname : method =
    let fun search (Class { super = NONE, methods, ... }) = 
            (lookup methods mname
             handle Subscript => raise Fail ("No such method: " ^ mname))
          | search (Class { super = SOME super, methods, ... }) =
            (lookup methods mname
             handle Subscript => search (lookup prog super))
        val { class, ... } = getobj h obj
    in search (lookup prog class) end;

(* ------------------------------------------------------------ *)

exception ReturnNONE of sto * heap

exception ReturnSOME of value * sto * heap

(* Decide whether c1 is a subclass of c2 *)

fun subclass prog c1 c2 =
    c1 = c2 orelse 
    case lookup prog c1 of 
        Class { super = SOME cls, ... } => subclass prog cls c2
      | _ => false;

(* Allocate a new string in the heap *)

fun newstring sto h s : value * sto * heap =
    let val strref = newloc ()
        val h1 = setsto h strref (String s)
    in (Ref strref, sto, h1) end

(* Allocate a new object in the heap *)

fun newobj prog sto h cname : value * sto * heap =
    let val Class { fields, ... } = lookup prog cname
        val (env1, sto1) = allocvars sto fields
        val objref = newloc ()
        val h1 = setsto h objref (Object { fields = env1, class = cname })
    in (Ref objref, sto1, h1) end

(* Create string from value *)

fun tostring h (Int i)   = Int.toString i
  | tostring h Null      = "null"
  | tostring h (Ref loc) = 
    case getsto h loc of
        String s              => s
      | Object { class, ... } => class ^ "@" ^ Int.fmt StringCvt.HEX loc;

(* Create a truth value from a value *)

fun valistrue (Int 0) = false
  | valistrue (Int _) = true
  | valistrue _       = raise Type "attempt to use reference as truth value";


(* ------------------------------------------------------------ *)
    
(* Execute statement, producing new store and heap *)

fun exec stmt prog (env : env) (sto : sto) (h : heap) : sto * heap = 
    case stmt of
        SetLocal(x, e) => 
            let val (v, sto1, h1) = eval e prog env sto h
            in (setsto sto1 (lookup env x) v, h1) end
      | SetField(eobj, fld, e) => 
            let val (obj, sto1, h1) = eval eobj prog env sto  h
                val (v,   sto2, h2) = eval e    prog env sto1 h1
            in (setfield sto2 h2 obj fld v, h2) end
      | If(e, stmt1, stmt2) => 
        let val (v, sto1, h1) = eval e prog env sto h 
        in 
            if valistrue v then 
                exec stmt1 prog env sto1 h1 
            else 
                exec stmt2 prog env sto1 h1 
        end
      | While(e, body) =>
        let fun loop (sto0, h0) = 
                let val (v, sto1, h1) = eval e prog env sto0 h0 
                in 
                    if valistrue v then 
                        loop (exec body prog env sto1 h1)
                    else 
                        (sto1, h1)
                end
        in loop (sto, h) end
      | Expr e => 
        let val (v, sto1, h1) = eval e prog env sto h
        in (sto1, h1) end
      | Block stmts => 
        let fun loop []       env sto h = (sto, h)
              | loop (s1::sr) env sto h = 
                let val (env1, (sto1, h1)) = stmtordec s1 prog env sto h 
                in loop sr env1 sto1 h1 end
        in loop stmts env sto h end  
      | Return NONE     => 
        raise ReturnNONE (sto, h) 
      | Return (SOME e) => 
        raise ReturnSOME (eval e prog env sto h)

and stmtordec (Stmt stmt) prog env sto h = 
    (env, exec stmt prog env sto h)
  | stmtordec (Dec xt)    prog env sto h = 
    let val (env1, sto1) = allocvar env sto xt
    in (env1, (sto1, h)) end

and eval e prog (env : env) (sto : sto) (h : heap) : value * sto * heap = 
    case e of
        GetLocal name       => 
            (getsto sto (lookup env name), sto, h)
      | GetField(eobj, fld) => 
            let val (obj, sto1, h1) = eval eobj prog env sto h
            in (getsto sto1 (getfieldloc h1 obj fld), sto1, h1) end
      | Cst cst =>
        (case cst of
             CstI i => (Int i, sto, h)
           | CstS s => newstring sto h s
           | CstN   => (Null, sto, h))
      | New cname => 
            newobj prog sto h cname
      | Prim1(ope, e1) => 
        let val (v1, sto1, h1) = eval e1 prog env sto h
        in 
            case (ope, v1) of
                ("!", Int i) => (Int(if i=0 then 1 else 0), sto1, h1)
              | _            => raise Fail ("no such primitive (1): " ^ ope)
             end
      | Prim2(ope, e1, e2) => 
        let val (v1, sto1, h1) = eval e1 prog env sto  h
            val (v2, sto2, h2) = eval e2 prog env sto1 h1
        in 
            case (ope, v1, v2) of
                ("+",  Int i1, Int i2) => (Int (i1+i2),     sto2, h2)
              | ("-",  Int i1, Int i2) => (Int (i1-i2),     sto2, h2)
              | ("*",  Int i1, Int i2) => (Int (i1*i2),     sto2, h2)
              | ("/",  Int i1, Int i2) => (Int (i1 div i2), sto2, h2)
              | ("%",  Int i1, Int i2) => (Int (i1 mod i2), sto2, h2)
              | ("==", _,      _     ) => (if v1 = v2 then Int 1 else Int 0, 
                                           sto2, h2)
              | ("!=", _,      _     ) => (if v1 = v2 then Int 0 else Int 1, 
                                           sto2, h2)
              | ("<",  Int i1, Int i2) => (if i1 < i2 then Int 1 else Int 0, 
                                           sto2, h2)
              | ("<=", Int i1, Int i2) => (if i1 <= i2 then Int 1 else Int 0, 
                                           sto2, h2)
              | (">",  Int i1, Int i2) => (if i1 > i2 then Int 1 else Int 0, 
                                           sto2, h2)
              | (">=", Int i1, Int i2) => (if i1 >= i2 then Int 1 else Int 0, 
                                           sto2, h2)
              | ("+",  Ref r1, Ref r2) => 
                    (case (getsto h2 r1, getsto h2 r2) of
                         (String s1, String s2) => newstring sto2 h2 (s1 ^ s2)
                       | _ => raise Fail "Prim2: + on illegal types")
              | _ => raise Fail ("no such primitive (2): " ^ ope)
        end
      | PrimC(ope, es) => 
        let val (vs, sto1, h1) = evals es prog env sto h
        in 
            case (ope, vs) of
                ("tostring", [v1]) => 
                    newstring sto1 h1 (tostring h1 v1)
              | ("print",    [v1]) => 
                    (print (tostring h1 v1); print "\n"; (v1, sto1, h1))
              | _ => raise Fail ("no such primitive (C): " ^ ope)
        end
      | Andalso(e1, e2) => 
        let val res as (v1, sto1, h1) = eval e1 prog env sto h
        in 
            if valistrue v1 then eval e2 prog env sto1 h1
            else res
        end
      | Orelse(e1, e2) => 
        let val res as (v1, sto1, h1) = eval e1 prog env sto h
        in 
            if valistrue v1 then res 
            else eval e2 prog env sto1 h1
        end
      | Call(eobj, mname, es) =>
        let val (obj, sto1, h1) = eval  eobj prog env sto  h
            val (vs,  sto2, h2) = evals es   prog env sto1 h1
            val (sto3, h3) = callmethod prog sto2 h2 obj mname vs
        in 
            (Null, sto3, h3)
        end
        handle ReturnSOME (v, sto, h) => (v, sto, h)
             | ReturnNONE (sto, h) => (Null, sto, h)

and evals []       prog env sto h = ([], sto, h)
  | evals (e1::er) prog env sto h = 
    let val (v1, sto1, h1) = eval  e1 prog env sto h 
        val (vr, stor, hr) = evals er prog env sto1 h1
    in (v1::vr, stor, hr) end

and callmethod prog sto h obj mname vals : sto * heap =
    let val Method { body, args, ... } = lookupmethod prog h obj mname
        val (menv1, sto1) = bindpars sto args vals
        val (menv2, sto2) = bindvar menv1 sto1 "this" obj
    in exec body prog menv2 sto2 h end;

(* ------------------------------------------------------------ *)

(* Transform a uJava program in abstract syntax into the
   representation required by the interpreter: *)

fun initialize (cdecs : classdec list) : prog =
    let (* Separate field declarations from method declarations *)
        fun fieldsmethods [] (flds, mths) = (flds, mths)
          | fieldsmethods (Fielddec(typ, name)::r) (flds,mths) =
            fieldsmethods r ((typ, name)::flds, mths)
          | fieldsmethods (Methoddec(res, name, args, body)::r) (flds,mths) =
            let val mth = Method{args=args, res=res, body=body}
            in fieldsmethods r (flds, bind1 mths (name, mth)) end
        fun class (Classdec(name, super, members)) = 
            let val (fields, methods) = fieldsmethods members ([], Env.empty)
            in 
                (name, 
                 Class {name=name, super=SOME super, 
                        fields=fields, methods=methods})
            end
        (* Class Object has one method: String toString() *)
        val object = 
            Class {name    = "Object", 
                   super   = NONE,
                   fields  = [], 
                   methods = Env.fromList
                       [("toString", 
                         Method{args = [],
                                res  = SOME(TypO "String"),
                                body =
                                Return(SOME (PrimC("tostring", 
                                                   [GetLocal "this"])))})]}
        val plainclasses = ("Object", object) :: List.map class cdecs
        val cenv0 = Env.fromList plainclasses

        (* Add all superclass fields to the field list (NB: this function 
           enters an infinite loop if the class hierarchy is cyclic): *)
        fun allfields NONE = []
          | allfields (SOME cname) = 
            let val Class {super, fields, ...} = lookup cenv0 cname
            in fields @ allfields super end
        fun completefields (_, Class {name, super, fields, methods}) =
            (name, Class {name    = name, 
                          super   = super, 
                          fields  = fields @ allfields super, 
                          methods = methods})
        val completedclasses = List.map completefields plainclasses
    in 
        Env.fromList completedclasses 
    end;

fun run (cdecs : classdec list) (vals : int list) = 
    let val prog = initialize cdecs
        val sto0 = Sto.empty () 
        val h0   = Sto.empty ()
        val (obj, sto1, h1) = newobj prog sto0 h0 "Main"
    in 
        callmethod prog sto1 h1 obj "main" (List.map Int vals)
    end;
