structure Prettyprint :> PRETTYPRINT =
struct

   (* Recommend using these precedences when using this library *)
   infixr 6 ++ <+>

   structure PageWidth =
   struct
      datatype t =
         AvailablePerLine of { line_length: int, ribbon_width: real }
       | Unbounded
      val default = AvailablePerLine { line_length = 80, ribbon_width = 1.0 }
   end

   datatype 'a doc =
      Failure (* Reject this document. Used internally by library functions. *)
    | Empty (* The empty document *)
    | Char of char (* A single character. Invariant: not a newline *)
    | Text of string (* String. Invariant: length > 1, no newlines *)
    | Line (* Hard line break. *)
    | FlatAlt of 'a doc * 'a doc (* First doc, except when grouped. Then second doc. *)
    | Cat of 'a doc * 'a doc (* Concatenation of docs *)
    | Nest of int * 'a doc (* Nest a number of spaces *)
    | Union of 'a doc * 'a doc (* Invariant: first line of 1st doc is longer than first line of second doc *)
    | Column of int -> 'a doc (* Document that depends on the current column *)
    | WithPageWidth of PageWidth.t -> 'a doc (* Document that depends on the page width *)
    | Nesting of int -> 'a doc (* Document that depends on the current nesting level *)
    | Annotated of 'a * 'a doc (* Annotated document *)

   structure Flattened =
   struct
      datatype 'a result =
         Flattened of 'a
       | AlreadyFlat
       | NeverFlat

      fun map f (Flattened x) = Flattened (f x)
        | map _ AlreadyFlat = AlreadyFlat
        | map _ NeverFlat = NeverFlat

      fun flatten x =
         case x
           of FlatAlt (_, y) => flatten y
            | Cat (x, y) => Cat (flatten x, flatten y)
            | Nest (i, x) => Nest (i, flatten x)
            | Line => Failure
            | Union (x, _) => flatten x
            | Column f => Column (flatten o f)
            | WithPageWidth f => WithPageWidth (flatten o f)
            | Nesting f => Nesting (flatten o f)
            | Annotated (ann, x) => Annotated (ann, flatten x)
            | x => x

      fun changes_upon_flattening x =
         case x
           of FlatAlt (_, y) => Flattened (flatten y)
            | Line => NeverFlat
            | Union (x, _) => Flattened x
            | Nest (i, x) => map (fn y => Nest (i, y)) (changes_upon_flattening x)
            | Annotated (ann, x) => map (fn y => Annotated (ann, y)) (changes_upon_flattening x)
            | Column f => Flattened (Column (flatten o f))
            | Nesting f => Flattened (Nesting (flatten o f))
            | WithPageWidth f => Flattened (WithPageWidth (flatten o f))
            | Empty => AlreadyFlat
            | Char _ => AlreadyFlat
            | Text _ => AlreadyFlat
            | Failure => NeverFlat
            | Cat (x, y) =>
                  case (changes_upon_flattening x, changes_upon_flattening y)
                    of (NeverFlat, _) => NeverFlat
                     | (_, NeverFlat) => NeverFlat
                     | (Flattened x', Flattened y') => Flattened (Cat (x', y'))
                     | (Flattened x', AlreadyFlat) => Flattened (Cat (x', y))
                     | (AlreadyFlat, Flattened y') => Flattened (Cat (x, y'))
                     | (AlreadyFlat, AlreadyFlat) => AlreadyFlat
   end

   (* Invariant: String must not contain newlines *)
   fun unsafeStr s =
      case String.size s
        of 0 => Empty
         | 1 => Char (String.sub (s, 0))
         | _ => Text s
         (* The empty document *)
   val empty = Empty
   (* Lays out the given document with the nesting level increased *)
   fun nest (0, x) = x
     | nest (i, x) = Nest (i, x)
   (* Newline, or space when grouped *)
   val line = FlatAlt (Line, Char #" ")
   (* Newline, or empty when grouped *)
   val line' = FlatAlt (Line, Empty)
   (* Space if the result fits on the page, or newline otherwise *)
   val softline = Union (Char #" ", Line)
   (* Empty if the result fits on the page, or newline otherwise *)
   val softline' = Union (Empty, Line)
   (* Always a linebreak *)
   val hardline = Line
   (* Try laying out the document in a single line *)
   fun group x =
      case x
        of Union _ => x
         | FlatAlt (a, b) =>
               (case Flattened.changes_upon_flattening b
                  of Flattened.Flattened b' => Union (b', a)
                   | Flattened.AlreadyFlat => Union (b, a)
                   | Flattened.NeverFlat => a)
                   | _ =>
                         case Flattened.changes_upon_flattening x
                           of Flattened.Flattened x' => Union (x', x)
                            | Flattened.AlreadyFlat => x
                            | Flattened.NeverFlat => x

                            (* Render the first document, falling back to the other when grouped *)
   val flatAlt = FlatAlt

   (*** Binary functions *)
   (* Concatenates documents *)
   val op++ = Cat
   (* Concatenates with a space in between *)
   fun op<+> (x, y) = x ++ Char #" " ++ y

   (*** List functions *)
   (* Concatenate with a given concatenation function *)
   fun concatWith _ [] = Empty
     | concatWith _ [x] = x
     | concatWith f (x::xs) = f (x, concatWith f xs)
   (* Appends the document to all but the last document in the list *)
   fun punctuate (_, []) = []
     | punctuate (_, [d]) = [d]
     | punctuate (p, d::ds) = d ++ p :: punctuate (p, ds)
   (** Sep - insert a space when grouped *)
   (* Concatenate horizontally *)
   fun hsep ds = concatWith op<+> ds
   (* Concatenate vertically *)
   fun vsep ds = concatWith (fn (x, y) => x ++ line ++ y) ds
   (* Concatenate horizontally as long as possible, continuing on the next lines *)
   fun fillSep ds = concatWith (fn (x, y) => x ++ softline ++ y) ds
   (* Try laying out vertically, falling back to vertically *)
   fun sep ds = group (vsep ds)
   (** Cat - empty when grouped *)
   (* Concatenate horizontally *)
   fun hcat ds = concatWith op++ ds
   (* Concatenate vertically *)
   fun vcat ds = concatWith (fn (x, y) => x ++ line' ++ y) ds
   (* Concatenate horizontally as long as possible, continuing on the next lines *)
   fun fillCat ds = concatWith (fn (x, y) => x ++ softline' ++ y) ds
   (* Try laying out vertically, falling back to vertically *)
   fun cat ds = group (vcat ds)

   (*** Reactive combinators *)
   (* Layout a document depending on which column it starts at *)
   val column = Column
   (* Layout a document depending on the current nesting level *)
   val nesting = Nesting
   (* Layout a document, allowing access to the current column width *)
   fun width (doc, f) =
      column (fn colStart =>
      doc ++ column (fn colEnd =>
      f (colEnd - colStart)))
   (* Layout a document depending on the page width *)
   val pageWidth = WithPageWidth

   (*** Fillers *)
   (* Insert a number of spaces *)
   fun spaces 0 = Empty
     | spaces 1 = Char #" "
     | spaces n = Text (CharVector.tabulate (n, fn _ => #" "))
   (* [fill (i, x)] Lays out the document x, then appends spaces until width is i. *)
   fun fill (n, doc) = width (doc, fn w => spaces (n - w))
   (* [fillBreak (i, x)] Lays out the document x, then appends spaces until width is i.
      If already larger, nesting level is increased by i and a line is appended. *)
   fun fillBreak (n, doc) = width (doc, fn w => if w > n then nest (n, line') else spaces (n - w))

   (*** Alignment functions *)
   (* Lays out the document with the nesting level set to the current column *)
   fun align d = column (fn k => nesting (fn i => nest (k - i, d)))
   (* Lays out the document with the nesting level set to the current column plus i *)
   fun hang (i, d) = align (nest (i, d))
   (* Indents the document by n spaces, starting from the current position *)
   fun indent (i, d) = hang (i, spaces i ++ d)

   (*** Annotations *)
   (* Add an annotation *)
   val annotate = Annotated
   (* Remove annotation (Note: slow) *)
   fun unAnnotate d = alterAnnotations (fn _ => []) d
   (* Modify annotations *)
   and reAnnotate f d = alterAnnotations (fn x => [f x]) d
   (* Generalized reAnnotate *)
   and alterAnnotations re =
      let
         fun go Failure = Failure
           | go Empty = Empty
           | go (Char c) = Char c
           | go (Text t) = Text t
           | go Line = Line
           | go (FlatAlt (x, y)) = FlatAlt (go x, go y)
           | go (Cat (x, y)) = Cat (go x, go y)
           | go (Nest (i, x)) = Nest (i, go x)
           | go (Union (x, y)) = Union (go x, go y)
           | go (Column f) = Column (go o f)
           | go (WithPageWidth f) = WithPageWidth (go o f)
           | go (Nesting f) = Nesting (go o f)
           | go (Annotated (ann, x)) = List.foldr Annotated (go x) (re ann)
      in
         go
      end

   (* Convert a string into a document *)
   fun str s =
      let
         fun spliton_newline sub =
            if Substring.size sub = 0 then
               []
            else
               let
                  val (pre, rest) = Substring.splitl (fn c => c <> #"\n") sub
               in
                  Substring.string pre :: spliton_newline (Substring.triml 1 rest)
               end
      in
         vsep (List.map unsafeStr (spliton_newline (Substring.full s)))
      end

   structure DocStream =
   struct
      datatype 'a t =
         SFail
       | SEmpty
       | SChar of char * 'a t
       | SText of string * 'a t
       | SLine of int * 'a t
       | SAnnPush of 'a * 'a t
       | SAnnPop of 'a t

      fun unAnnotate doc = alterAnnotations (fn _ => NONE) doc
      and reAnnotate f doc = alterAnnotations (SOME o f) doc
      and alterAnnotations f =
         let
            datatype remove_policy = Remove | DontRemove
            fun go stack = 
               fn SFail => SFail
              | SEmpty => SEmpty
              | SChar (c, rest) => SChar (c, go stack rest)
              | SText (t, rest) => SText (t, go stack rest)
              | SLine (i, rest) => SLine (i, go stack rest)
              | SAnnPush (ann, rest) =>
                    (case f ann
                       of NONE => go (Remove::stack) rest
                        | SOME ann' => SAnnPush (ann', go (DontRemove::stack) rest))
                        | SAnnPop rest =>
                              case stack
                                of [] => raise Fail "Ill-formed DocStream"
                                 | DontRemove::stack' => SAnnPop (go stack' rest)
                                 | Remove::stack' => go stack' rest
         in
            go []
         end

      fun layoutWadlerLeijen (fits, pageWidth, doc) =
         let
            datatype 'a pipeline =
               Nil
             | Cons of int * 'a doc * 'a pipeline
             | UndoAnn of 'a pipeline

            fun initialIndentation (SLine (i, _)) = SOME i
              | initialIndentation (SAnnPush (_, s)) = initialIndentation s
              | initialIndentation (SAnnPop s) = initialIndentation s
              | initialIndentation _ = NONE

            fun selectNicer (lineIndent, currentColumn, x, y) =
               if fits (lineIndent, currentColumn, initialIndentation y, x) then
                  x
               else y

            fun best (nl, cc, Nil) = SEmpty
              | best (nl, cc, UndoAnn ds) = SAnnPop (best (nl, cc, ds))
              | best (nl, cc, Cons (i, d, ds)) =
               case d
                 of Failure => SFail
                  | Empty => best (nl, cc, ds)
                  | Char c => SChar (c, best (nl, cc+1, ds))
                  | Text t => SText (t, best (nl, cc+String.size t, ds))
                  | Line => 
                     let
                        val x = best (i, i, ds)
                        val i' = case x of
                                    SEmpty => 0
                                  | SLine _ => 0
                                  | _ => i
                     in
                        SLine (i', x)
                     end
                  | FlatAlt (x, _) => best (nl, cc, (Cons (i, x, ds)))
                  | Cat (x, y) => best (nl, cc, Cons (i, x, Cons (i, y, ds)))
                  | Nest (j, x) => best (nl, cc, Cons (i+j, x, ds))
                  | Union (x, y) =>
                     let
                        val x' = best (nl, cc, Cons (i, x, ds))
                        val y' = best (nl, cc, Cons (i, y, ds))
                     in
                        selectNicer (nl, cc, x', y')
                     end
                  | Column f => best (nl, cc, Cons (i, f cc, ds))
                  | WithPageWidth f => best (nl, cc, Cons (i, f pageWidth, ds))
                  | Nesting f => best (nl, cc, Cons (i, f i, ds))
                  | Annotated (a, x) => SAnnPush (a, best (nl, cc, Cons (i, x, UndoAnn ds)))
         in
            best (0, 0, Cons (0, doc, Nil))
         end

      fun remainingWidth (lineLength, ribbonFraction, lineIndent, currentColumn) =
         let
            val columnsLeftInLine = lineLength - currentColumn
            val ribbonWidth = Real.floor (Real.fromInt lineLength * ribbonFraction)
            val ribbonWidth = Int.max (0, Int.min (lineLength, ribbonWidth))
            val columnsLeftInRibbon = lineIndent + ribbonWidth - currentColumn
         in
            Int.min (columnsLeftInLine, columnsLeftInRibbon)
         end

      fun layoutUnbounded doc =
         let
            fun failsOnFirstLine SFail = true
              | failsOnFirstLine SEmpty = false
              | failsOnFirstLine (SChar (_, s)) = failsOnFirstLine s
              | failsOnFirstLine (SText (_, s)) = failsOnFirstLine s
              | failsOnFirstLine (SLine _) = false
              | failsOnFirstLine (SAnnPush (_, s)) = failsOnFirstLine s
              | failsOnFirstLine (SAnnPop s) = failsOnFirstLine s 

            fun fits (_, _, _, sdoc) = not (failsOnFirstLine sdoc)
         in
            layoutWadlerLeijen (fits, PageWidth.Unbounded, doc)
         end

      fun layoutPretty (PageWidth.Unbounded, doc) = layoutUnbounded doc
        | layoutPretty (pageWidth as PageWidth.AvailablePerLine {line_length, ribbon_width}, doc) =
         let
            fun fits (w, sds) =
               if w < 0 then
                  false
               else
                  case sds of
                     SFail => false
                   | SEmpty => true
                   | SChar (_, x) => fits (w - 1, x)
                   | SText (s, x) => fits (w - String.size s, x)
                   | SLine _ => true
                   | SAnnPush (_, x) => fits (w, x)
                   | SAnnPop x => fits (w, x)
            fun fits' (nl, cc, _, sds) =
               fits (remainingWidth (line_length, ribbon_width, nl, cc), sds)
         in
            layoutWadlerLeijen (fits', pageWidth, doc)
         end

      fun layoutSmart (PageWidth.Unbounded, doc) = layoutUnbounded doc
        | layoutSmart (pageWidth as PageWidth.AvailablePerLine {line_length, ribbon_width}, doc) =
         let
            fun fits (nl, cc, ii, sds) =
               let
                  val availableWidth = remainingWidth (line_length, ribbon_width, nl, cc)
                  val minNestingLevel =
                     case ii of
                        NONE => cc
                      | SOME i => Int.min (i, cc)
                  fun go (w, sds) =
                     if w < 0 then
                        false
                     else
                        case sds of
                           SFail => false
                         | SEmpty => true
                         | SChar (_, x) => go (w - 1, x)
                         | SText (s, x) => go (w - String.size s, x)
                         | SLine (i, x) =>
                              if minNestingLevel < i then
                                 go (line_length - i, x)
                              else true
                         | SAnnPush (_, x) => go (w, x)
                         | SAnnPop x => go (w, x)

               in
                  go (availableWidth, sds)
               end
         in
            layoutWadlerLeijen (fits, pageWidth, doc)
         end

      fun layoutCompact doc =
         let
            fun scan (_, []) = SEmpty
              | scan (col, d::ds) =
               case d of
                  Failure => SFail
                | Empty => scan (col, ds)
                | Char c => SChar (c, scan (col + 1, ds))
                | Text t => SText (t, scan (col + String.size t, ds))
                | FlatAlt (x, _) => scan (col, x::ds)
                | Line => SLine (0, scan (0, ds))
                | Cat (x, y) => scan (col, x::y::ds)
                | Nest (_, x) => scan (col, x::ds)
                | Union (_, y) => scan (col, y::ds)
                | Column f => scan (col, f col::ds)
                | WithPageWidth f => scan (col, f PageWidth.Unbounded::ds)
                | Nesting f => scan (col, f 0::ds)
                | Annotated (_, rest) => scan (col, rest::ds)
         in
            scan (0, [doc])
         end

      fun render sds =
         let
            fun go SFail = raise Fail "SFail in rendered DocStream.t!"
              | go SEmpty = []
              | go (SChar (c, rest)) = String.str c :: go rest
              | go (SText (t, rest)) = t :: go rest
              | go (SLine (i, rest)) = ("\n" ^ CharVector.tabulate (i, fn _ => #" ")) :: go rest
              | go (SAnnPush (_, rest)) = go rest
              | go (SAnnPop rest) = go rest
         in
            String.concat (go sds)
         end

      fun renderIO (out, sds) =
         let
            fun go SFail = raise Fail "SFail!"
              | go SEmpty = ()
              | go (SChar (c, rest)) = (TextIO.output1 (out, c); go rest)
              | go (SText (t, rest)) = (TextIO.output (out, t); go rest)
              | go (SLine (i, rest)) = (TextIO.output (out, "\n"^CharVector.tabulate(i, fn _ => #" ")); go rest)
              | go (SAnnPush (_, rest)) = go rest
              | go (SAnnPop rest) = go rest
         in
            go sds
         end
   end

   structure DocTree =
   struct
      datatype 'a t =
         TEmpty
       | TChar of char
       | TText of string
       | TLine of int
       | TAnn of 'a * 'a t
       | TConcat of 'a t list

      fun alterAnnotations f =
         fn TEmpty  => TEmpty
          | TChar c => TChar c
          | TText t => TText t
          | TLine i => TLine i
          | TConcat xs => TConcat (List.map (alterAnnotations f) xs)
          | TAnn (ann, rest) => List.foldr TAnn (alterAnnotations f rest) (f ann)

      fun reAnnotate f doc = alterAnnotations (fn x => [f x]) doc

      fun unAnnotate doc = alterAnnotations (fn _ => []) doc

      structure Parser :>
      sig
         type ('a, 's) t
         val map: ('a -> 'b) -> ('a, 's) t -> ('b, 's) t
         val pure: 'a -> ('a, 's) t
         val bind: ('a, 's) t -> ('a -> ('b, 's) t) -> ('b, 's) t
         val fail: ('a, 's) t
         val many: ('a, 's) t -> ('a list, 's) t
         val lift: ('s -> ('a * 's) option) -> ('a, 's) t
         val run: ('a, 's) t * 's -> ('a * 's) option
      end =
      struct
         type ('a, 's) t = 's -> ('a * 's) option
         fun lift x = x
         fun run (p, s) = p s
         fun map f p s = Option.map (fn (x, s') => (f x, s')) (p s)
         fun pure x s = SOME (x, s)
         fun bind p f s = Option.mapPartial (fn (a', s') => f a' s') (p s)
         fun fail _ = NONE
         fun many p s =
            case p s
              of NONE => SOME ([], s)
               | SOME (x, s') => map (fn rest => x :: rest) (many p) s'
      end

      fun to_tree_parser s =
         let
            datatype 'a tok =
               TokEmpty
             | TokChar of char
             | TokText of string
             | TokLine of int
             | TokAnnPush of 'a
             | TokAnnPop

            val next_token = Parser.lift
               (fn DocStream.SFail => raise Fail "Uncaught SFail!"
                 | DocStream.SEmpty => NONE
                 | (DocStream.SChar (c, rest)) => SOME (TokChar c, rest)
                 | (DocStream.SText (t, rest)) => SOME (TokText t, rest)
                 | (DocStream.SLine (i, rest)) => SOME (TokLine i, rest)
                 | (DocStream.SAnnPush (ann, rest)) => SOME (TokAnnPush ann, rest)
                 | (DocStream.SAnnPop rest) => SOME (TokAnnPop, rest))

            fun wrap [] = TEmpty
              | wrap [x] = x
              | wrap xs = TConcat xs

            val content_piece =
               Parser.bind next_token
               (fn TokEmpty  => Parser.pure TEmpty
                 | TokChar c => Parser.pure (TChar c)
                 | TokText t => Parser.pure (TText t)
                 | TokLine i => Parser.pure (TLine i)
                 | TokAnnPop => Parser.fail
                 | TokAnnPush ann =>
                    Parser.bind (Parser.lift to_tree_parser)
                    (fn contents => Parser.bind next_token
                    (fn TokAnnPop => Parser.pure (TAnn (ann, contents))
                      | _ => raise Fail "Ill-formed document!")))
         in
            Parser.run (Parser.map wrap (Parser.many content_piece), s)
         end

      fun fromStream sds =
         case to_tree_parser sds
           of NONE => raise Fail "Conversion from DocStream.t to DocTree.t failed!"
            | SOME (r, DocStream.SEmpty) => r
            | SOME _ => raise Fail "Conversion from DocStream.t to DocTree.t left unconsumed input!"
      end

   end
