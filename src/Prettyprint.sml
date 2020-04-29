structure Prettyprint :> Prettyprint =
struct

  infixr 6 ++ <+>

  structure PageWidth =
  struct
    datatype t = AvailablePerLine of { line_length: int, ribbon_fraction: real }
               | Unbounded
    val default = AvailablePerLine { line_length = 80, ribbon_fraction = 1.0 }
  end

  datatype 'a doc = Failure (* Reject this document *)
                  | Empty (* The empty document *)
                  | Char of char (* A single character. Not a newline *)
                  | Text of string (* String. Length > 1, no newlines *)
                  | Line (* Hard line break *)
                  | FlatAlt of 'a doc * 'a doc (* First doc, except when grouped. Then second doc *)
                  | Cat of 'a doc * 'a doc (* Concatenation of docs *)
                  | Nest of int * 'a doc (* Nest a number of spaces *)
                  | Union of 'a doc * 'a doc (* Invariant: first line of 1st doc is longer than first line of second doc *)
                  | Column of int -> 'a doc
                  | WithPageWidth of PageWidth.t -> 'a doc
                  | Nesting of int -> 'a doc 
                  | Annotated of 'a * 'a doc

  structure Flattened =
  struct
    datatype 'a result = Flattened of 'a
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
    | concatWith f (x::xs) = List.foldr f x xs
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
      if Substring.size sub = 0
      then []
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
    datatype 'a t = SFail
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

    fun layoutWadlerLeijen (fits, pWidth, doc) =
    let
      datatype 'a pipeline = Nil
                           | Cons of int * 'a doc * 'a pipeline
                           | UndoAnn of 'a pipeline
      fun best (nl, cc, Nil) = SEmpty
        | best (nl, cc, UndoAnn ds) = SAnnPop (best (nl, cc, ds))
        | best (nl, cc, Cons (i, d, ds)) =
        case d
          of Failure          => SFail
           | Empty            => best (nl, cc, ds)
           | Char c           => SChar (c, best (nl, cc+1, ds))
           | Text t           => SText (t, best (nl, cc+String.size t, ds))
           | Line             => SLine (i, best (i, i, ds))
           | FlatAlt (x, _)   => best (nl, cc, (Cons (i, x, ds)))
           | Cat (x, y)       => best (nl, cc, Cons (i, x, Cons (i, y, ds)))
           | Nest (j, x)      => best (nl, cc, Cons (i+j, x, ds))
           | Union (x, y)     => select_nicer (nl, cc, best (nl, cc, Cons (i, x, ds)), best (nl, cc, Cons (i, y, ds)))
           | Column f         => best (nl, cc, Cons (i, f cc, ds))
           | WithPageWidth f  => best (nl, cc, Cons (i, f pWidth, ds))
           | Nesting f        => best (nl, cc, Cons (i, f i, ds))
           | Annotated (a, x) => SAnnPush (a, best (nl, cc, Cons (i, x, UndoAnn ds)))

      and select_nicer (lineIndent, currentColumn, x, y) =
        case pWidth
          of PageWidth.Unbounded => if not (fails_on_first_line x) then x else y
           | PageWidth.AvailablePerLine {line_length, ribbon_fraction} =>
               let
                 val minNestingLevel =
                   case initial_indentation y
                     of SOME i => Int.min (i, currentColumn)
                      | NONE => currentColumn
                 val columnsLeftInLine = line_length - currentColumn
                 fun clamp (min, max) x = Int.min (min, Int.max (max, x))
                 val ribbonWidth = clamp (0, line_length) (Real.round (Real.fromInt line_length * ribbon_fraction))
                 val columnsLeftInRibbon = lineIndent + ribbonWidth - currentColumn
                 val availableWidth = Int.min (columnsLeftInLine, columnsLeftInRibbon)
               in
                 if fits (pWidth, minNestingLevel, availableWidth, x)
                 then x
                 else y
               end 

      and initial_indentation (SLine (i, _)) = SOME i
        | initial_indentation (SAnnPush (_, s)) = initial_indentation s
        | initial_indentation (SAnnPop s) = initial_indentation s
        | initial_indentation _ = NONE

      and fails_on_first_line SFail = true
        | fails_on_first_line SEmpty = false
        | fails_on_first_line (SChar (_, s)) = fails_on_first_line s
        | fails_on_first_line (SText (_, s)) = fails_on_first_line s
        | fails_on_first_line (SLine _) = false
        | fails_on_first_line (SAnnPush (_, s)) = fails_on_first_line s
        | fails_on_first_line (SAnnPop s) = fails_on_first_line s 
    in
      best (0, 0, Cons (0, doc, Nil))
    end

    fun layoutPretty (pWidth, x) = 
    let
      fun fits (w, sds) =
        if w < 0
        then false
        else case sds
               of SFail => false
                | SEmpty => true
                | SChar (_, x) => fits (w - 1, x)
                | SText (t, x) => fits (w - String.size t, x)
                | SLine _ => true
                | SAnnPush (_, x) => fits (w, x)
                | SAnnPop x => fits (w, x)
    in 
      layoutWadlerLeijen (fn (_, _, w, sd) => fits (w, sd), pWidth, x)
    end

    fun render SFail = raise Fail "SFail must not appear in a rendered SimpleDocStream. This is a bug in the layout algorithm!"
      | render SEmpty = ""
      | render (SChar (c, x)) = String.str c ^ render x
      | render (SText (t, x)) = t ^ render x
      | render (SLine (i, x)) = "\n" ^ CharVector.tabulate (i, fn _ => #" ") ^ render x
      | render (SAnnPush (_, x)) = render x
      | render (SAnnPop x) = render x
    fun renderIO (out, sds) =
    let
      fun go sds =
        case sds
          of SFail => raise Fail "SFail!"
           | SEmpty => ()
           | SChar (c, rest) => (TextIO.output1 (out, c); go rest)
           | SText (t, rest) => (TextIO.output (out, t); go rest)
           | SLine (i, rest) => (TextIO.output (out, "\n"^CharVector.tabulate(i, fn _ => #" ")); go rest)
           | SAnnPush (_, rest) => go rest
           | SAnnPop rest => go rest
    in
      go sds
    end
  end

  structure DocTree =
  struct
    datatype 'a t = TEmpty
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

    structure Parser =
    struct
      type ('a, 's) t = 's -> ('a * 's) option
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
      datatype 'a tok = TokEmpty
                      | TokChar of char
                      | TokText of string
                      | TokLine of int
                      | TokAnnPush of 'a
                      | TokAnnPop
      fun next_token DocStream.SFail                  = raise Fail "Uncaught SFail!"
        | next_token DocStream.SEmpty                 = NONE
        | next_token (DocStream.SChar (c, rest))      = SOME (TokChar c, rest)
        | next_token (DocStream.SText (t, rest))      = SOME (TokText t, rest)
        | next_token (DocStream.SLine (i, rest))      = SOME (TokLine i, rest)
        | next_token (DocStream.SAnnPush (ann, rest)) = SOME (TokAnnPush ann, rest)
        | next_token (DocStream.SAnnPop rest)         = SOME (TokAnnPop, rest)
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
              Parser.bind to_tree_parser
              (fn contents => Parser.bind next_token
              (fn TokAnnPop => Parser.pure (TAnn (ann, contents))
                | _ => raise Fail "Ill-formed document!")))
    in
      Parser.map wrap (Parser.many content_piece) s
    end

    fun fromStream sds =
      case to_tree_parser sds
        of NONE => raise Fail "Conversion from DocStream.t to DocTree.t failed!"
         | SOME (r, DocStream.SEmpty) => r
         | SOME (_, _) => raise Fail "Conversion from DocStream.t to DocTree.t left unconsumed input!"
    end

  end
