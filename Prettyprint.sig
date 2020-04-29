
signature Prettyprint =
sig

(* Document type, with typed annotations *)
type 'a doc
(* Width of the page to render *)
structure PageWidth:
sig
  datatype t = 
      AvailablePerLine of { line_length: int, ribbon_fraction: real }
    | Unbounded (* Never introduces linebreaks *)
  val default : t
end

(*** Basic functionality *)
(* Convert a string into a document *)
val str : string -> 'a doc
(* Invariant: String must not contain newlines *)
val unsafeStr : string -> 'a doc
(* The empty document *)
val empty : 'a doc
(* Lays out the given document with the nesting level increased *)
val nest : int * 'a doc -> 'a doc
(* Newline, or space when grouped *)
val line : 'a doc
(* Newline, or empty when grouped *)
val line' : 'a doc
(* Space if the result fits on the page, or newline otherwise *)
val softline : 'a doc 
(* Empty if the result fits on the page, or newline otherwise *)
val softline' : 'a doc
(* Always a linebreak *)
val hardline : 'a doc 
(* Try laying out the document in a single line *)
val group : 'a doc -> 'a doc 
(* Render the first document, falling back to the other when grouped *)
val flatAlt : 'a doc * 'a doc -> 'a doc

(*** Alignment functions *)
(* Lays out the document with the nesting level set to the current column *)
val align : 'a doc -> 'a doc
(* Lays out the document with the nesting level set to the current column plus i *)
val hang : int * 'a doc -> 'a doc
(* Indents the document by n spaces, starting from the current position *)
val indent : int * 'a doc -> 'a doc

(*** Binary functions *)
(* Concatenates documents *)
val op++ : 'a doc * 'a doc -> 'a doc
(* Concatenates with a space in between *)
val op<+> : 'a doc * 'a doc -> 'a doc

(*** List functions *)
(* Concatenate with a given concatenation function *)
val concatWith : ('a doc * 'a doc -> 'a doc) -> 'a doc list -> 'a doc
(* Appends the document to all but the last document in the list *)
val punctuate : 'a doc * 'a doc list -> 'a doc list
(** Sep - insert a space when grouped *)
(* Concatenate horizontally *)
val hsep : 'a doc list -> 'a doc
(* Concatenate vertically *)
val vsep : 'a doc list -> 'a doc
(* Concatenate horizontally as long as possible, continuing on the next lines *)
val fillSep : 'a doc list -> 'a doc
(* Try laying out vertically, falling back to vertically *)
val sep : 'a doc list -> 'a doc
(** Cat - empty when grouped *)
(* Concatenate horizontally *)
val hcat : 'a doc list -> 'a doc
(* Concatenate vertically *)
val vcat : 'a doc list -> 'a doc
(* Concatenate horizontally as long as possible, continuing on the next lines *)
val fillCat : 'a doc list -> 'a doc
(* Try laying out vertically, falling back to vertically *)
val cat : 'a doc list -> 'a doc

(*** Reactive combinators *)
(* Layout a document depending on which column it starts at *)
val column : (int -> 'a doc) -> 'a doc
(* Layout a document depending on the current nesting level *)
val nesting : (int -> 'a doc) -> 'a doc
(* Layout a document, allowing access to the current column width *)
val width : 'a doc * (int -> 'a doc) -> 'a doc
(* Layout a document depending on the page width *)
val pageWidth : (PageWidth.t -> 'a doc) -> 'a doc

(*** Fillers *)
(* [fill (i, x)] Lays out the document x, then appends spaces until width is i. *)
val fill : int * 'a doc -> 'a doc
(* [fillBreak (i, x)] Lays out the document x, then appends spaces until width is i.
   If already larger, nesting level is increased by i and a line is appended. *)
val fillBreak : int * 'a doc -> 'a doc
(* Insert a number of spaces *)
val spaces : int -> 'a doc

(*** Annotations *)
(* Add an annotation *)
val annotate : 'a * 'a doc -> 'a doc
(* Remove annotation (Note: slow) *)
val unAnnotate : 'a doc -> 'b doc
(* Modify annotations *)
val reAnnotate : ('a -> 'b) -> 'a doc -> 'b doc
(* Generalized reAnnotate *)
val alterAnnotations : ('a -> 'b list) -> 'a doc -> 'b doc

structure DocStream:
sig
  datatype 'a t =
      SFail
    | SEmpty
    | SChar of char * 'a t
    | SText of string * 'a t
    | SLine of int * 'a t
    | SAnnPush of 'a * 'a t
    | SAnnPop of 'a t

  (*** Annotations *)
  (* Remove annotation (Note: slow) *)
  val unAnnotate : 'a t -> 'b t
  (* Modify annotations *)
  val reAnnotate : ('a -> 'b) -> 'a t -> 'b t
  (* Generalized reAnnotate *)
  val alterAnnotations : ('a -> 'b option) -> 'a t -> 'b t

  (* Default layout algorithm *)
  val layoutPretty : PageWidth.t * 'a doc -> 'a t

  (* Convert to a string *)
  val render : 'a t -> string
  (* Render to the screen *)
  val renderIO : TextIO.outstream * 'a t -> unit

end

structure DocTree:
sig
  datatype 'a t =
      TEmpty
    | TChar of char
    | TText of string
    | TLine of int
    | TAnn of 'a * 'a t
    | TConcat of 'a t list

  (*** Annotations *)
  (* Remove annotations *)
  val unAnnotate : 'a t -> 'b t
  (* Modify annotations *)
  val reAnnotate : ('a -> 'b) -> 'a t -> 'b t
  (* Generalized reAnnotate *)
  val alterAnnotations : ('a -> 'b list) -> 'a t -> 'b t

  (* Convert to the DocTree format from the DocStream format *)
  val fromStream : 'a DocStream.t -> 'a t

end

end
