signature PRETTYPRINT =
sig

   (* ['a doc] represents a document with annotations of type 'a *)
   type 'a doc

   structure PageWidth:
   sig
      (* [PageWidth.t] represents the output page environment, telling the layout algorithm
      when to insert line breaks into the output.
      The 'line_length' represents the size that the line can become before introducing a line break.
      The 'ribbon_width' represents the fraction of the line that can be printed at a time. This
      calculation ignores leading whitespace.
      *)
      datatype t = 
         AvailablePerLine of { line_length: int, ribbon_width: real }
       | Unbounded (* Never introduces linebreaks *)
      val default: t
   end

   (*** Basic functionality *)
   (* [str s] is a document consisting of the string 's' *)
   val str: string -> 'a doc
   (* [unsafeStr] is an optimization of 'str', where the string must not contain newlines *)
   val unsafeStr: string -> 'a doc
   (* [empty] is the empty document *)
   val empty: 'a doc
   (* [nest (i, doc)] lays out 'doc' with the nesting level increased by 'i' *)
   val nest: int * 'a doc -> 'a doc
   (* [line] is a newline, or space when grouped *)
   val line: 'a doc
   (* [line'] is a newline, or empty when grouped *)
   val line': 'a doc
   (* [softline] is a space if the result fits on the page, or newline otherwise *)
   val softline: 'a doc 
   (* [softline'] is empty if the result fits on the page, or newline otherwise *)
   val softline': 'a doc
   (* [hardline] is always a linebreak *)
   val hardline: 'a doc 
   (* [group d] tries to lay out 'd' on one line. If it fails, returns the document unchanged *)
   val group: 'a doc -> 'a doc 
   (* [flatAlt (a, b)] renders 'a' if not grouped, or 'b' if grouped *)
   val flatAlt: 'a doc * 'a doc -> 'a doc

   (*** Alignment functions *)
   (* [align d] lays out document 'd' with the nesting level set to the current column *)
   val align: 'a doc -> 'a doc
   (* [hang (i, d)] lays out document 'd' with the nesting level set to the current column plus 'i' *)
   val hang: int * 'a doc -> 'a doc
   (* [indent (i, d)] indents the document 'd' by 'i' spaces, starting from the current position *)
   val indent: int * 'a doc -> 'a doc

   (*** Binary functions *)
   (* [a ++ b] concatenates documents 'a' and 'b' *)
   val ++ : 'a doc * 'a doc -> 'a doc
   (* [a <+> b] concatenates documents 'a' and 'b' with a space in between *)
   val <+> : 'a doc * 'a doc -> 'a doc

   (*** List functions *)
   (* [concatWith f docs] concatenates 'docs' with the concatenation function 'f' *)
   val concatWith: ('a doc * 'a doc -> 'a doc) -> 'a doc list -> 'a doc
   (* [punctuate (p, docs)] appends the document 'p' to all but the last document in 'docs' *)
   val punctuate: 'a doc * 'a doc list -> 'a doc list
   (** Sep - insert a space when grouped *)
   (* [hsep docs] concatenates 'docs' horizontally *)
   val hsep: 'a doc list -> 'a doc
   (* [vsep docs] concatenates 'docs' vertically *)
   val vsep: 'a doc list -> 'a doc
   (* [fillSep docs] concatenates 'docs' horizontally as long as possible, continuing on the next lines *)
   val fillSep: 'a doc list -> 'a doc
   (* [sep docs] tries laying 'docs' out vertically, falling back to vertically *)
   val sep: 'a doc list -> 'a doc
   (** Cat - empty when grouped *)
   (* [hcat docs] concatenates 'docs' horizontally *)
   val hcat: 'a doc list -> 'a doc
   (* [vcat docs] concatenates 'docs' vertically *)
   val vcat: 'a doc list -> 'a doc
   (* [fillCat docs] concatenates 'docs' horizontally as long as possible, continuing on the next lines *)
   val fillCat: 'a doc list -> 'a doc
   (* [cat docs] tries laying 'docs' out vertically, falling back to vertically *)
   val cat: 'a doc list -> 'a doc

   (*** Reactive combinators *)
   (* [column f] lays out 'f' 'c', where 'c' is the current column *)
   val column: (int -> 'a doc) -> 'a doc
   (* [nesting f] lays out 'f' 'i', where 'i' is the current nesting level *)
   val nesting: (int -> 'a doc) -> 'a doc
   (* [width (d, f)] lays out 'd' ++ 'f' 'c', where 'c' is the column width of document 'd' *)
   val width: 'a doc * (int -> 'a doc) -> 'a doc
   (* [pageWidth f] lays out 'f' 'w', where 'w' is the current page width *)
   val pageWidth: (PageWidth.t -> 'a doc) -> 'a doc

   (*** Fillers *)
   (* [fill (i, d)] Lays out the document 'd', then appends spaces until width is 'i'. *)
   val fill: int * 'a doc -> 'a doc
   (* [fillBreak (i, d)] Lays out the document 'd', then appends spaces until width is 'i'.
   If already larger, nesting level is increased by 'i' and a line is appended. *)
   val fillBreak: int * 'a doc -> 'a doc
   (* [spaces i] results in a document containing 'i' space characters *)
   val spaces: int -> 'a doc

   (*** Annotations *)
   (* [annotate (ann, doc)] annotates 'doc' with the annotation 'ann' *)
   val annotate: 'a * 'a doc -> 'a doc
   (* [unAnnotate doc] removes all annotations from a document. Note: faster to do so on DocStream.t *)
   val unAnnotate: 'a doc -> 'b doc
   (* [reAnnotate f doc] modifies existing annotations on 'doc' by passing them to 'f'. Note applies. *)
   val reAnnotate: ('a -> 'b) -> 'a doc -> 'b doc
   (* [alterAnnotations f doc] modifies existing annotations, possibly adding or removing alongside altering. *)
   val alterAnnotations: ('a -> 'b list) -> 'a doc -> 'b doc

   structure DocStream:
   sig
      (* [DocStream.t] is the output of a layout algorithm, representing a stream of instructions *)
      datatype 'a t =
         SFail (* Used internally. Running into one is a bug in the layout algorithm *)
       | SEmpty (* Empty document *)
       | SChar of char * 'a t (* Document containing 1 char *)
       | SText of string * 'a t (* Document containing a string *)
       | SLine of int * 'a t (* New line, indenting a given number of spaces *)
       | SAnnPush of 'a * 'a t (* Push a new annotation onto the stack *)
       | SAnnPop of 'a t (* Pop the last annotation from the stack *)

      (*** Annotations *)
      (* Remove annotation *)
      val unAnnotate: 'a t -> 'b t
      (* Modify annotations *)
      val reAnnotate: ('a -> 'b) -> 'a t -> 'b t
      (* Generalized reAnnotate *)
      val alterAnnotations: ('a -> 'b option) -> 'a t -> 'b t

      (* Default layout algorithm *)
      val layoutPretty: PageWidth.t * 'a doc -> 'a t

      (* Smarter layout algorithm *)
      val layoutSmart: PageWidth.t * 'a doc -> 'a t

      (* Layout compactly *)
      val layoutCompact: 'a doc -> 'a t

      (* Convert to a string *)
      val render: 'a t -> string
      (* Render to the screen *)
      val renderIO: TextIO.outstream * 'a t -> unit

   end

   structure DocTree:
   sig
      datatype 'a t =
         TEmpty (* Empty document *)
       | TChar of char (* Document containing 1 char *)
       | TText of string (* Document containing a string *)
       | TLine of int (* New line, indenting a given number of spaces *)
       | TAnn of 'a * 'a t (* A document annotated with the given one *)
       | TConcat of 'a t list (* A list of concatenated documents *)

      (*** Annotations *)
      (* Remove annotations *)
      val unAnnotate: 'a t -> 'b t
      (* Modify annotations *)
      val reAnnotate: ('a -> 'b) -> 'a t -> 'b t
      (* Generalized reAnnotate *)
      val alterAnnotations: ('a -> 'b list) -> 'a t -> 'b t

      (* Convert to the DocTree format from the DocStream format *)
      val fromStream: 'a DocStream.t -> 'a t
   end

end
