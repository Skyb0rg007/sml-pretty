
signature Ansi =
sig

  type style
  val op+ : style * style -> style
  val empty : style

  datatype color =
      Black
    | Red
    | Green
    | Yellow
    | Blue
    | Magenta
    | Cyan
    | White

  val color : color -> style
  val bgColor : color -> style
  val colorDull : color -> style
  val bgColorDull : color -> style
  val bold : style
  val italicized : style
  val underlined : style

  val renderIO : TextIO.outstream * style Prettyprint.DocStream.t -> unit

end
