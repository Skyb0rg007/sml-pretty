
structure Ansi :> Ansi =
struct

  datatype intensity = Vivid | Dull

  datatype color =
      Black
    | Red
    | Green
    | Yellow
    | Blue
    | Magenta
    | Cyan
    | White

  type style = {
    foreground: (intensity * color) option,
    background: (intensity * color) option,
    bold: bool,
    italics: bool,
    underline: bool
  }

  fun option_combine (SOME x, _) = SOME x
    | option_combine (NONE, x) = x

  fun op+
    ({foreground = foreground1, background = background1, bold = bold1, italics = italics1, underline = underline1},
     {foreground = foreground2, background = background2, bold = bold2, italics = italics2, underline = underline2}) =
     { foreground = option_combine (foreground1, foreground2)
     , background = option_combine (background1, background2)
     , bold = bold1 orelse bold2
     , italics = italics1 orelse italics2
     , underline = underline1 orelse underline2
     }

  val empty = { foreground = NONE, background = NONE, bold = false, italics = false, underline = false }

  fun concat styles = List.foldr op+ empty styles

  fun color c = { foreground = SOME (Vivid, c), background = NONE, bold = false, italics = false, underline = false } 
  fun bgColor c = { foreground = NONE, background = SOME (Vivid, c), bold = false, italics = false, underline = false } 
  fun colorDull c = { foreground = SOME (Dull, c), background = NONE, bold = false, italics = false, underline = false } 
  fun bgColorDull c = { foreground = NONE, background = SOME (Dull, c), bold = false, italics = false, underline = false } 
  val bold = { foreground = NONE, background = NONE, bold = true, italics = false, underline = false }
  val italicized = { foreground = NONE, background = NONE, bold = false, italics = false, underline = false }
  val underlined = { foreground = NONE, background = NONE, bold = false, italics = false, underline = true }

  fun output_style (out, {foreground, background, bold, italics, underline}) =
  let
    val color_to_code =
      fn Black   => 0
       | Red     => 1
       | Green   => 2
       | Yellow  => 3
       | Blue    => 4
       | Magenta => 5
       | Cyan    => 6
       | White   => 7

    fun print_code n = TextIO.output (out, ";" ^ Int.toString n)
    fun print_str s = TextIO.output (out, s)
  in
    print_str "\027[0";
    case foreground
      of SOME (Vivid, c) => print_code (Int.+ (90, color_to_code c))
       | SOME (Dull, c) => print_code (Int.+ (30, color_to_code c))
       | NONE => ();
    case background
      of SOME (Vivid, c) => print_code (Int.+ (100, color_to_code c))
       | SOME (Dull, c) => print_code (Int.+ (40, color_to_code c))
       | NONE => ();
    if bold
      then print_code 1
      else print_code 22;
    if italics
      then print_code 3
      else print_code 23;
    if underline
      then print_code 4
      else print_code 24;
    print_str "m"
  end

  fun renderIO (out, sds) =
  let
    val noStyle = empty
    open Prettyprint
    open DocStream
    val stack = ref [noStyle]
    fun push x = stack := x :: !stack
    fun pop () = let val s = !stack in stack := List.tl s; List.hd s end
    fun peek () = List.hd (!stack)

    fun go SFail = raise Fail "Uncaught SFail"
      | go SEmpty = ()
      | go (SChar (c, rest)) = (TextIO.output1 (out, c); go rest)
      | go (SText (t, rest)) = (TextIO.output (out, t); go rest)
      | go (SLine (i, rest)) = (TextIO.output1 (out, #"\n"); TextIO.output (out, CharVector.tabulate (i, fn _ => #" ")); go rest)
      | go (SAnnPush (s, rest)) =
          let
            val current_style = peek()
            val new_style = s + current_style
          in
            push new_style; output_style (out, new_style); go rest
          end
      | go (SAnnPop rest) = (pop(); output_style (out, peek()); go rest)
  in
    go sds;
    if !stack <> [noStyle]
    then raise Fail "Stack not fully consumed"
    else ()
  end

end
