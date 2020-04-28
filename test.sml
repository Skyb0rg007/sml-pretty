
open Prettyprint
open DocStream

infixr 6 ++ <+>

datatype exp
    = App of exp * exp
    | Abs of string * exp
    | Int of int
    | Var of string
    | Let of (string * exp) list * exp

fun pretty_exp x = case x
    of App (a, b) => str "(" ++ pretty_exp a <+> pretty_exp b ++ str ")"
     | Abs (a, b) => str "(\\" ++ str a ++ str "." <+>  (pretty_exp b) ++ str ")"
     | Int n => str (Int.toString n)
     | Var s => str s
     | Let (decs, e) => 
        let
            fun pdec (a, b) = str "val" <+> fill (4, str a) <+> str "=" <+> pretty_exp b
        in
            align (str "let" ++ line ++ indent (4, vcat (map pdec decs)) ++ line ++ str "in" ++ line ++ indent (4, pretty_exp e) ++ line ++ str "end")
        end

val exp = Abs ("x", Let ([("foo", Abs ("y", Var "y"))], App (Var "foo", Int 1)))
val doc : unit doc = pretty_exp exp 
val _ = print (render_string (layoutPretty (Unbounded, doc)) ^ "\n")
handle Fail msg => print ("Fail: " ^ msg ^ "\n")