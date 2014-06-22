package salgo.util

import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context
import scala.annotation.StaticAnnotation

class TupleExtension extends StaticAnnotation {
  // TODO: Max tuple arity with annotation parameter, is this possible?
  def macroTransform(annottees: Any*): Any = macro TupleExtension.impl
}

object TupleExtension {
  // TODO: parameter can also be a generic type with the given type argument
  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    val inputs = annottees.map(_.tree).toList
    val (method, _) = inputs match {
      case (method: DefDef) :: rest => (method, rest)
      case _ => c.abort(c.enclosingPosition, "TupleExtension must be applied to a method")
    }

    try {
      val q"$modifiers def $name[$typ]($p: $ptyp): $rtyp = $_" = method

      {
        // Check the only argument's type is identical to the type parameter
        val TypeDef(_, TypeName(typArg), _, _) = typ
        val Ident(TypeName(argTyp)) = ptyp
        if (typArg != argTyp) {
          c.abort(c.enclosingPosition, s"Argument ${q"$p"} must be of ${q"$typ"}")
        }

        // Check whether the return type is specified, this is a limitation of Scala overloaded method
        // where the return type must be explicitly specified
        // An alternative way is to rename the original method to a fresshName
        // and make all the tuple extension methods call that method
        rtyp match {
          case TypeTree() =>
            c.abort(c.enclosingPosition, "The return type must be explicit")
          case _ =>
        }
      }

      def copyTypeParam(name: String) =
        TypeDef(typ.mods, TypeName(name), typ.tparams, typ.rhs)

      def copyArgName(num: Int) =
        Ident(TermName(name.toString + num))

      val extensions = for (n <- 2 to 22) yield {
        val ids = (1 to n).toList
        val typs = ids.map(i => copyTypeParam("T" + i))
        val argNames = ids.map(i => copyArgName(i))
        val args = typs.zip(argNames).map { case (t, argName) => q"$argName: ${t.name}" }
        q"$modifiers def $name[..$typs](..$args): $rtyp = $name((..$argNames))"
      }

      c.Expr[Any] {
        q"..${method :: extensions.toList}"
      }
    }
    catch {
      case e: MatchError =>
        c.abort(c.enclosingPosition, s"TupleExtension must be applied to method with one type parameter and one argument")
    }
  }
}
