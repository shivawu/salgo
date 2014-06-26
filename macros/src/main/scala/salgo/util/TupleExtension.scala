package salgo.util

import scala.language.experimental.macros
import scala.language.{higherKinds, existentials}
import scala.reflect.macros.whitebox.Context
import scala.annotation.StaticAnnotation

class TupleExtension extends StaticAnnotation {
  // TODO: Max tuple arity with annotation parameter, is this possible?
  def macroTransform(annottees: Any*): Any = macro TupleExtension.impl
}

object TupleExtension {
  private val maxArity = 10

  // TODO: parameter can also be a generic type with the given type argument
  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    println("prefix: " + showRaw(c.prefix))
    println("app: " + showRaw(c.macroApplication))

    val inputs = annottees.map(_.tree).toList
    val (method, _) = inputs match {
      case (method: DefDef) :: rest => (method, rest)
      case _ => c.abort(c.enclosingPosition, "TupleExtension must be applied to a method")
    }

    try {
      val q"$modifiers def $methodName[$typeParam]($paramName: $paramType): $returnType = $_" = method

      val paramBaseType = {
        val tq"$paramBaseType[$_]" = paramType
        c.typecheck(paramBaseType, mode = c.TYPEmode).tpe
      }

      val tplus = typeOf[TuplePlus[X forSome {type X[_]}]]
      val ptype = internal.typeRef(NoPrefix, tplus.typeSymbol, paramBaseType.etaExpand :: Nil)

      val imp = c.inferImplicitValue(ptype)
      println("inferred " + showRaw(imp))
      println("inferred " + showCode(imp))

      {
        // Check the only argument's type is identical to the type parameter
        val TypeDef(_, TypeName(typArg), _, _) = typeParam
        val Ident(TypeName(argTyp)) = paramType
        if (typArg != argTyp) {
          c.abort(c.enclosingPosition, s"Argument ${q"$paramName"} must be of ${q"typeParam"}")
        }

        // Check whether the return type is specified, this is a limitation of Scala overloaded method
        // where the return type must be explicitly specified
        // An alternative way is to rename the original method to a fresshName
        // and make all the tuple extension methods call that method
        returnType match {
          case TypeTree() =>
            c.abort(c.enclosingPosition, "The return type must be explicit")
          case _ =>
        }
      }

      def copyTypeParam(name: String) =
        TypeDef(typeParam.mods, TypeName(name), typeParam.tparams, typeParam.rhs)

      def copyArgName(num: Int) =
        Ident(TermName(paramName.toString + num))

      val extensions = for (n <- 2 to maxArity) yield {
        val ids = (1 to n).toList
        val typs = ids.map(i => copyTypeParam("T" + i))
        val argNames = ids.map(i => copyArgName(i))
        val args = typs.zip(argNames).map { case (t, argName) => q"$argName: ${t.name}" }
        q"$modifiers def $methodName[..$typs](..$args): $returnType = $methodName((..$argNames))"
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

class TestAnnotation(val x: Int, val y: Int) extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro TestAnnotation.impl
}

object TestAnnotation {
  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    annottees(0)
  }
}
/*
implicit object ListTuplePlus extends TuplePlus[List] {
  def plus[A, B](a: List[A], b: List[B]): List[(A, B)] = ???
}


@salgo.util.TupleExtension({
  forA {
    FOR << XX
    X << Y
  } yieldA FOR
}) object $
 */