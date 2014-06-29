package salgo.util

import language.experimental.macros
import scala.reflect.macros.blackbox.Context
import scala.annotation.StaticAnnotation

class CurriedUpdate extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro CurriedUpdate.curry
}

object CurriedUpdate {

  import MacroAnnotationUtil._

  private val updateRenamed = "updateR"

  def curry(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    def renameMethod(method: DefDef, newName: String): DefDef = {
      DefDef(
        method.mods, TermName(newName), method.tparams,
        method.vparamss,
        method.tpt, method.rhs)
    }

    val methodDef = onMethod(c)(annottees(0))
    if (methodDef.name.decodedName.toString != "update") {
      c.abort(c.enclosingPosition, "CurriedUpdate can only be applied to the update method")
    }
    if (methodDef.vparamss.size != 2) {
      c.abort(c.enclosingPosition, "Curried update must have two argument list")
    }
    if (methodDef.vparamss(0).size == 0) {
      c.abort(c.enclosingPosition, "The first argument list must not be empty")
    }
    if (methodDef.vparamss(1).size != 1) {
      c.abort(c.enclosingPosition, "The second argument list must have only one element")
    }

    c.Expr[Any] {
      q"""
        import scala.language.experimental.macros

        ${renameMethod(methodDef, updateRenamed)}
        def update(values: Any*): Unit = macro salgo.util.CurriedUpdate.updateMacroDispatcher
      """
    }
  }

  def updateMacroDispatcher(c: Context)(values: c.Expr[Any]*): c.Expr[Unit] = {
    import c.universe._

    val updateParams = values.take(values.size - 1)
    c.Expr[Unit] {
      Apply(
        Apply(
          Select(c.prefix.tree, TermName(updateRenamed)),
          updateParams.map(_.tree).toList
        ),
        List(values.last.tree)
      )
    }
  }
}
