package salgo.util

import scala.reflect.macros.blackbox.Context

object MacroAnnotationUtil {

  sealed trait MacroAnnotationTarget

  object MethodDef extends MacroAnnotationTarget
  object ClassDef extends MacroAnnotationTarget
  object VarDef extends MacroAnnotationTarget

  def onMethod(c: Context)(annottee: c.Expr[Any]): c.universe.DefDef = {
    import c.universe._
    annottee.tree match {
      case method: DefDef => method
      case _ => c.abort(c.enclosingPosition, "Annotation can only be applied to method")
    }
  }
}
