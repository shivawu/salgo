package salgo.io

import salgo.io.Dsl._

object MacroImpl {
  import scala.reflect.macros.whitebox.Context

  def readImpl[T: c.WeakTypeTag](c: Context)(format: c.Expr[Format[T]]): c.Expr[T] = {
    import c.universe._

    def preEval(elem: c.Tree): c.Tree = {
      elem match {
        case q"$e * $n" =>
          n match {
            case Literal(Constant(x: Int)) =>
              if (x > 22) {
                c.abort(c.enclosingPosition, "Scala doesn't support tuples with >22 elements")
              }
              val constRep = symbolOf[ConstRep.type].asClass.module
              q"$constRep(${preEval(e)}, $x)"

            case _ =>
              val exprRep = symbolOf[ExprRep.type].asClass.module
              q"$exprRep(${preEval(e)}, ${showCode(n)})"
          }
        case q"$a +[$_] $b" =>
          q"${preEval(a)} + ${preEval(b)}"
        case _ =>
          elem
      }
    }

    val realFmt = {
      val pre = preEval(format.tree)
      val expr = c.Expr[Format[T]](c.untypecheck(pre.duplicate)) // This looks stupid...
      c.eval(expr)
    }

    // "self" is a val capturing the current prefix
    // which is then used in the following actions
    val self = TermName(c.freshName("self"))
    val nextToken = q"$self.nextToken()"

    def formatToTree[A](fmt: Format[A], typ: Type): c.Tree = {
      fmt match {
        case Token =>
          q"""
            $nextToken match {
              case None => throw new java.io.EOFException
              case Some(s) => s
            }
          """
        case Line =>
          q"""
            $self.nextLine() match {
              case None => throw new java.io.EOFException
              case Some(l) => l
            }
          """

        case DoubleFmt =>
          val token = formatToTree(Token, weakTypeOf[String])
          q"$token.toDouble"

        case IntFmt =>
          val token = formatToTree(Token, weakTypeOf[String])
          q"$token.toInt"

        case LongFmt =>
          val token = formatToTree(Token, weakTypeOf[String])
          q"$token.toLong"

        case ConstRep(elemFmt, n) =>
          // Thanks to that the return type of whitebox macro is
          // equal to the actual Expr, not the type signature
          // We can implement this ConstRep, easily, by
          // just generating the tuple
          val elemAct = formatToTree(elemFmt, typ.typeArgs(0))

          val tuple = List.fill(n)(q"$elemAct")
          q"(..$tuple)"

        case ExprRep(elemFmt, x) =>
          val expr = c.parse(x)
          val etyp = typ.typeArgs(0)
          val elemAct = formatToTree(elemFmt, etyp)
          q"""
            val buf = collection.mutable.ListBuffer.empty[$etyp]
            var i = 0
            while (i < $expr) {
              buf += $elemAct
              i += 1
            }
            buf.toList
          """

        case Cons(ef1, ef2) =>
          val etyp1 = typ.typeArgs(0)
          val etyp2 = typ.typeArgs(1)
          val act1 = formatToTree(ef1, etyp1)
          val act2 = formatToTree(ef2, etyp2)
          if (ef1.isInstanceOf[Cons[_, _]]) {
            // Flatten the sub Cons to TupleN
            val q"(..$act1s)" = act1
            q"(..$act1s, $act2)"
          }
          else {
            q"($act1, $act2)"
          }
      }
    }

    c.Expr[T] {
      q"""
        val $self = ${c.prefix.tree}
        ${formatToTree(realFmt, weakTypeOf[T])}
      """
    }
  }
}
