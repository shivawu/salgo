package salgo.io

import scala.language.implicitConversions
import salgo.util.TuplePlus

object Dsl {
  sealed trait Format[T] {
    def *(count: Int): Format[List[T]] = ConstRep(this, count)

    def +[T2](elem: Format[T2]): Format[(T, T2)] = Cons(this, elem)
  }

  case object IntFmt extends Format[Int]
  case object LongFmt extends Format[Long]
  case object DoubleFmt extends Format[Double]
  case object Token extends Format[String]
  case object Line extends Format[String]

  private[io] case class Cons[T1, T2](e1: Format[T1], e2: Format[T2]) extends Format[(T1, T2)]
  // Here the arg type List[T] is a place holder for the * operator to work
  // At macro expansion time, the result type for ConstRep is tuples
  private[io] case class ConstRep[T](elem: Format[T], count: Int) extends Format[List[T]]
  private[io] case class ExprRep[T](elem: Format[T], expr: String) extends Format[List[T]]

  implicit def intToFmt(int: Int.type) = IntFmt
  implicit def longToFmt(long: Long.type) = LongFmt
  implicit def doubleToFmt(dbl: Double.type) = DoubleFmt

  implicit object FormatTuplePlus extends TuplePlus[Format] {
    override def plus[A, B](a: Format[A], b: Format[B]): Format[(A, B)] = Cons(a, b)
  }
}
