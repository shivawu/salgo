package salgo.io

class InputTest extends Input {
  val input = Console.in

  val n = this read Int
  val arr = this read (Int * n)
  val q = t._1


  sealed trait A[T]
  case class C[T](t: T) extends A[T]
  case class B[T1, T2](t1: A[T1], t2: A[T2]) extends A[(T1, T2)]

  val a: A[(Int, Int)] = new B(C(1), C(2))

  def test[T](a: A[T]): T = {
    a match {
      case C(t) =>
        t
      case B(t1, t2) =>
        val at1 = test(t1)
        val at2 = test(t2)
        (at1, at2)
    }
  }

}
