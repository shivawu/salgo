package salgo.collection.array

trait ArrayExtension {
  implicit class ArrayExt2[T](underlay: Array[Array[T]]) {
    def apply(a: Int, b: Int): T = underlay(a)(b)
    def update(a: Int, b: Int, value: T) = underlay(a).update(b, value)
  }

  implicit class ArrayExt3[T](underlay: Array[Array[Array[T]]]) {
    def apply(a: Int, b: Int): Array[T] = underlay(a)(b)
    def apply(a: Int, b: Int, c: Int): T = underlay(a)(b)(c)
    def update(a: Int, b: Int, c: Int, value: T) = underlay(a)(b).update(c, value)
  }

  implicit class ArrayExt4[T](underlay: Array[Array[Array[Array[T]]]]) {
    def apply(a: Int, b: Int): Array[Array[T]] = underlay(a)(b)
    def apply(a: Int, b: Int, c: Int): Array[T] = underlay(a)(b)(c)
    def apply(a: Int, b: Int, c: Int, d: Int): T = underlay(a)(b)(c)(d)
    def update(a: Int, b: Int, c: Int, d: Int, value: T) = underlay(a)(b)(c).update(d, value)
  }

  implicit class ArrayExt5[T](underlay: Array[Array[Array[Array[Array[T]]]]]) {
    def apply(a: Int, b: Int): Array[Array[Array[T]]] = underlay(a)(b)
    def apply(a: Int, b: Int, c: Int): Array[Array[T]] = underlay(a)(b)(c)
    def apply(a: Int, b: Int, c: Int, d: Int): Array[T] = underlay(a)(b)(c)(d)
    def apply(a: Int, b: Int, c: Int, d: Int, e: Int): T = underlay(a)(b)(c)(d)(e)
    def update(a: Int, b: Int, c: Int, d: Int, e: Int, value: T) = underlay(a)(b)(c)(d).update(e, value)
  }
}
