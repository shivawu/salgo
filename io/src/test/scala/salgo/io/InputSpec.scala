package salgo.io

import org.scalatest.WordSpec
import org.scalatest.Assertions._
import java.io.EOFException
import salgo.io.Dsl._

class InputSpec extends WordSpec {

  "Reading from string" when {
    "empty input" should {
      "throw EOFException" in {
        intercept[EOFException] {
          Input("").read(Int)
        }
      }
    }

    "input with 1 token" should {
      "return the result with the right type, or throws Exception" in {
        val text = "1"
        assert(1 == Input(text).read(Int))
        assert("1" == Input(text).read(Token))
        intercept[EOFException] {
          Input(text).read(Int + Int)
        }

        assert(1.5 == Input("1.500").read(Double))
        assert("1 2" == Input("1 2\n3").read(Line))
      }
    }

    "input with 2 tokens" should {
      "return the result no matter the format is" in {
        val text = "1 2"
        assert((1, 2) == Input(text).read(Int + Int))
        assert((1, 2) == Input(text).read(Int * 2))
        assert(("1", 2) == Input(text).read(Token + Int))
        assert("1 2" == Input(text).read(Line))
      }
    }

    "input with 3 tokens" should {
      "return the result as a tuple after flattenning" in {
        val text = "1 2 3"
        assert((1, 2, 3) == Input(text).read(Int * 3))
        assert((1, 2, 3) == Input(text).read(Int + Int + Int))
        assert(((1, 2), 3) == Input(text).read(Int * 2 + Int))
        assert((1, (2, 3)) == Input(text).read(Int + Int * 2))
        assert((1, (2, 3)) == Input(text).read(Int + (Int + Int)))
      }
    }

    "input with n tokens" should {
      val text = (1 to 10).mkString(" ")
      val n = 10

      "return a list" in {
        assert((1 to 5).toList == Input(text).read(Int * (n / 2)))
        assert((1 to 10).toList == Input(text).read(Int * n))
        intercept[EOFException] { Input(text).read(Int * (n + 2)) }
      }
      "return a list in a closure" in {
        val a = 10
        def f(n: Int) = Input(text).read(Int * (n * a / a + 3 - 3))

        {
          val a = 0
          assert((1 to 5).toList == f(n / 2))
          assert((1 to 10).toList == f(n))
          intercept[EOFException] { f(n + 2) }
        }
      }
    }
  }
}
