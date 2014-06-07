package salgo.io

import scala.language.experimental.macros
import scala.language.higherKinds

import java.io.{Console => _, _}
import Dsl._

trait Input {

  def source: Reader

  private lazy val reader = new BufferedReader(source)

  def nextToken(): Option[String] = {
    var c = Option.empty[Char]
    do {
      c = nextChar()
    } while (c != None && Character.isWhitespace(c.get))
    if (c == None)
      None
    else {
      val buffer = new StringBuilder
      do {
        buffer += c.get
        c = nextChar()
      } while (c != None && !Character.isWhitespace(c.get))
      Some(buffer.toString)
    }
  }

  def nextLine(): Option[String] = {
    val line = reader.readLine()
    if (line == null)
      None
    else
      Some(line)
  }

  def nextChar(): Option[Char] = {
    val c = reader.read()
    if (c < 0)
      None
    else
      Some(c.toChar)
  }

  def read[T](format: Format[T]): Any = macro MacroImpl.readImpl[T]
}

object Input {
  val stdin = apply(Console.in)

  def apply(str: String): Input =
    apply(new StringReader(str))

  def apply(file: File): Input =
    apply(new FileReader(file))

  def apply(input: InputStream): Input =
    apply(new InputStreamReader(input))

  def apply(input: Reader): Input = {
    new Input {
      val source = input
    }
  }
}
