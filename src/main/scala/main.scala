import comptime._
import functional._

import scala.compiletime._

@main def main =
  val a = (1, false, (), "Halp!")
  val size: 4 = constValue[Size[a.type]]
  println(size)
