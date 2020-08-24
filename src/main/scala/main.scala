import comptime._
import functional.{given _, _}

import scala.compiletime._

@main def main =
  val a = (1, false, (), "Halp!")
  val size: 4 = constValue[Size[a.type]]
  println(size)

  val initialState = 1.to(10).toList
  println(s"initialState=$initialState")

  val (finalState, result) = crunchNumbers(_ + 1)(initialState)
  println(s"finalState=$finalState, result=$result")

def crunchNumbers(f: Int => Int): StateM[Seq[Int]][Int] =
  def take = for
    items       <- StateM.get[Seq[Int]]
    (head, tail) = (items.head, items.tail)
    _           <- StateM.set(tail)
  yield head

  for
    first <- take.fmap(f)
    second <- take.fmap(f)
    third <- take.fmap(f)
  yield first + second + third
