package comptime

import scala.compiletime._
import scala.compiletime.ops._

type Size[T <: Tuple] <: Int = T match
  case EmptyTuple => 0
  case _ *: t     => S[Size[t]]

