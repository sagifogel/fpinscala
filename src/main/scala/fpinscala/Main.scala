package fpinscala

import fpinscala.chapter10.Monoid._
import fpinscala.chapter10.Part

import scala.{Option => _, Some => _, Stream => _}
object Main {
  def main(args: Array[String]): Unit = {
    val input = "lorem ipsum dolor sit amet, "
    val count = countWords(input)

    println(count)
  }
}
