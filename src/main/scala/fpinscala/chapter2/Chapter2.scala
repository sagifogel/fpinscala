package fpinscala.chapter2

import scala.annotation.tailrec

object Chapter2 {
  def fib(n: Int): Int = {
    @tailrec
    def go(current: Int, next: Int, j: Int): Int = {
      if (j == 0) current
      else go(next, current + next, j - 1)
    }

    go(0, 1, n)
  }

  def isSorted[A](as: Array[A])(ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean = {
      if (n >= as.length) true
      else if (ordered(as(n - 1), as(n))) loop(n + 1)
      else false
    }

    loop(1)
  }

  def curry[A, B, C](f: (A, B) => C): A => B => C = a => (b: B) => f(a, b)

  def uncurry[A,B,C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)

  def compose[A,B,C](f: B => C, g: A => B): A => C = a => f(g(a))
}
