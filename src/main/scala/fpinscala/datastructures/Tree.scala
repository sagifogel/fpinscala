package fpinscala.datastructures

import scala.Function.const

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](tree: Tree[A]): Int = tree match {
    case Branch(left, right) => 1 + size(left) + size(right)
    case Leaf(_) => 1
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Branch(left, right) => maximum(left).max(maximum(right))
    case Leaf(value) => value
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Branch(left, right) => 1 + depth(left).max(depth(right))
    case Leaf(_) => 0
  }

  def map[A, B](as: Tree[A])(f: A => B): Tree[B] = as match {
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    case Leaf(value) => Leaf(f(value))
  }

  def fold[A, B](as: Tree[A])(f: A => B)(b: (B, B) => B): B = as match {
    case Branch(left, right) => b(fold(left)(f)(b), fold(right)(f)(b))
    case Leaf(value) => f(value)
  }

  def size2[A](tree: Tree[A]): Int = {
    fold(tree)(const(1))(1 + _ + _)
  }

  def maximum2(t: Tree[Int]): Int = {
    fold(t)(identity)(math.max)
  }

  def depth2[A](t: Tree[A]): Int = {
    fold(t)(const(0))(1 + _.max(_))
  }

  def mapViaFold[A, B](t: Tree[A])(f: A => B): Tree[B] = {
    val composed: A => Tree[B] = Leaf.apply[B] _ compose f

    fold(t)(composed)(Branch.apply)
  }
}
