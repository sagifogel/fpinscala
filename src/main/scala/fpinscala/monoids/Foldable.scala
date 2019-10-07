package fpinscala.monoids

import fpinscala.monoids.Monoid._
import fpinscala.syntax.Syntax._
import scala.Function.const
import fpinscala.datastructures.{Branch, Leaf, Tree}

trait Foldable[F[_]] {
  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
    foldMap(as)(f.curried)(endoMonoid[B])(z)

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    foldMap(as)(f.flip.curried)(endoMonoid[B])(z)

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)


  def toList[A](fa: F[A]): List[A] = {
    foldMap(fa)(a => List(a))(listMonoid[A])
  }
}

object Foldable {
  val listFoldable: Foldable[List] = new Foldable[List] {
    override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
      as.foldRight(mb.zero)((a, b) => mb.op(f(a), b))
  }

  val indexedSeqFoldable: Foldable[IndexedSeq] = new Foldable[IndexedSeq] {
    override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
      as.foldRight(mb.zero)((a, b) => mb.op(f(a), b))
  }

  val streamFoldable: Foldable[Stream] = new Foldable[Stream] {
    override def foldMap[A, B](as: Stream[A])(f: A => B)(mb: Monoid[B]): B =
      as.foldRight(mb.zero)((a, b) => mb.op(f(a), b))
  }

  val treeFoldable: Foldable[Tree] = new Foldable[Tree] {
    override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B = as match {
      case Branch(left, right) => mb.op(foldMap(left)(f)(mb), foldMap(right)(f)(mb))
      case Leaf(value) => f(value)
    }
  }

  val optionFoldable: Foldable[Option] = new Foldable[Option] {
    override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B = as match {
      case Some(value) => f(value)
      case None => mb.zero
    }
  }

  def productMonoid[A,B](A: Monoid[A], B: Monoid[B]): Monoid[(A,B)] = new Monoid[(A, B)] {
    override def op(a1: (A, B), a2: (A, B)): (A, B) =
      (A.op(a1._1, a2._1), B.op(a1._2, a2._2))

    override def zero: (A, B) = (A.zero, B.zero)
  }

  def mapMergeMonoid[K,V](V: Monoid[V]): Monoid[Map[K, V]] =
    new Monoid[Map[K, V]] {
      def zero: Map[K, V] = Map[K,V]()
      def op(a: Map[K, V], b: Map[K, V]): Map[K, V] =
        (a.keySet ++ b.keySet).foldLeft(zero) { (acc,k) =>
          acc.updated(k, V.op(a.getOrElse(k, V.zero),
            b.getOrElse(k, V.zero)))
        }
    }

  def functionMonoid[A,B](B: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
    override def op(a1: A => B, a2: A => B): A => B = a => B.op(a1(a), a2(a))

    override def zero: A => B = const(B.zero)
  }

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =  {
    foldMapV(as, mapMergeMonoid[A, Int](intAddition))(a => Map(a -> 1))
  }
}
