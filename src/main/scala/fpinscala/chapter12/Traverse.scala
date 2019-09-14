package fpinscala.chapter12

import fpinscala.chapter10.Monoid
import fpinscala.chapter11.{Monad => _, Functor}
import fpinscala.chapter6.State

trait Traverse[F[_]] extends Functor[F] { self =>

  import Traverse._

  def traverse[G[_], A, B](fa: F[A])(f: A => G[B])(implicit G: Applicative[G]): G[F[B]] =
    sequence(map(fa)(f))

  def sequence[G[_], A](fga: F[G[A]])(implicit G: Applicative[G]): G[F[A]] = traverse(fga)(identity)

  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    traverse[Id, A, B](fa)(f)

  def foldMap[A, M](as: F[A])(f: A => M)(mb: Monoid[M]): M = {
    traverse[({type f[x] = Const[M, x]})#f, A, M](as)(f)(monoidApplicative(mb))
  }

  def traverseS[S, A, B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[({type f[x] = State[S, x]})#f, A, B](fa)(f)(Monad.stateMonad)

  def zipWithIndex[A](ta: F[A]): F[(A, Int)] =
    traverseS(ta)((a: A) => for {
      i <- State.get[Int]
      _ <- State.set(i + 1)
    } yield (a, i)).run(v1 = 0)._1

  def toList[A](fa: F[A]): List[A] =
    traverseS(fa)((a: A) => for {
      as <- State.get[List[A]]
      _ <- State.set(a :: as)
    } yield ()).run(Nil)._2.reverse

  def mapAccum[S, A, B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
    traverseS(fa)((a: A) => for {
      s1 <- State.get[S]
      (b, s2) = f(a, s1)
      _ <- State.set(s2)
    } yield b).run(s)

  def toList2[A](fa: F[A]): List[A] =
    mapAccum(fa, List.empty[A])((a, s) => ((), a :: s))._2.reverse

  def zipWithIndex2[A](fa: F[A]): F[(A, Int)] =
    mapAccum(fa, 0)((a, s) => ((a, s), s + 1))._1

  def reverse[A](fa: F[A]): F[A] = {
    mapAccum(fa, toList(fa).reverse)((_, as) => (as.head, as.tail))._1
  }

  def foldLeft[A, B](fa: F[A], z: B)(f: (B, A) => B): B = {
    mapAccum(fa, z)((a, b) => ((), f(b, a)))._2
  }

  def zipL[A, B](fa: F[A], fb: F[B]): F[(A, Option[B])] =
    (mapAccum(fa, toList(fb)) {
      case (a, Nil) => ((a, None), Nil)
      case (a, b :: bs) => ((a, Some(b)), bs)
    })._1

  def zipR[A, B](fa: F[A], fb: F[B]): F[(Option[A], B)] =
    mapAccum(fb, toList(fa)) {
      case (b, Nil) => ((None, b), Nil)
      case (b, a :: as) => ((Some(a), b), as)
    }._1

  def fuse[G[_], H[_], A, B](fa: F[A])(f: A => G[B], g: A => H[B])
                            (implicit G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) = {
    (sequence(map(fa)(f)), sequence(map(fa)(g)))
  }

  def compose[G[_]](implicit G: Traverse[G]): Traverse[({type f[x] = F[G[x]]})#f] = {
    new Traverse[({type f[x] = F[G[x]]})#f] {
      override def traverse[M[_], A, B](fa: F[G[A]])(f: A => M[B])(implicit M: Applicative[M]): M[F[G[B]]] = {
        self.traverse(fa)(G.traverse(_)(f))
     }
    }
  }
}

case class Tree[+A](head: A, tail: List[Tree[A]])

object Traverse {
  type Id[A] = A
  type Const[M, B] = M

  implicit val applicativeId: Applicative[Id] = new Applicative[Id] {
    override def map2[A, B, C](fa: Id[A], fb: Id[B])(f: (A, B) => C): Id[C] = f(fa, fb)

    override def unit[A](a: => A): Id[A] = a
  }

  implicit val listTraversable: Traverse[List] = new Traverse[List] {
    override def map[A, B](fa: List[A])(f: A => B): List[B] = fa map f

    override def traverse[G[_], A, B](fa: List[A])(f: A => G[B])(implicit G: Applicative[G]): G[List[B]] = {
      fa.foldRight(G.unit(List.empty[B]))((a, b) => G.map2(f(a), b)(_ :: _))
    }
  }

  implicit val optionTraversable: Traverse[Option] = new Traverse[Option] {
    override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa map f

    override def traverse[G[_], A, B](fa: Option[A])(f: A => G[B])(implicit G: Applicative[G]): G[Option[B]] =
      fa.foldRight(G.unit(None: Option[B]))((a, _) => G.map(f(a))(Some.apply))
  }

  implicit val treeTraversable: Traverse[Tree] = new Traverse[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] =
      Tree(f(fa.head), fa.tail.map(map(_)(f)))

    override def traverse[G[_], A, B](fa: Tree[A])(f: A => G[B])(implicit G: Applicative[G]): G[Tree[B]] = {
      val partial = G.apply(G.unit(Tree.apply[B] _ curried))(f(fa.head))
      val mapped = fa.tail.map(traverse(_)(f))

      G.apply(partial)(G.sequence(mapped))
    }

    def traverse2[G[_], A, B](fa: Tree[A])(f: A => G[B])(implicit G: Applicative[G]): G[Tree[B]] = {
      val traversed = G.sequence(fa.tail.map(tree => traverse(tree)(f)))

      G.map2(f(fa.head), traversed)(Tree.apply)
    }
  }

  implicit def monoidApplicative[M](M: Monoid[M]):
  Applicative[({type f[x] = Const[M, x]})#f] = new Applicative[({type f[x] = Const[M, x]})#f] {
    def unit[A](a: => A): M = M.zero

    def map2[A, B, C](m1: M, m2: M)(f: (A, B) => C): M = M.op(m1, m2)
  }
}
