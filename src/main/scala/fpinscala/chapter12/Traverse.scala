package fpinscala.chapter12

import fpinscala.chapter11.Functor

trait Traverse[F[_]] extends Functor[F] {
  import Traverse._

  def traverse[G[_], A, B](fa: F[A])(f: A => G[B])(implicit G: Applicative[G]): G[F[B]] =
    sequence(map(fa)(f))

  def sequence[G[_], A](fga: F[G[A]])(implicit G: Applicative[G]): G[F[A]] = traverse(fga)(identity)

  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    traverse[Id, A, B](fa)(f)
}

case class Tree[+A](head: A, tail: List[Tree[A]])

object Traverse {
  type Id[A] = A

  implicit val applicativeId: Applicative[Id] = new Applicative[Id] {
    override def map2[A, B, C](fa: Id[A], fb: Id[B])(f: (A, B) => C): Id[C] = f(fa, fb)

    override def unit[A](a: => A): Id[A] = a
  }

  val listTraversable: Traverse[List] = new Traverse[List] {
    override def map[A, B](fa: List[A])(f: A => B): List[B] = fa map f

    override def traverse[G[_], A, B](fa: List[A])(f: A => G[B])(implicit G: Applicative[G]): G[List[B]] = {
      fa.foldRight(G.unit(List.empty[B]))((a, b) => G.map2(f(a), b)(_ :: _))
    }
  }

  val optionTraversable: Traverse[Option] = new Traverse[Option] {
    override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa map f

    override def traverse[G[_], A, B](fa: Option[A])(f: A => G[B])(implicit G: Applicative[G]): G[Option[B]] =
      fa.foldRight(G.unit(None: Option[B]))((a, _) => G.map(f(a))(Some.apply))
  }

  val treeTraversable: Traverse[Tree] = new Traverse[Tree] {
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
}
