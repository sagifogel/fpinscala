package fpinscala.monoids

trait Monoid[A] {
  def op(a1: A, a2: A): A

  def zero: A
}

sealed trait WC

case class Stub(chars: String) extends WC

case class Part(lStub: String, words: Int, rStub: String) extends WC

object Monoid {

  import fpinscala.parallelism.Par._

  val stringMonoid: Monoid[String] = new Monoid[String] {
    def op(a1: String, a2: String): String = a1 + a2

    val zero = ""
  }

  def listMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
    override def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2

    override def zero: List[A] = Nil
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 + a2

    override def zero: Int = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 * a2

    override def zero: Int = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2

    override def zero: Boolean = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2

    override def zero: Boolean = true
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2

    override def zero: Option[A] = None
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    override def op(a1: A => A, a2: A => A): A => A = a2 compose a1

    override def zero: A => A = identity
  }

  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    override def op(a1: Par[A], a2: Par[A]): Par[A] = map2(a1, a2)(m.op)

    override def zero: Par[A] = unit(m.zero)
  }

  val orderedInt: Monoid[Int => Boolean] = new Monoid[Int => Boolean] {
    override def op(a1: Int => Boolean, a2: Int => Boolean): Int => Boolean =
      i => a1(1) && a2(i)

    override def zero: Int => Boolean = _ => true
  }

  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    override def op(a: WC, b: WC): WC = (a, b) match {
      case (Stub(c), Stub(d)) => Stub(c + d)
      case (Stub(c), Part(l, w, r)) => Part(c + l, w, r)
      case (Part(l, w, r), Stub(c)) => Part(l, w, r + c)
      case (Part(l1, w1, r1), Part(l2, w2, r2)) =>
        Part(l1, w1 + (if ((r1 + l2).isEmpty) 0 else 1) + w2, r2)
    }

    override def zero: WC = Stub("")
  }

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = {
    as.foldRight(m.zero)((a, b) => m.op(f(a), b))
  }

  def foldRightViaFoldMap[A, B](as: List[A], z: => B)(f: (A, B) => B): B = {
    foldMap(as, endoMonoid[B])(f.curried)(z)
  }

  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = v match {
    case Seq() => m.zero
    case Seq(x) => f(x)
    case Seq(x, y) => m.op(f(x), f(y))
    case _ =>
      val half = v.length / 2
      val (left, right) = v.splitAt(half)

      m.op(foldMapV(left, m)(f), foldMapV(right, m)(f))
  }

  def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = {
    foldMapV(v, par(m))(unit[B] _ compose f)
  }

  def countWords(str: String): Int = {
    val wc = foldMapV(str.toIndexedSeq, wcMonoid)(c =>
      if (c == ' ' || c == ','  || c == '.') Part("", 0, "")
      else Stub(c.toString))

    wc match {
      case Part (l, w, r) => w + (if (l.isEmpty) 0 else 1) + (if (r.isEmpty) 0 else 1)
      case Stub(c) => if (c.isEmpty) 0 else 1
    }
  }
}
