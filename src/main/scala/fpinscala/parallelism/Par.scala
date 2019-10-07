package fpinscala.parallelism

import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}

object Par {
  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = (_: ExecutorService) => UnitFuture(a)

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true

    def get(timeout: Long, units: TimeUnit): A = get

    def isCancelled = false

    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      def call: A = a(es).get
    })

  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] = map(parList)(_.sorted)

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = {
    traverse(ps)(asyncF(f))
  }

  def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
    traverse(ps)(identity)
  }

  def traverse[A, B](ps: List[A])(f: A => Par[B]): Par[List[B]] = {
    ps.foldRight(unit(List.empty[B]))((a, b) => map2(f(a), b)(_ :: _))
  }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    parMap(as.filter(f))(identity)
  }

  def delay[A](fa: => Par[A]): Par[A] = es => fa(es)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = es =>
    if (run(es)(cond).get) t(es)
    else f(es)

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = {
    map2(n, sequence(choices))((n, list) => list(n))
  }

  def choice2[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = {
    choiceN(map(cond)(if (_) 0 else 1))(List(t, f))
  }

  def chooser[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] = es => {
    val par = run(es)(map(pa)(choices)).get

    par(es)
  }

  def flatMap[A, B](a: Par[A])(f: A => Par[B]): Par[B] = {
    join(map(a)(f))
  }

  def join[A](a: Par[Par[A]]): Par[A] = es => {
    val par = run(es)(a).get
    par(es)
  }

  def joinViaFlatMap[A](a: Par[Par[A]]): Par[A] = {
    flatMap(a)(identity)
  }

  def map2ViaFlatMap[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] = {
    flatMap(pa)(a => map(pb)(f(a, _)))
  }
}
