package fpinscala.implicits

object Function1Syntax {
    implicit class Function2Syntax[-T1, -T2, +R](val f: (T1, T2) => R) extends AnyVal {
      def flip: (T2, T1) => R = (t2, t1) => f(t1, t2)
    }
}
