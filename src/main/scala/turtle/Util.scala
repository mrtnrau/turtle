package turtle

object util {

  def lift2[A,B,C,E](f: A => B => C)(ea: Either[E, A])(eb: Either[E, B]): Either[E, C] =
    for {
      a <- ea
      b <- eb
    } yield f(a)(b)

}