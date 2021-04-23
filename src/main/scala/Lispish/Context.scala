sealed abstract class Context[+T] {
  def extend[S >: T](b: Var, v: S): Context[S] = {
    new Binding(this, b, v)
  }
  def lookup(v: Var): Option[T]
}
case object NullContext extends Context[Nothing] {
  def lookup(v: Var): Option[Nothing] = {
    None
  }
}
case class Binding[T](outerContext: Context[T], variable: Var, value: T)
    extends Context[T] {
  def lookup(v: Var): Option[T] = {
    // println(s"looking up ${v} in context ${this}")
    if (v == variable) {
      Some(value)
    } else {
      outerContext.lookup(v)
    }
  }

  def apply(v: Var, t: T) = {
    Binding(NullContext, v, t)
  }

  override def toString() = {
    s"""|{
            |  $variable = $value,
            |  outerContext: {
            |    $outerContext
            |  }
            |}""".stripMargin
  }
}
