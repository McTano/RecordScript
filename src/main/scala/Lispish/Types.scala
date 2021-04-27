sealed trait Type
case object NumType extends Type
case object Top extends Type
case class Fun(lhs: Type, rhs: Type, res: Type) extends Type
