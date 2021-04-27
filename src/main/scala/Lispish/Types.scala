sealed abstract class Type

case object BoolType extends Type

case object NumType extends Type
case object TopType extends Type

case class BinopType(lhs: Type, rhs: Type, res: Type) extends Type
