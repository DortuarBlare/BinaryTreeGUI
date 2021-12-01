package TypeBuilders

import Traits.Comparator
import scala.util.Random

class IntegerBuilder extends Traits.TypeBuilder {
  override def typeName() = "Int"

  override def create[Type](): Type = (new Random().nextInt(10000000) + 1).asInstanceOf[Type]

  override def getComparator(): Traits.Comparator = (o1: Any, o2: Any) =>
        o1.asInstanceOf[Int] - o2.asInstanceOf[Int]
}
