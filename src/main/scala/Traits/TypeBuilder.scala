package Traits

trait TypeBuilder {
  def typeName(): String
  def create[Type](): Type
  def getComparator(): Traits.Comparator
}
