package interop

class TraitImpl(name: String) extends MyTrait {
  def traitName = name
}

object TraitImpl {
  def apply = new TraitImpl("foo")
  def apply(name: String) = new TraitImpl(name)
}
