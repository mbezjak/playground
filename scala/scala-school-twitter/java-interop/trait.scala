package interop

trait MyTrait {
  def traitName: String
  def upperTraitName = traitName.toUpperCase
}
