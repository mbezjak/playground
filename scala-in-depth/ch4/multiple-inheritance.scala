trait Property {
  val name: String
  override val toString = "Property(" + name + ")"
}

val x = new Property { override val name = "HI" }
val y = new { val name = "HI" } with Property
