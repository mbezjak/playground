object Now
object simulate {
  def once(behaviour: () => Unit) = new {
    def right(now: Now.type): Unit = behaviour()
  }
}

simulate once { () => println("-> simulate once") } right Now
