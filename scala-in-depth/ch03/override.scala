trait Animal {
  def talk: String
}

trait Cat extends Animal {
  override def talk: String = "Meow"
}

trait Dog extends Animal {
  override def talk: String = "Woof"
}

(new Cat with Dog).talk
(new Dog with Cat).talk
