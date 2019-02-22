class Letter {
  def allLetters: List[Letter] = Nil
}
case class MultipliedLetter[A <: Letter, B <: Letter](a: A, b: B)
    extends Letter {
//	This can remain like this, multiplied automatons are unused, lack of static methods is weird
  override def allLetters: List[MultipliedLetter[A, B]] = Nil
}

//Letter over automaton with ??? states over A alphabet
case class BuchiLetter[A <: Letter](
    transitions: List[AcceptingTransition[NondeterminizedState[A]]])
    extends Letter {
//	TODO
  override def allLetters: List[BuchiLetter[A]] = Nil

}
