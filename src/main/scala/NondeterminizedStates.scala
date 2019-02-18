abstract class NondeterminizedState[A <: Letter] extends State {
  def readLetter(a: A): List[(NondeterminizedState[A], Boolean)]
}

