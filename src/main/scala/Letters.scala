class Letter {}

case class MultipliedLetter[A <: Letter, B <: Letter](a: A, b: B)
    extends Letter {}

//Letter over automaton with n states over A alphabet
case class BuchiLetter[A <: Letter](
    reachableStates: List[
      (NondeterminizedState[A], Boolean, NondeterminizedState[A])])
    extends Letter {}
