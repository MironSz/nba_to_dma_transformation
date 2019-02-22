case class AlphabetLetter(c: Char) extends Letter {
  override def allLetters: List[Letter] = super.allLetters
}

case class ExampleNondeterminizedState(
    stateId: Int,
    transitions: Map[Int, AlphabetLetter => List[(Int, Boolean)]])
    extends NondeterminizedState[AlphabetLetter] {
  override def readLetter(
      a: AlphabetLetter
  ): List[(NondeterminizedState[AlphabetLetter], Boolean)] = {
    val fNextState: AlphabetLetter => List[(Int, Boolean)] = transitions
      .get(stateId)
      .orNull
    fNextState(a).map(b =>
      (ExampleNondeterminizedState(b._1, transitions), b._2))
  }
}

object CSVtoNBA {
  def parse(csv: String): ExampleNBA = null
//	TODO
}

case class ExampleNBA(currentStates2: List[(ExampleNondeterminizedState, Int)],
                      prevCondition: List[Int])
    extends NBA[AlphabetLetter](currentStates = currentStates2,
                                prevConditions = prevCondition) {
  override lazy val allStates: List[ExampleNondeterminizedState] = null //TODO
}
