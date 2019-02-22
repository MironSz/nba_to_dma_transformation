case class AlphabetLetter(c: Char) extends Letter {
  override def allLetters: List[Letter] = super.allLetters
}

 class ExampleNondeterminizedState(
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
      (new ExampleNondeterminizedState(b._1, transitions), b._2))
  }
}

object CSVtoNBA {
  def parse(csvFile: String): ExampleNBA = {
    val bufferedSource = io.Source.fromFile(csvFile)
    val transitions: Map[Int, AlphabetLetter => List[(Int, Boolean)]] =
      bufferedSource.getLines.zipWithIndex
        .map(lineWithIndex => {
//          val index = lineWithIndex._2
          val line = lineWithIndex._1
          val splittedLine = line.split(" ")
          val index = splittedLine(0).toInt

          val letterToTransition = splittedLine.tail
            .map(tDescription => {
              val splittedDescription = tDescription.split(";")
              val letter = splittedDescription(0).charAt(0)
              val transitionsOverLetter = splittedDescription.tail
                .map(t =>
                  (t.split(",")(0).toInt, t.split(",")(1).charAt(0) == 't'))
                .toList
              (letter, transitionsOverLetter)
            })
            .toList
          def f(c: Char) = letterToTransition.find(_._1 == c).orNull._2

          (index, (c: AlphabetLetter) => f(c.c))
        })
        .toMap
    val startingState = new ExampleNondeterminizedState(0, transitions)
    new ExampleNBA((startingState, 0) :: Nil, Nil)
  }

}

class ExampleNBA(currentStates2: List[(ExampleNondeterminizedState, Int)],
                      prevCondition: List[Int])
    extends NBA[AlphabetLetter](currentStates = currentStates2,
                                prevConditions = prevCondition) {
  override lazy val allStates: List[ExampleNondeterminizedState] = null //TODO
}

/*
1 a;2,t;3,f;4 b;2;3;1*/
