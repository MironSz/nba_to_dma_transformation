abstract class TransducerState[A <: Letter, B <: Letter] extends State {
  def readLetter(a: A): (B, TransducerState[A, B])

  def dmaReversedImage(dma: DMA[B],
                       startingStateInA: DeterminizedState[B]): DMA[A] =
    new DMA[A](
      DeterminizedStateWithTransducer[A, B](dma.startingState, this),
      MullerConditionImageTransducer(
        dma.condition,
        DeterminizedStateWithTransducer(dma.startingState, this))
    )
}

case class LetterToBuchiTransducer[A <: Letter](
    reachedStates: List[NondeterminizedState[A]])
    extends TransducerState[A, BuchiLetter[A]] {
  override def readLetter(
      a: A): (BuchiLetter[A], LetterToBuchiTransducer[A]) = {
    val buchiLetter = BuchiLetter[A](
      reachedStates
        .flatMap(
          state =>
            state
              .readLetter(a)
              .map(transition => (state, transition._2, transition._1)))
    )
    val newReachedStates =
      buchiLetter.reachableStates.map(letter => letter._3).distinct
    (buchiLetter, LetterToBuchiTransducer[A](newReachedStates))
  }
}

case class DagToTreeTransducer[A <: Letter](
    reachedStatesSortedByProfile: List[NondeterminizedState[A]])
    extends TransducerState[BuchiLetter[A], BuchiLetter[A]] {

  override def readLetter(a: BuchiLetter[A])
    : (BuchiLetter[A], TransducerState[BuchiLetter[A], BuchiLetter[A]]) = {
    // Usunąć nieosiąglne stany
    val allReachableStates: List[NondeterminizedState[A]] =
      reachedStatesSortedByProfile
    val allReachableStatesWithInd: List[(NondeterminizedState[A], Int)] =
      allReachableStates.zipWithIndex

    val removedUnreachableFromLetter
      : List[(NondeterminizedState[A], Boolean, NondeterminizedState[A])] =
      a.reachableStates.filter(b => allReachableStates contains b._1)

    val reachableStatesWithDuplicates
      : List[(Int, Boolean, NondeterminizedState[A], NondeterminizedState[A])] =
      allReachableStatesWithInd
        .map(rssbf =>
          (rssbf._2, removedUnreachableFromLetter.filter(_._1 == rssbf._1)))
        .map(b => b._2.map(c => (b._1, c._2, c._1, c._3)))
        .flatten

    val reachableStatesWithDuplicatesSorted =
      reachableStatesWithDuplicates
        .sortWith((a, b) => if (a._1 != b._1) a._1 < b._1 else a._2 < b._2)
        .map(c => (c._2, c._3, c._4))

    val reachableStatesWithoutDuplicates =
      reachableStatesWithDuplicatesSorted
        .foldLeft(Nil: List[(Boolean,
                             NondeterminizedState[A],
                             NondeterminizedState[A])])(
          (l: List[(Boolean, NondeterminizedState[A], NondeterminizedState[A])],
           state: (Boolean, NondeterminizedState[A], NondeterminizedState[A])) =>
            if (l.map(c => (c._2, c._3)).contains((state._2, state._3))) l
            else state :: l)
        .map(c => (c._2, c._1, c._3))

    val newBuchiLetter = BuchiLetter[A](reachableStatesWithoutDuplicates)

    val newReachedStatesSortedByProfile =
      reachableStatesWithoutDuplicates.map(c => c._3)
    (newBuchiLetter, DagToTreeTransducer[A](newReachedStatesSortedByProfile))
    //mogę reachedStateSortedByProfile zzipować z
  }
}
