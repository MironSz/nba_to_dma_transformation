abstract class Condition[S <: State] {
  def evaluateCondition(t: Transition[S]): Condition[S]

  def acceptanceRates: Int
}

class MullerCondition[S <: State](
    val transitionsOccurances: List[List[(Transition[S], Int)]],
    val previousCondition: MullerCondition[S])
    extends Condition[S] {

  override def acceptanceRates: Int =
    transitionsOccurances.map(t => t.map(t2 => t2._2).min).max
  override def evaluateCondition(
      t: Transition[S]
  ): MullerCondition[S] = {
    val newtransitionsOccurances: List[List[(Transition[S], Int)]] =
      transitionsOccurances.map(l => {
        val l2 = l.map(b => b._1)
        if (l2.contains(t))
          l.map(b => if (b._1 == t) (b._1, b._2 + 1) else b)
        else
          l.map(b => (b._1, 0))
      })
    new MullerCondition[S](newtransitionsOccurances, this)
  }
}

class MullerConditionReversedImageTransducer[
    A <: Letter,
    B <: Letter,
    +S <: DeterminizedStateWithTransducer[A, B]](
    bMullerCondition: MullerCondition[DeterminizedState[B]],
    override val previousCondition: MullerConditionReversedImageTransducer[A,
                                                                           B,
                                                                           S])
    extends MullerCondition[S](transitionsOccurances =
                                 null: List[List[(Transition[S], Int)]],
                               previousCondition = previousCondition) {

  override def evaluateCondition(
      t: Transition[S]): MullerConditionReversedImageTransducer[A, B, S] = {
    val bTransition = new Transition[DeterminizedState[B]](t.from.s, t.to.s)
    val newBMullerCondition = bMullerCondition.evaluateCondition(bTransition)
    new MullerConditionReversedImageTransducer[A, B, S](newBMullerCondition,
                                                        this)
  }
}
