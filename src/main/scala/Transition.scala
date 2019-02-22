class Transition[+S <: State](val from: S,val  to: S) {}

case class AcceptingTransition[+S <: State](override val from: S,
                                           override val to: S,
                                           acc: Boolean)
    extends Transition[S](from, to) {}
