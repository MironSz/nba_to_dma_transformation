case class Transition[-S <: State](from: S, to: S) {}

case class AcceptingTransition[-S <: State](override val from: S,
                                           override val to: S,
                                           acc: Boolean)
    extends Transition[S](from, to) {}
