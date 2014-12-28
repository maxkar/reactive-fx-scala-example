package ru.maxkar.reactive.wave


/**
 * Base class which can create participants and attach
 * them to current "participation context".
 * <p>Wave participables
 * immediately adds new participants into it and allow
 * them to manage resolution order.
 * <p>Default participable works without any context
 * and do not allow anything to update (it denotes
 * context external to the reactive world).
 */
abstract class Participable private[wave]() {
  /**
   * Creates a new participant. New participant
   * may be involved in a current context (if
   * context is a wave) and may get a boot call
   * to reevaluate its state after dependency
   * resolution.
   * <p>Boot callback is not invoked during call
   * to this method. It would be invoked during
   * further wave resolution (if wave is active).
   *
   * @param onBoot boot handler. This handler is invoke during
   * the first attempt to resolve this node.
   * @param onResolved resolved handler. This handler is invoked
   * after this node was resolved. It gives user a chance to
   * complete node update (by checking dependency nodes and updating
   * current node appropriately for example). It is too late to
   * perform wave-related operations (like deferring) so wave is
   * not passed to this handler.
   * @param onCleanup cleanup listener. This listener would be
   * invoked after the propagation wave. Main goal for this method
   * is to reset "events" to some "default" state. Events can't be reset
   * in the <code>onResolved</code> listeners because that events can
   * be consumed by other nodes in the flow.
   */
  def participant(
        onBoot : Wave ⇒ Unit,
        onResolved : Wave ⇒ Unit,
        onCleanup : () ⇒ Unit)
      : Participant
}



/**
 * Participation companion. Implements a default participable.
 */
final object Participable {
  /**
   * Default participation context. Used to denote
   * process external to the "reactive" world.
   * This node should be used every time new participant
   * is needed outside of active propagation
   * (i.e. in most cases).
   */
  final object DefaultParticipable extends Participable {
    override def participant(
          onBoot : Wave ⇒ Unit,
          onResolved : Wave ⇒ Unit,
          onCleanup : () ⇒ Unit)
        : Participant =
      new Participant(onBoot, onResolved, onCleanup)
  }
}

