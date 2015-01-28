package ru.maxkar.fx


/**
 * List of event listerers.
 * Incapsulates them and provides framework-safe
 * listener list manipulation.
 * @param T type of the listener in this list.
 */
final class ListenerList[T] {
  /** Collected items. */
  private val items = new java.util.concurrent.CopyOnWriteArrayList[T]



  /** Adds a listener into this list. */
  def += (listener : T) : Unit =
    items add listener


  /** Removes a single listener instance from this list. */
  def -= (listener : T) : Unit =
    items remove listener



  /** Executes an operation for each listener. */
  def forEach(cb : T ⇒ Unit) : Unit = {
    val itr = items.iterator()
    while (itr.hasNext())
      cb(itr.next())
  }
}
