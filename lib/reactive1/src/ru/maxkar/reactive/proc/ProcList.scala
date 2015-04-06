package ru.maxkar.reactive.proc


/**
 * Queue containing procedures. Cooperates with procedures to provide
 * cool (and relatively efficient) way of managing queues.
 */
private[proc] final class ProcList {
  /** First element of the list. */
  private var head : Procedure = null

  /** Last element of the list. */
  private var last : Procedure = null



  /** Adds procedure into this process list. */
  private[proc] def += (proc : Procedure) : Unit = {
    if (head == null) {
      head = proc
      last = proc
    } else {
      last.next = proc
      last = proc
    }
  }


  /** Checks if this process list is empty. */
  private[proc] def empty() : Boolean =
    head == null



  /** Extracts and returns next procedure. */
  private[proc] def take() : Procedure = {
    var res = head
    head = res.next
    if (head == null)
      last = null
    res.next = null
    res
  }



  /** Moves list into this one. */
  private[proc] def fillFrom(peer : ProcList) : Unit = {
    if (peer.head == null)
      return

    if (head == null) {
      head = peer.head
      last = peer.last
    } else {
      last.next = peer.head
      last = peer.last
    }

    peer.head = null
    peer.last = null
  }
}
