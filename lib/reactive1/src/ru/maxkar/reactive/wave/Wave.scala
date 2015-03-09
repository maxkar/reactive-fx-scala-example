package ru.maxkar.reactive.wave


import java.util.IdentityHashMap
import scala.collection.mutable.Queue
import scala.collection.mutable.ArrayBuffer

/**
 * One update/propagation transaction.
 * @param parent parent wave for this one. If it is not <code>null</code> then this
 * (current) wave is an aftershock of the previous (parent) one.
 */
class Wave private[wave](private val parent : Wave){
  /** Status of all nodes engaged in the wave. */
  private var currentNodes = new IdentityHashMap[Node[_], ResolveState[_]]()



  /** Node "boot" order. */
  private var bootQueue = new Queue[Node[_]]



  /** Queue of nodes ready to be processed. */
  private var completeQueue = new Queue[Node[_]]



  /**
   * Returns value of the node during this wave.
   * Returns default node value if node does not participate in this wave
   * @throws IllegalStateException if node is in the wave but is not
   * resolved yet.
   */
  def valueOf[T](node : Node[T]) : T = {
    val state = currentNodes.get(node).asInstanceOf[ResolveState[T]]
    state match {
      case null ⇒ node.defaultValue
      case Resolved(x) ⇒ x
      case _ ⇒ throw new IllegalStateException("Illegal node state " + state)
    }
  }



  /**
   * Schedules node for update/refresh and returns update status.
   */
  private def refresh(node : Node[_]) : RefreshStatus = {
    var cur = this
    do {
      val state = cur.currentNodes.get(node)
      state match {
        case null ⇒ cur = cur.parent
        case Resolved(_) ⇒ return NeedSubwave
        case Boot ⇒ return RefreshScheduled
        case Updating(c, _, _) ⇒
          c.active = false
          cur.currentNodes.put(node, Boot)
          cur.bootQueue += node
          return RefreshScheduled
        case Completing(_) ⇒
          cur.currentNodes.put(node, Boot)
          cur.bootQueue += node
          return RefreshScheduled
      }
      cur = cur.parent
    } while (cur != null)
    return NotFound
  }




  /**
   * Adds nodes to be processed. Creates nested wave if needed.
   */
  private def processBatch(items : Seq[Node[_]]) : Unit = {
    val newItems = new IdentityHashMap[Node[_], Unit]()
    var useSubctx = false
    val queue = new Queue[Node[_]]
    queue ++= items

    while (!queue.isEmpty) {
      val item = queue.dequeue()
      if (!newItems.containsKey(item))
        refresh(item) match {
          case NeedSubwave ⇒
            useSubctx = true
            newItems.put(item, ())
            mergeQ(queue, item.upstream())
          case NotFound ⇒
            newItems.put(item, ())
            mergeQ(queue, item.upstream())
          case RefreshScheduled ⇒ ()
        }
    }


    if (!useSubctx)
      batchEnqueue(newItems)
    else {
      val sub = new Wave(this)
      sub.batchEnqueue(newItems)
      sub.process()
      mergeR(sub)
    }
  }



  /** Internal batch enqueueing items in the node. */
  private def batchEnqueue(items : IdentityHashMap[Node[_], Unit]) : Unit = {
    val itr = items.keySet().iterator()
    while (itr.hasNext()) {
      val key = itr.next()
      currentNodes.put(key, Boot)
      bootQueue += key
    }
  }




  /** Adds all deps into the queue. */
  private def mergeQ(queue : Queue[Node[_]], itr : NodeIterator) : Unit = {
    var dep = itr()
    while (dep != null) {
      queue += dep
      dep = itr()
    }
  }



  /** Merges result with child result. */
  private def mergeR(peer : Wave) : Unit = {
    val itr = peer.currentNodes.entrySet().iterator()
    while (itr.hasNext()) {
      val pair = itr.next()
      val self = currentNodes.get(pair.getKey())
      if (self == null)
        currentNodes.put(pair.getKey(), pair.getValue())
      else
        currentNodes.put(pair.getKey(), merge(
          pair.getKey().asInstanceOf[Node[Any]], self, pair.getValue()))
    }
  }



  private def merge[T](key : Node[T], v1 : ResolveState[_], v2 : ResolveState[_]) : ResolveState[T] =
    Resolved(key.merge(
      v1.asInstanceOf[Resolved[T]].value,
      v2.asInstanceOf[Resolved[T]].value))



  /** Processes wave updates. */
  private def process() : Unit = {
    while (!bootQueue.isEmpty || !completeQueue.isEmpty) {
      while (!bootQueue.isEmpty)
        boot(bootQueue.dequeue())

      while (!completeQueue.isEmpty)
        complete(completeQueue.dequeue())
    }


    val viter = currentNodes.values().iterator()
    while (viter.hasNext())
      if (!viter.next().isInstanceOf[Resolved[_]])
        throw new IllegalStateException("Cyclic dependency found in the update loop")
  }



  /**
   * Completes a node update.
   */
  private def complete(node : Node[_]) : Unit = {
    val state = currentNodes.get(node).asInstanceOf[Completing]
    val res = node.action(this)
    currentNodes.put(node, Resolved(res))

    val diter = state.deps.iterator
    while (diter.hasNext)
      roll(diter.next)
  }



  /** Rolls a state in the node. */
  private def roll(dep : ActiveDep) : Unit = {
    if (!dep.active)
      return

    val node = dep.node
    val state = currentNodes.get(node).asInstanceOf[Updating]

    val nextDep = getNextDep(state.iterator)
    if (nextDep == null) {
      currentNodes.put(node, Completing(state.deps))
      completeQueue += node
    } else {
      nextDep += dep
    }
  }




  /** Boots (starts processing) of a node. */
  private def boot(node : Node[_]) : Unit = {
    val depItr = node.deps()
    val nextDep = getNextDep(depItr)
    if (nextDep == null) {
      currentNodes.put(node, Completing(new ArrayBuffer[ActiveDep]))
      completeQueue += node
    } else {
      val dep = new ActiveDep(node)
      currentNodes.put(node, Updating(dep, depItr, new ArrayBuffer[ActiveDep]))
      nextDep += dep
    }
  }



  /** Fetches a next dependency. */
  private def getNextDep(itr : NodeIterator) : ArrayBuffer[ActiveDep] = {
    while (true) {
      val item = itr()
      if (item == null)
        return null
      val buf = depBuffer(item)
      if (buf != null)
        return buf
    }
    null
  }



  /** Fetches a next dependency buffer. */
  private def depBuffer[T](node : Node[T]) : ArrayBuffer[ActiveDep] = {
    val state = currentNodes.get(node).asInstanceOf[ResolveState[T]]
    state match {
      case null ⇒ null
      case Updating(_, _, b) ⇒ b
      case Completing(b) ⇒ b
      case _ ⇒ null
    }
  }
}
