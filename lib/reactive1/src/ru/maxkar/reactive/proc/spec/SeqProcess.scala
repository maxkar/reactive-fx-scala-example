package ru.maxkar.reactive.proc.spec

import ru.maxkar.reactive.proc._


/**
 * Sequential processor for actions.
 * @param items items to use.
 */
private[spec] final class SeqProcess(items : Seq[Action])
    extends Process {


  /** Iterator over items. */
  private val itr = items.iterator


  /** Peer process under the hood. */
  private var peerProcess : Process = itr.next().start()


  override def proceedTillNextProcedure() : Procedure = {
    var proc = peerProcess.proceedTillNextProcedure()
    while (proc == null) {
      if (!itr.hasNext)
        return null

      peerProcess = itr.next.start()
      proc = peerProcess.proceedTillNextProcedure()
    }

    proc
  }
}
