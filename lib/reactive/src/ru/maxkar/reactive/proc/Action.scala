package ru.maxkar.reactive.proc


/**
 * Composable part of the procedure. Each action could
 * be used as a part of one procedure and each procedure could
 * internally consists of many actions. However actions changing
 * some state (depenedncy lists, etc...) should be parts of one
 * procedure only.
 */
trait Action {
  /**
   * Starts a process. Next procedure's action will be started after
   * once this action completes.
   * @return action execution process.
   */
  def start() : Process
}



/** Convenience action factories. */
final object Action {
  /** No-operations action. */
  val noop : Action = new Action() {
    override def start() : Process = Process.noop
  }



  /**
   * Action which awaits one or more static processes.
   * This function do not register or update any dependencies.
   */
  def await(procedures : Procedure*) : Action =
    if (procedures.size == 0)
      noop
    else
      new Action() {
        override def start() : Process = Process.await(procedures)
      }



  /** Creates a sequential action. */
  def seq(actions : Action*) : Action =
    actions.size match {
      case 0 ⇒ noop
      case 1 ⇒ actions.head
      case _ ⇒
        new Action() {
          override def start() : Process = Process.fromActions(actions)
        }
    }



  /** Creates an atomic action without result. */
  def forUnit(block : ⇒ Unit) : Action =
    new Action() {
      override def start() : Process = {
        block
        Process.noop
      }
    }



  /**
   * Creates an action which will execute a block and await for the
   * returned procedure. This action do not update any bindings.
   */
  def forSingle(block : ⇒ Procedure) : Action =
    new Action() {
      override def start() : Process = Process.await(Seq(block))
    }



  /**
   * Creates an action which will execute a block and await for all
   * returned procedures. This action do not update any binding.
   */
  def forSeq(block : ⇒ Iterable[Procedure]) : Action =
    new Action() {
      override def start() : Process = Process.await(block)
    }




  /**
   * Binds a dependency and creates a new dynamic binding action.
   * This action will:
   * <ol>
   *   <li>Execute block to fetch a dynamic dependency.
   *   <li>Remove a dependency and register a new one if item returned
   *     on step 1 is different from previously registered binding.
   * </ol>
   */
  def dynamicBindTo(binder : DepBinder, block : ⇒ Procedure) : Action = {
    var last = block
    var reg = binder += last

    new Action() {
      override def start() : Process = {
        val np = block
        if (np != last) {
          reg.dispose()
          last = np
          reg = binder += np
        }

        Process.await(Seq(np))
      }
    }
  }
}

