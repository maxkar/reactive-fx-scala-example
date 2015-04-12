package ru.maxkar.reactive.value

import ru.maxkar.reactive.deps.Binder
import ru.maxkar.reactive.proc.Procedure
import ru.maxkar.reactive.proc.{Specification ⇒ Specs}


/** Implementation of shallow submodel dispatch.  */
private[value] final class ShallowSubmodelDispatch[S, R](
      binder : Binder, fn : (S, BindContext) ⇒ R, base : Behaviour[S])
    extends Behaviour[R] {

  /** Sub-binder and its destructor. */
  private var (subBinder, subDtor) = binder.sub()

  /** Dependent model. */
  private var v = fn(base.value(), new BindContext(subBinder))

  /** Change flag. */
  private var changed = false



  /** Update procedure. */
  private val proc =
    Procedure.compile(
      Specs.seq(
        Specs.await(base.change.procedure),
        Specs.forUnit { reevalAfterBase }
      ),
      binder,
      () ⇒ changed = false)



  /** Updates after base is ready. */
  private def reevalAfterBase() : Unit = {
    if (!base.change.value())
      return


    subDtor.dispose()
    val (nsb, nsd) = binder.sub()
    subBinder = nsb
    subDtor = nsd

    val nv = fn(base.value(), new BindContext(subBinder))

    if (nv != v) {
      changed = true
      v = nv
    }
  }



  /* IMPLEMENTATION */
  override def value() = v
  override val change = Signal(proc, changed)
}

