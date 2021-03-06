package ru.maxkar.async

import ru.maxkar.async._

import ru.maxkar.fun.syntax._
import ru.maxkar.reactive.value._

import java.util.concurrent._

import Runnables._

/**
 * Asynchronous task executor. Pefroms all the operations in the
 * dedicated thread and pushes operation progress back to the
 * platform thread.
 * @param runPlatf output to the platform thread (EDT, FX thread, etc...)
 */
final class AsyncExecutor(runPlatf : Runnable ⇒ Unit) extends Promising {
  /** Operation executor. */
  private val executor = Executors.newSingleThreadExecutor()


  /** Shuts this executor down. Returns an operation promise. */
  def shutdown() : Promise[Unit] = {
    val res = variable[PromiseState[Unit]](InProgress)

    new Thread(run {
      executor.shutdown()
      executor.awaitTermination(10, TimeUnit.DAYS)
      runPlatf(run {
        res.set(Success())
      })
    }).start()

    res
  }



  /**
   * Executes an operation on the executor thread.
   * Moves all state notifications back to the application thread.
   */
  override def apply[T](op : ⇒ T) : Promise[T] = {
    val res = variable[PromiseState[T]](InProgress)

    def fail(t : Throwable) : Unit = {
      if (res.value != InProgress)
        return
      res.set(Failure(t))
    }

    try {
      executor.submit(run {
        try {
          val v = Success(op)
          runPlatf(run {
            res.set(v)
          })
        } catch {
          case t : Throwable ⇒ runPlatf(run { fail(t) })
        }
      })
    } catch {
      case t : Throwable ⇒ fail(t)
    }

    res
  }
}
