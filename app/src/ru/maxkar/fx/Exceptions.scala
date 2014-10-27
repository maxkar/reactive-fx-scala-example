package ru.maxkar.fx

import java.io._

/** Exception utils. */
object Exceptions {
  /** Converts exception to string. */
  def fullException(exn : Throwable) : String = {
    val sw = new StringWriter()
    val pw = new PrintWriter(sw)
    exn.printStackTrace(pw)
    return sw.toString()
  }
}
