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
