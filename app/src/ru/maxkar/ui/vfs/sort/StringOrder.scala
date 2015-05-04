package ru.maxkar.ui.vfs.sort

/** Order for strings. */
object StringOrder {
  /** "Normal" string order comparator. */
  def normalOrder(str1 : String, str2 : String) : Int =
    str1.compareTo(str2)


  /** Case-insensitive scala order. */
  def caseInsensitiveOrder(str1 : String, str2 : String) : Int =
    str1.compareToIgnoreCase(str2)
}
