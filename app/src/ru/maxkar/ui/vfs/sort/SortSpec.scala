package ru.maxkar.ui.vfs.sort

import ru.maxkar.ui.vfs.FileInfo
import ru.maxkar.ui.vfs.FileType

/** Sort order specification. */
abstract sealed class SortSpec {
  import SortSpec._

  def compare(a : FileInfo, b : FileInfo) : Int = {
    val orddif = typeOrd(a.fileType) - typeOrd(b.fileType)
    if (orddif != 0)
      orddif
    else
      compareEx(a, b)
  }

  protected def compareEx(a : FileInfo, b : FileInfo) : Int
}

case object SortNameCS extends SortSpec {
  override def toString() : String = "By name, case-sensinite"
  override protected def compareEx(a : FileInfo, b : FileInfo) : Int =
    StringOrder.normalOrder(a.name, b.name)
}

case object SortNameCI extends SortSpec {
  override def toString() : String = "By name, case-insensitive"
  override protected def compareEx(a : FileInfo, b : FileInfo) : Int =
    StringOrder.caseInsensitiveOrder(a.name, b.name)
}

case object SortNumCS extends SortSpec {
  override def toString() : String = "By number, case-sensitive"
  override protected def compareEx(a : FileInfo, b : FileInfo) : Int =
    Numtoken.compare(StringOrder.normalOrder, a.nsvector, b.nsvector)
}

case object SortNumCI extends SortSpec {
  override def toString() : String = "By number, case-insensitive"
  override protected def compareEx(a : FileInfo, b : FileInfo) : Int =
    Numtoken.compare(StringOrder.caseInsensitiveOrder, a.nsvector, b.nsvector)
}



/** Sort specification utilities. */
object SortSpec {
  /** Provides relative type ordering. */
  private def typeOrd(t : FileType) : Int =
    t match {
      case FileType.ParentDirectory ⇒ 1
      case FileType.Directory ⇒ 2
      case _ ⇒ 3
    }

  /** All sorting items. */
  val allItems : Seq[SortSpec] = Seq(
    SortNameCS, SortNumCS, SortNameCI, SortNumCI)
}
