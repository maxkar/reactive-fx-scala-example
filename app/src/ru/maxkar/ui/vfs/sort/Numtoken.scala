package ru.maxkar.ui.vfs.sort

import scala.collection.mutable.ArrayBuffer
import java.math.BigInteger

/** Numeric token in the file name. */
abstract sealed class Numtoken


/** Simple string (non-number) value. */
case class NumtokenStr(str : String) extends Numtoken

/** String of digits, numeric part of the name. */
case class NumtokenNum(str : String) extends Numtoken



/** Token companion. */
object Numtoken {
  /** Parses a string into a sequence of tokens. */
  def parse(input : String) : Seq[Numtoken] = {
    val res = new ArrayBuffer[Numtoken]
    var ptr = 0
    while (ptr < input.length) {
      if (Character.isDigit(input.charAt(ptr))) {
        val numStart = ptr
        do {
          ptr += 1
        } while (ptr < input.length && Character.isDigit(input.charAt(ptr)))

        res += NumtokenNum(input.substring(numStart, ptr))
      }

      if (ptr < input.length) {
        val strStart = ptr
        do {
          ptr += 1
        } while (ptr < input.length && !Character.isDigit(input.charAt(ptr)))

        res += NumtokenStr(input.substring(strStart, ptr))
      }
    }

    res
  }



  /** Compares two strings as number strings. */
  private def numOrder(a : String, b : String) : Int =
    if (a.length < b.length)
      -1
    else if (a.length > b.length)
      1
    else
      a.compareTo(b)



  /** Token comparator. */
  private def tokCompare(
        strOrder : (String, String) ⇒ Int,
        tok1 : Numtoken, tok2 : Numtoken)
      : Int =
    (tok1, tok2) match {
      case (NumtokenStr(a), NumtokenStr(b)) ⇒ strOrder(a, b)
      case (NumtokenNum(a), NumtokenNum(b)) ⇒ numOrder(a, b)
      case (NumtokenNum(_), _) ⇒ -1
      case (_, NumtokenNum(_)) ⇒ 1
    }




  /** Creates a numtoken comparator backed by the string order. */
  def compare(
        strOrder : (String, String) ⇒ Int,
        tok1 : Seq[Numtoken], tok2 : Seq[Numtoken])
      : Int = {
    val i1 = tok1.iterator
    val i2 = tok2.iterator

    while (i1.hasNext && i2.hasNext) {
      val tmp = tokCompare(strOrder, i1.next, i2.next)
      if (tmp != 0)
        return tmp
    }

    if (i1.hasNext)
      1
    else if (i2.hasNext)
      -1
    else
      0
  }
}
