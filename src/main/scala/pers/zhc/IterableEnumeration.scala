package pers.zhc

import java.util

/**
  * @author bczhc
  */
class IterableEnumeration[T](private val e: util.Enumeration[T])
    extends Iterator[T] {
  override def hasNext: Boolean = e.hasMoreElements

  override def next(): T = e.nextElement()
}
