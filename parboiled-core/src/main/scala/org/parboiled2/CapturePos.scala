package org.parboiled2

class CapturePos(val pos: Long) extends AnyVal {
  def start = (pos & 0x00000000FFFFFFFFL).toInt
  def end = (pos >>> 32).toInt
}

object CapturePos {
  def apply(start: Int, end: Int) = new CapturePos((end.toLong << 32) + start)
}
