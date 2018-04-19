package com.scalableQuality.quick.core.checks

import java.util.regex.Pattern

class CheckColumnValueMatchRegex(
    private val pattern: Pattern
) extends Check {
  override protected def executeCheck(value: String): Boolean =
    pattern.matcher(value).matches()

}

object CheckColumnValueMatchRegex {
  def apply(
      pattern: Pattern
  ): CheckColumnValueMatchRegex = new CheckColumnValueMatchRegex(pattern)
}
