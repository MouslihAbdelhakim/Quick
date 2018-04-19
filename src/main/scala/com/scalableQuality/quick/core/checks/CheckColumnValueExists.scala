package com.scalableQuality.quick.core.checks

case object CheckColumnValueExists extends Check {
  override protected def executeCheck(value: String): Boolean = true
}
