package com.scalableQuality.quick.surface.output

import com.scalableQuality.quick.core.Reporting.{
  FilledValidationAndMatchingReport,
  ValidationAndMatchingReport
}
import com.scalableQuality.quick.surface.{
  ExitStatus,
  FilesAreDifferent,
  FilesAreIdentical,
  InterruptedByAnError
}

object ExitWithStatus {
  def exitWithReport(
      filledValidationReports: List[FilledValidationAndMatchingReport]): Unit =
    filledValidationReports match {
      case Nil =>
        ExitWithStatus(FilesAreIdentical)
      case _ =>
        ExitWithStatus(FilesAreDifferent)
    }

  def interruptedByAnError(): Unit = ExitWithStatus(InterruptedByAnError)

  private def apply(exitStatus: ExitStatus): Unit =
    System.exit(exitStatus.value)
}
