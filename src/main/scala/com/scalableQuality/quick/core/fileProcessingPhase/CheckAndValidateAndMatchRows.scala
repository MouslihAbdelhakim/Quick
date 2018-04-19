package com.scalableQuality.quick.core.fileProcessingPhase

import com.scalableQuality.quick.core.Reporting.{EmptyValidationAndMatchingReport, FilledValidationAndMatchingReport, ValidationAndMatchingReport}
import com.scalableQuality.quick.core.fileComponentDescriptions.OrderedRowDescription
import com.scalableQuality.quick.mantle.parsing.RawRow
class CheckAndValidateAndMatchRows(
    orderedRowDescription: OrderedRowDescription,
    leftFileRows: List[RawRow],
    rightFileRows: List[RawRow],
    leftFileLabel: Option[String],
    rightFileLabel: Option[String]
) extends RowsProcessingPhase {
  override def execute(): ValidationAndMatchingReport = if (orderedRowDescription.isValidatable) {
    val (leftFileRowsPassedChecks, leftFileRowsFailedChecks) =
      CheckingProcess(orderedRowDescription, leftFileRows)
    val (rightFileRowsPassedChecks, rightFileRowsFailedChecks) =
      CheckingProcess(orderedRowDescription, rightFileRows)

    val (invalidLeftFileRows, invalidRightFileRows) =
      ValidationProcess(orderedRowDescription,
                        leftFileRowsPassedChecks,
                        rightFileRowsPassedChecks)

    val leftFileRowsToBeMatched = invalidLeftFileRows ::: leftFileRowsFailedChecks
    val rightFileRowsToBeMatched = invalidRightFileRows ::: rightFileRowsFailedChecks
    val matchedRows = MatchingProcess(orderedRowDescription,
                                      leftFileRowsToBeMatched,
                                      rightFileRowsToBeMatched)
    ValidationAndMatchingReport(
      orderedRowDescription,
      leftFileLabel,
      rightFileLabel,
      matchedRows
    )
  } else {
    EmptyValidationAndMatchingReport
  }
}
object CheckAndValidateAndMatchRows {
  def apply(
      orderedRowDescription: OrderedRowDescription,
      leftFileRows: List[RawRow],
      rightFileRows: List[RawRow],
      leftFileLabel: Option[String],
      rightFileLabel: Option[String]
  ): CheckAndValidateAndMatchRows = new CheckAndValidateAndMatchRows(
    orderedRowDescription,
    leftFileRows,
    rightFileRows,
    leftFileLabel,
    rightFileLabel
  )
}
