package com.scalableQuality.quick.core.fileComponentDescriptions

import com.scalableQuality.quick.core.Reporting.ComparisonBetweenTwoColumns
import com.scalableQuality.quick.core.checks.Check
import com.scalableQuality.quick.core.phases.{ColumnUsageStages, MatchingStage}
import com.scalableQuality.quick.mantle.parsing.{LiteralDelimiter, RawRow}

class DelimitedRowDivider(
    private val columnsDescriptions: List[DelimitedColumnDescription],
    private val delimiter: LiteralDelimiter
) extends RowDivider {
  override def keepOnlyColumnsDescriptionsUsedIn(
      columnUsages: ColumnUsageStages*
  ): DelimitedRowDivider = {
    val keptColumnDescriptions = for {
      colDesc <- columnsDescriptions
      if colDesc.shouldUseDuring(columnUsages: _*)
    } yield colDesc
    DelimitedRowDivider(
      keptColumnDescriptions,
      delimiter
    )
  }

  override def compare(
      leftFileRawRow: Option[RawRow],
      rightFileRawRow: Option[RawRow]
  ): List[ComparisonBetweenTwoColumns] = {
    val splitLeftRow = leftFileRawRow.map(this.delimiter.splitRow)
    val splitRightRow = rightFileRawRow.map(this.delimiter.splitRow)
    for {
      colDesc <- columnsDescriptions
    } yield colDesc.compareTwoColumns(splitLeftRow, splitRightRow)
  }

  override def isMatchable: Boolean =
    columnsDescriptions
      .collectFirst {
        case col if col.shouldUseDuring(MatchingStage) => true
      }
      .getOrElse(false)

  override def columnsComparisonValuesFor(
      stage: ColumnUsageStages,
      rawRow: RawRow): List[Option[String]] = {
    val splittedRow = this.delimiter.splitRow(rawRow)
    for {
      colDesc <- columnsDescriptions
      if colDesc.shouldUseDuring(stage)
    } yield colDesc.comparisonValue(splittedRow)
  }

  override def executeCheckOn(row: RawRow): Boolean = {
    def getCheckResult(results: List[Boolean]): Boolean = {
      val rowDefaultCheckResult = Check.noChecksWereExecutedDefaultResult
      results.foldLeft(rowDefaultCheckResult)(_ && _)
    }
    val splitRow = delimiter.splitRow(row)
    val allChecksResults = for {
      colDesc <- columnsDescriptions
    } yield colDesc.checkColumnValue(splitRow)
    getCheckResult(allChecksResults)
  }

  override def usableDuringValidation: Boolean = if(columnsDescriptions.isEmpty){
    false
  } else {
    columnsDescriptions.map(_.usableDuringValidation).reduceLeft(_ || _)
  }

  override def equals(obj: scala.Any): Boolean = obj match {
    case rowDivider: DelimitedRowDivider =>
      rowDivider.columnsDescriptions == this.columnsDescriptions &&
      rowDivider.delimiter == this.delimiter
    case _ => false
  }
}

object DelimitedRowDivider {
  // Pattern.quote() escapes special regex chars
  def apply(
      columnsDescriptions: List[DelimitedColumnDescription],
      delimiter: LiteralDelimiter
  ): DelimitedRowDivider = new DelimitedRowDivider(
    columnsDescriptions,
    delimiter
  )
}
