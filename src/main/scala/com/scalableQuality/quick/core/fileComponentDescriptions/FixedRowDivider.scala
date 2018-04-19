package com.scalableQuality.quick.core.fileComponentDescriptions

import com.scalableQuality.quick.core.Reporting.ComparisonBetweenTwoColumns
import com.scalableQuality.quick.core.checks.Check
import com.scalableQuality.quick.core.phases.{ColumnUsageStages, MatchingStage}
import com.scalableQuality.quick.mantle.parsing.RawRow

class FixedRowDivider(
    private[FixedRowDivider] val columnsDescriptions: List[
      FixedColumnDescription]
) extends RowDivider {

  override def keepOnlyColumnsDescriptionsUsedIn(
      columnUsages: ColumnUsageStages*
  ): FixedRowDivider = {
    val keptColumnDescriptions = for {
      colDesc <- columnsDescriptions
      if colDesc.shouldUseDuring(columnUsages: _*)
    } yield colDesc
    FixedRowDivider(keptColumnDescriptions)
  }

  override def compare(
      leftFileRawRow: Option[RawRow],
      rightFileRawRow: Option[RawRow]
  ): List[ComparisonBetweenTwoColumns] =
    for {
      colDesc <- columnsDescriptions
    } yield colDesc.compareTwoColumns(leftFileRawRow, rightFileRawRow)

  override def isMatchable: Boolean =
    columnsDescriptions
      .collectFirst {
        case col if col.shouldUseDuring(MatchingStage) => true
      }
      .getOrElse(false)

  override def columnsComparisonValuesFor(
      stage: ColumnUsageStages,
      rawRow: RawRow): List[Option[String]] =
    for {
      colDesc <- columnsDescriptions
      if colDesc.shouldUseDuring(stage)
    } yield colDesc.comparisonValue(rawRow)

  override def executeCheckOn(row: RawRow): Boolean = {
    def getCheckResult(results: List[Boolean]): Boolean = {
      val rowDefaultCheckResult = Check.noChecksWereExecutedDefaultResult
      results.foldLeft(rowDefaultCheckResult)(_ && _)
    }
    val allChecksResults = for {
      colDesc <- columnsDescriptions
    } yield colDesc.checkColumnValue(row)
    getCheckResult(allChecksResults)
  }

  override def usableDuringValidation: Boolean = if(columnsDescriptions.isEmpty){
    false
  } else {
    columnsDescriptions.map(_.usableDuringValidation).reduceLeft(_ || _)
  }

  override def equals(obj: scala.Any): Boolean = obj match {
    case rowDivider: FixedRowDivider =>
      rowDivider.columnsDescriptions == this.columnsDescriptions
    case _ => false
  }

}

object FixedRowDivider {

  def apply(
      columnsDescriptions: List[FixedColumnDescription]
  ): FixedRowDivider = new FixedRowDivider(columnsDescriptions)

}
