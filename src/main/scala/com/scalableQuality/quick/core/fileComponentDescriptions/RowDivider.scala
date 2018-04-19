package com.scalableQuality.quick.core.fileComponentDescriptions

import com.scalableQuality.quick.core.Reporting.ComparisonBetweenTwoColumns
import com.scalableQuality.quick.core.phases.ColumnUsageStages
import com.scalableQuality.quick.mantle.parsing.RawRow

trait RowDivider {
  def keepOnlyColumnsDescriptionsUsedIn(
      columnUsages: ColumnUsageStages*
  ): RowDivider

  def compare(
      leftFileRawRow: Option[RawRow],
      rightFileRawRow: Option[RawRow]
  ): List[ComparisonBetweenTwoColumns]

  def isMatchable: Boolean

  def columnsComparisonValuesFor(stage: ColumnUsageStages,
                                 rawRow: RawRow): List[Option[String]]

  def executeCheckOn(row: RawRow): Boolean

  def usableDuringValidation: Boolean
}
