package com.scalableQuality.quick.core.fileComponentDescriptions

import com.scalableQuality.quick.mantle.parsing.RawRow
import java.security.MessageDigest

import com.scalableQuality.quick.core.Reporting.ComparisonBetweenTwoColumns
import com.scalableQuality.quick.core.phases.{
  ColumnUsageStages,
  MatchingStage,
  ValidationStage
}

import scala.annotation.tailrec

class OrderedRowDescription(
    private[OrderedRowDescription] val rowDivider: RowDivider,
    val label: String
) {

  def keepOnlyColumnsDescriptionsUsedIn(
      columnUsages: ColumnUsageStages*
  ): OrderedRowDescription = {
    val nextRowDivider =
      this.rowDivider.keepOnlyColumnsDescriptionsUsedIn(columnUsages: _*)
    OrderedRowDescription(nextRowDivider, this.label)
  }

  def compare(
      leftFileRawRow: Option[RawRow],
      rightFileRawRow: Option[RawRow]
  ): List[ComparisonBetweenTwoColumns] =
    rowDivider.compare(leftFileRawRow, rightFileRawRow)

  // validationSignatureOf and matchingSignatureOf functions return
  // List[Byte] instead of List[Option[String]] to avoid huge ram usage
  def validationSignatureOf(
      row: RawRow
  ): List[Option[List[Byte]]] = {
    val validationValue = validationValuesOf(row)
    getSignature(validationValue)
  }

  def matchingSignatureOf(
      row: RawRow
  ): List[Option[List[Byte]]] = {
    val matchingValue = matchingValuesOf(row)
    getSignature(matchingValue)
  }

  def isMatchable: Boolean = rowDivider.isMatchable
  def isValidatable: Boolean = rowDivider.usableDuringValidation

  private def validationValuesOf(
      row: RawRow
  ): List[Option[String]] =
    rowDivider.columnsComparisonValuesFor(ValidationStage, row)

  private def matchingValuesOf(
      row: RawRow
  ): List[Option[String]] =
    rowDivider.columnsComparisonValuesFor(MatchingStage, row)

  // to avoid considering a row with absent columns values equivalent
  // to a row with empty columns the getSignature function will
  // return a list consisting of Some[List[Byte]] to represent the
  // resulted hash of concatenated successive column values
  // and None to represented the absent column

  // the getSignature function will append the column value's length at to it
  // to avoid generating the same hash for the two List[Option[String]]
  // below List(Some("AAA"),Some("AA")) and List(Some("AA"),Some("AAA"))

  private[fileComponentDescriptions] def getSignature(
      columns: List[Option[String]]
  ): List[Option[List[Byte]]] = {
    def sha1Sum(bytes: Array[Byte]): List[Byte] = {
      val sha1Digest = MessageDigest.getInstance("SHA1")
      sha1Digest.reset()
      sha1Digest.update(bytes)
      sha1Digest.digest().toList
    }

    def appendColumnValue(
        stringBuilderOption: Option[StringBuilder],
        columnValue: String
    ): Option[StringBuilder] = stringBuilderOption match {
      case None =>
        val newStringBuilder = new StringBuilder(columnValue)
        newStringBuilder append columnValue.length.toString
        Some(newStringBuilder)
      case Some(stringBuilder) =>
        stringBuilder append columnValue
        stringBuilder append columnValue.length.toString
        Some(stringBuilder)
    }
    @tailrec def loop(
        columns: List[Option[String]],
        concatenatedPreviousColumnValuesOpt: Option[StringBuilder],
        digestedBytes: List[Option[List[Byte]]]
    ): List[Option[List[Byte]]] = columns match {
      case Nil =>
        concatenatedPreviousColumnValuesOpt match {
          case None =>
            digestedBytes.reverse
          case Some(columnValue) =>
            val signatureOfPreviousColumnValues = sha1Sum(
              columnValue.toString.getBytes)
            val finalDigestedBytes = Some(signatureOfPreviousColumnValues)
            (finalDigestedBytes :: digestedBytes).reverse
        }
      case Some(columnValue) :: restOfColumnValues =>
        val concatenatedStrings: Option[StringBuilder] =
          appendColumnValue(concatenatedPreviousColumnValuesOpt, columnValue)
        loop(restOfColumnValues, concatenatedStrings, digestedBytes)
      case None :: restOfColumnValues =>
        concatenatedPreviousColumnValuesOpt match {
          case None =>
            loop(restOfColumnValues,
                 concatenatedPreviousColumnValuesOpt,
                 None :: digestedBytes)
          case Some(concatenatedPreviousColumnValues) =>
            val signatureOfPreviousColumnValues = sha1Sum(
              concatenatedPreviousColumnValues.toString.getBytes)
            loop(restOfColumnValues,
                 None,
                 None :: Some(signatureOfPreviousColumnValues) :: digestedBytes)
        }
    }
    loop(columns, None, Nil)
  }

  def check(rawRow: RawRow): Boolean = rowDivider.executeCheckOn(rawRow)

}

object OrderedRowDescription {
  def apply(
      rowDivider: RowDivider,
      label: String
  ): OrderedRowDescription = new OrderedRowDescription(rowDivider, label)
}
