package com.scalableQuality.quick.core.fileComponentDescriptions

import com.scalableQuality.quick.core.Reporting.ComparisonBetweenTwoColumns
import com.scalableQuality.quick.core.checks.{Check, CheckColumnValue}
import com.scalableQuality.quick.core.fileComponentDescriptions.errorMessages.FixedColumnDescriptionErrorMessages
import com.scalableQuality.quick.core.phases.{ColumnUsageStages, ShouldUseDuring, ValidationStage}
import com.scalableQuality.quick.core.valueMapping.ValueMapper
import com.scalableQuality.quick.mantle.constructFromXml.{AttributeValueConversion, AttributeValueExtractor, AttributesValuesExtractor, XMLHelperFunctions}
import com.scalableQuality.quick.mantle.error.{BunchOfErrors, UnrecoverableError}
import com.scalableQuality.quick.mantle.parsing.RawRow

import scala.xml.MetaData

class FixedColumnDescription(
    val metaData: ColumnDescriptionMetaData,
    val position: FixedPosition,
    comparisonMapper: ValueMapper,
    columnValueChecks: CheckColumnValue
) {
  def shouldUseDuring(stages: ColumnUsageStages*): Boolean =
    metaData.shouldUseDuring(stages: _*)
  def columnValue(row: RawRow): Option[String] =
    position.extractColumnValue(row)
  def comparisonValue(row: RawRow): Option[String] =
    comparisonMapper(columnValue(row))
  def compareTwoColumns(leftRow: Option[RawRow],
                        rightRow: Option[RawRow]): ComparisonBetweenTwoColumns =
    ComparisonBetweenTwoColumns(
      this.metaData,
      leftRow.flatMap(this.columnValue),
      rightRow.flatMap(this.columnValue),
      compare(leftRow, rightRow),
      checkColumnValue(leftRow),
      checkColumnValue(rightRow)
    )

  def checkColumnValue(row: RawRow): Boolean = {
    val value = columnValue(row)
    columnValueChecks(value)
  }

  def usableDuringValidation: Boolean = shouldUseDuring(ValidationStage) || columnValueChecks.checksAreDefined

  private def checkColumnValue(maybeRow: Option[RawRow]): Boolean =
    maybeRow
      .map(checkColumnValue(_))
      .getOrElse(Check.noChecksWereExecutedDefaultResult)


  private def compare(leftRow: Option[RawRow],
                      rightRow: Option[RawRow]): Boolean = {
    val leftColumn = leftRow.flatMap(this.comparisonValue)
    val rightColumn = rightRow.flatMap(this.comparisonValue)
    leftColumn == rightColumn
  }

}

object FixedColumnDescription {
  def apply(
      metaData: ColumnDescriptionMetaData,
      position: FixedPosition,
      comparisonMapper: ValueMapper,
      columnValueChecks: CheckColumnValue
  ): FixedColumnDescription =
    new FixedColumnDescription(metaData,
                               position,
                               comparisonMapper,
                               columnValueChecks)

  def apply(
      position: FixedPosition,
      shouldUseDuring: ShouldUseDuring,
      comparisonMapper: ValueMapper,
      label: String,
      columnValueChecks: CheckColumnValue
  ): FixedColumnDescription = {
    val metaData =
      ColumnDescriptionMetaData(position.toString, label, shouldUseDuring)
    FixedColumnDescription(metaData,
                           position,
                           comparisonMapper,
                           columnValueChecks)
  }

  def apply(metaData: MetaData)
    : Either[UnrecoverableError, FixedColumnDescription] = {
    val unknownAttributeList = XMLHelperFunctions.collectUnknownAttributes(
      FixedColumnDescription.listOfAttributesKeys,
      metaData)
    unknownAttributeList match {
      case Nil =>
        val attributesValues = AttributesValuesExtractor(metaData, labelKey)
        val labelAttributeValue = attributesValues.get(labelKey)

        val fixedPositionEither = FixedPosition(metaData)
        val shouldUseDuringEither = ShouldUseDuring(metaData)
        val comparisonMapperEither = ValueMapper(metaData)
        val checkColumnValueEither = CheckColumnValue(metaData)

        validateAttributeValues(labelAttributeValue,
                                fixedPositionEither,
                                shouldUseDuringEither,
                                comparisonMapperEither,
                                checkColumnValueEither) match {
          case Right(
              (label,
               fixedPosition,
               shouldUseDuring,
               comparisonMapper,
               checkColumnValue)) =>
            val fixedColumnDescription = FixedColumnDescription(
              fixedPosition,
              shouldUseDuring,
              comparisonMapper,
              label,
              checkColumnValue
            )
            Right(fixedColumnDescription)
          case Left(errorMessage) =>
            Left(errorMessage)
        }
      case _ =>
        val bunchOfErrors = BunchOfErrors(unknownAttributeList)
        FixedColumnDescriptionErrorMessages.invalidAttributes(bunchOfErrors)
    }

  }
  private def validateAttributeValues(
      labelAttributeValueEither: Either[UnrecoverableError, String],
      positionEither: Either[UnrecoverableError, FixedPosition],
      shouldUseDuringEither: Either[UnrecoverableError, ShouldUseDuring],
      valueMapperEither: Either[UnrecoverableError, ValueMapper],
      checkColumnValueEither: Either[UnrecoverableError, CheckColumnValue]
  ): Either[
    UnrecoverableError,
    (String, FixedPosition, ShouldUseDuring, ValueMapper, CheckColumnValue)] =
    labelAttributeValueEither match {

      case Right(labelAttributeValue) =>
        (positionEither,
         shouldUseDuringEither,
         valueMapperEither,
         checkColumnValueEither) match {
          case (Right(position),
                Right(shouldUseDuring),
                Right(valueMapper),
                Right(checkColumnValue)) =>
            val classParameters =
              (labelAttributeValue,
               position,
               shouldUseDuring,
               valueMapper,
               checkColumnValue)
            Right(classParameters)
          case _ =>
            val errorMessages = UnrecoverableError.collectAllErrorsToList(
              positionEither,
              shouldUseDuringEither,
              valueMapperEither,
              checkColumnValueEither)
            FixedColumnDescriptionErrorMessages.invalidAttributes(
              labelAttributeValue,
              errorMessages)
        }

      case Left(labelAttributeErrorMessage) =>
        val otherErrors = UnrecoverableError.collectAllErrorsToList(
          positionEither,
          shouldUseDuringEither,
          valueMapperEither,
          checkColumnValueEither)
        FixedColumnDescriptionErrorMessages.invalidAttributes(
          labelAttributeErrorMessage :: otherErrors)
    }

  private val labelKey =
    AttributeValueExtractor("label", AttributeValueConversion.extractValue)
  val listOfAttributesKeys =
    labelKey ::
      ShouldUseDuring.listOfAttributesKeys :::
      ValueMapper.listOfAttributesKeys :::
      FixedPosition.listOfAttributesKeys :::
      CheckColumnValue.listOfAttributesKeys

}
