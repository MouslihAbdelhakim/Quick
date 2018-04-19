package com.scalableQuality.quick.core.fileComponentDescriptions

import com.scalableQuality.quick.core.Reporting.ComparisonBetweenTwoColumns
import com.scalableQuality.quick.core.checks.{Check, CheckColumnValue}
import com.scalableQuality.quick.core.fileComponentDescriptions.errorMessages.DelimitedColumnDescriptionErrorMessages
import com.scalableQuality.quick.core.phases.{ColumnUsageStages, ShouldUseDuring, ValidationStage}
import com.scalableQuality.quick.core.valueMapping.ValueMapper
import com.scalableQuality.quick.mantle.constructFromXml._
import com.scalableQuality.quick.mantle.error.{BunchOfErrors, UnrecoverableError}

import scala.xml.MetaData

class DelimitedColumnDescription(
    metaData: ColumnDescriptionMetaData,
    position: DelimitedPosition,
    comparisonMapper: ValueMapper,
    columnValueChecks: CheckColumnValue
) {

  def shouldUseDuring(stages: ColumnUsageStages*): Boolean =
    metaData.shouldUseDuring(stages: _*)
  def columnValue(row: Vector[String]): Option[String] =
    position.extractColumnValue(row)
  def comparisonValue(row: Vector[String]): Option[String] =
    comparisonMapper(columnValue(row))
  def compareTwoColumns(
      leftRow: Option[Vector[String]],
      rightRow: Option[Vector[String]]): ComparisonBetweenTwoColumns =
    ComparisonBetweenTwoColumns(
      this.metaData,
      leftRow.flatMap(this.columnValue),
      rightRow.flatMap(this.columnValue),
      compare(leftRow, rightRow),
      checkColumnValue(leftRow),
      checkColumnValue(rightRow)
    )
  def checkColumnValue(row: Vector[String]): Boolean = {
    val value = columnValue(row)
    columnValueChecks(value)
  }

  def  usableDuringValidation: Boolean = shouldUseDuring(ValidationStage) || columnValueChecks.checksAreDefined

  private def checkColumnValue(maybeStrings: Option[Vector[String]]): Boolean =
    maybeStrings
      .map(checkColumnValue(_))
      .getOrElse(Check.noChecksWereExecutedDefaultResult)

  private def compare(leftRow: Option[Vector[String]],
                      rightRow: Option[Vector[String]]): Boolean = {
    val leftColumn = leftRow.flatMap(this.comparisonValue)
    val rightColumn = rightRow.flatMap(this.comparisonValue)
    leftColumn == rightColumn
  }
}

object DelimitedColumnDescription {
  def apply(
      metaData: ColumnDescriptionMetaData,
      position: DelimitedPosition,
      comparisonMapper: ValueMapper,
      columnValueChecks: CheckColumnValue
  ): DelimitedColumnDescription =
    new DelimitedColumnDescription(metaData,
                                   position,
                                   comparisonMapper,
                                   columnValueChecks)

  def apply(metaData: MetaData)
    : Either[UnrecoverableError, DelimitedColumnDescription] = {
    val unknownAttributeList = XMLHelperFunctions.collectUnknownAttributes(
      listOfAttributesKeys,
      metaData)
    unknownAttributeList match {
      case Nil =>
        val attributesValues = AttributesValuesExtractor(metaData, labelKey)
        val labelAttributeValue = attributesValues.get(labelKey)

        val delimitedPositionEither = DelimitedPosition(metaData)
        val shouldUseDuringEither = ShouldUseDuring(metaData)
        val comparisonMapperEither = ValueMapper(metaData)
        val checkColumnValueEither = CheckColumnValue(metaData)

        validateAttributeValues(labelAttributeValue,
                                delimitedPositionEither,
                                shouldUseDuringEither,
                                comparisonMapperEither,
                                checkColumnValueEither) match {
          case Right(
              (label,
               delimitedPosition,
               shouldUseDuring,
               comparisonMapper,
               checkColumnValue)) =>
            val metaData = ColumnDescriptionMetaData(delimitedPosition.toString,
                                                     label,
                                                     shouldUseDuring)
            Right(
              DelimitedColumnDescription(metaData,
                                         delimitedPosition,
                                         comparisonMapper,
                                         checkColumnValue))
          case Left(errorMessage) =>
            Left(errorMessage)
        }
      case _ =>
        val bunchOfErrors = BunchOfErrors(unknownAttributeList)
        DelimitedColumnDescriptionErrorMessages.invalidAttributes(bunchOfErrors)
    }
  }

  private def validateAttributeValues(
      labelAttributeEither: Either[UnrecoverableError, String],
      positionEither: Either[UnrecoverableError, DelimitedPosition],
      shouldUseDuringEither: Either[UnrecoverableError, ShouldUseDuring],
      valueMapperEither: Either[UnrecoverableError, ValueMapper],
      checkColumnValueEither: Either[UnrecoverableError, CheckColumnValue]
  ): Either[UnrecoverableError,
            (String,
             DelimitedPosition,
             ShouldUseDuring,
             ValueMapper,
             CheckColumnValue)] =
    labelAttributeEither match {

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
            DelimitedColumnDescriptionErrorMessages.invalidAttributes(
              labelAttributeValue,
              errorMessages)
        }

      case Left(labelAttributeErrorMessage) =>
        val otherErrors = UnrecoverableError.collectAllErrorsToList(
          positionEither,
          shouldUseDuringEither,
          valueMapperEither,
          checkColumnValueEither)
        DelimitedColumnDescriptionErrorMessages.invalidAttributes(
          labelAttributeErrorMessage :: otherErrors)
    }

  private val labelKey =
    AttributeValueExtractor("label", AttributeValueConversion.extractValue)
  val listOfAttributesKeys: List[AttributeValueExtractor[_]] =
    labelKey ::
      ShouldUseDuring.listOfAttributesKeys :::
      ValueMapper.listOfAttributesKeys :::
      DelimitedPosition.listOfAttributesKeys :::
      CheckColumnValue.listOfAttributesKeys
}
