package com.scalableQuality.quick.core.checks

import java.util.regex.Pattern

import com.scalableQuality.quick.core.checks.errorMessages.CheckColumnValueErrorMessages
import com.scalableQuality.quick.mantle.constructFromXml.{
  AttributeValueConversion,
  AttributeValueExtractor,
  AttributesValuesExtractor
}
import com.scalableQuality.quick.mantle.error.{
  AttributeNotFoundError,
  UnrecoverableError
}

import scala.xml.MetaData

class CheckColumnValue(
    checks: List[Check]
) {
  def apply(columnValue: Option[String]): Boolean =
    checks match {
      case Nil => Check.noChecksWereExecutedDefaultResult
      case firstPreValidationFunction :: restOfPreValidationFunctions =>
        val firstPreValidationResult = firstPreValidationFunction(columnValue)
        restOfPreValidationFunctions.foldLeft(firstPreValidationResult) {
          (previousPreValidationResult, preValidationFunction) =>
            previousPreValidationResult && preValidationFunction(columnValue)
        }
    }

  def checksAreDefined: Boolean = ! checks.isEmpty
}

object CheckColumnValue {

  def apply(
      checks: List[Check]
  ): CheckColumnValue = new CheckColumnValue(checks)

  def apply(
      metaData: MetaData): Either[UnrecoverableError, CheckColumnValue] = {
    val attributesValuesExtractor = AttributesValuesExtractor(
      metaData,
      checkColumnValueMatchRegexAttributeKey,
      checkColumnValueExistsAttributeKey)
    val checkColumnValueMatchRegexAttributeValueEither =
      attributesValuesExtractor.get(checkColumnValueMatchRegexAttributeKey)
    val checkColumnValueExistsAttributeValueEither =
      attributesValuesExtractor.get(checkColumnValueExistsAttributeKey)
    val classParameters = validateCheckColumnValueAttributes(
      checkColumnValueMatchRegexAttributeValueEither,
      checkColumnValueExistsAttributeValueEither
    )
    classParameters match {
      case Right((patternOpt, shouldCheckExistence)) =>
        val emptyCheckList = Nil
        val checkListProbablyWithMatchRegex =
          probablyIncludeColumnValueMatchRegex(patternOpt, emptyCheckList)
        val checkListProbablyWithCheckExists =
          ProbablyIncludeCheckColumnValueExists(shouldCheckExistence,
                                                checkListProbablyWithMatchRegex)
        Right(CheckColumnValue(checkListProbablyWithCheckExists))
      case Left(errors) =>
        CheckColumnValueErrorMessages.invalidChecksAttributesValues(errors)
    }
  }

  private def ProbablyIncludeCheckColumnValueExists(
      shouldCheckExistence: Boolean,
      checkList: List[Check]): List[Check] =
    includeInListIfItShould(CheckColumnValueExists,
                            shouldCheckExistence,
                            checkList)

  private def probablyIncludeColumnValueMatchRegex(
      patternOpt: Option[Pattern],
      checkList: List[Check]
  ): List[Check] = {
    val checkColumnValueMatchRegex =
      patternOpt.map(CheckColumnValueMatchRegex(_))
    includeInListIfItShould(checkColumnValueMatchRegex.get,
                            checkColumnValueMatchRegex.isDefined,
                            checkList)
  }

  private def includeInListIfItShould(getcheck: => Check,
                                      shouldInclude: Boolean,
                                      checkList: List[Check]): List[Check] =
    if (shouldInclude)
      getcheck :: checkList
    else
      checkList

  private def validateCheckColumnValueAttributes(
      checkColumnValueMatchRegexAttribute: Either[UnrecoverableError, Pattern],
      checkColumnValueExistsAttribute: Either[UnrecoverableError, Boolean]
  ): Either[List[UnrecoverableError], (Option[Pattern], Boolean)] =
    (checkColumnValueMatchRegexAttribute, checkColumnValueExistsAttribute) match {
      case (Right(pattern), Right(boolean)) =>
        Right((Some(pattern), boolean))
      case (Left(_: AttributeNotFoundError), Right(boolean)) =>
        Right((None, boolean))
      case _ =>
        UnrecoverableError.collectAllErrors(checkColumnValueMatchRegexAttribute,
                                            checkColumnValueExistsAttribute)
    }

  private val checkColumnValueMatchRegexAttributeKey =
    AttributeValueExtractor("checkColumnValueMatches",
                            AttributeValueConversion.toPattern)

  private val defaultCheckColumnValueExistsAttributeValue = Right(true)
  private val checkColumnValueExistsAttributeKey = AttributeValueExtractor(
    "checkColumnValueExists",
    AttributeValueConversion.toBoolean,
    defaultCheckColumnValueExistsAttributeValue)
  val listOfAttributesKeys = List(checkColumnValueMatchRegexAttributeKey,
                                  checkColumnValueExistsAttributeKey)
}
