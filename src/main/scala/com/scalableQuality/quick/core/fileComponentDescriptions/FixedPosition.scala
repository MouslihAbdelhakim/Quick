package com.scalableQuality.quick.core.fileComponentDescriptions

import com.scalableQuality.quick.core.fileComponentDescriptions.errorMessages.FixedLengthPositionErrorMessages
import com.scalableQuality.quick.mantle.parsing.RawRow
import com.scalableQuality.quick.mantle.constructFromXml._
import com.scalableQuality.quick.mantle.error.{
  AttributeNotFoundError,
  UnrecoverableError
}

import scala.xml.MetaData

class FixedPosition(
    startsAt: Int,
    endsAt: Int,
    override val toString: String
) {
  def extractColumnValue(row: RawRow): Option[String] =
    try {
      if(startsAt < row.value.length && endsAt > row.value.length)
        Some(row.value.substring(startsAt))
      else
        Some(row.value.substring(startsAt.toInt, endsAt))
    } catch {
      case e: Exception => None
    }

}

object FixedPosition {

  private def apply(startsAt: Int, endsAt: Int): FixedPosition = {
    val actualStartsAt = startsAt - 1
    val toString = "%4d-%-4d".format(startsAt, endsAt)
    new FixedPosition(actualStartsAt, endsAt, toString)
  }

  def apply(
      elemAttributes: MetaData): Either[UnrecoverableError, FixedPosition] = {

    val attributeValues =
      AttributesValuesExtractor(elemAttributes, listOfAttributesKeys)

    val startsAtParameterValue = attributeValues.get(startsAtKey)
    val endsAtParameterValue = attributeValues.get(endsAtKey)
    val lengthParameterValue = attributeValues.get(lengthKey)

    validateAttributeValues(startsAtParameterValue,
                            endsAtParameterValue,
                            lengthParameterValue) match {
      case Right((starsAtValue, endsAtValue, lengthValue)) =>
        FixedPosition(starsAtValue, endsAtValue, lengthValue)
      case Left(errorMessages) =>
        FixedLengthPositionErrorMessages.invalidAttributes(errorMessages)
    }
  }

  def validateAttributeValues(
      startsAtAttributeValues: Either[UnrecoverableError, Int],
      endsAtAtAttributeValues: Either[UnrecoverableError, Int],
      lengthAttributeValues: Either[UnrecoverableError, Int]
  ): Either[List[UnrecoverableError], (Int, Option[Int], Option[Int])] =
    (startsAtAttributeValues, endsAtAtAttributeValues, lengthAttributeValues) match {
      case (Right(startsAtValue), Right(endsAtValue), Right(lengthValue)) =>
        val attributeValues =
          (startsAtValue, Some(endsAtValue), Some(lengthValue))
        Right(attributeValues)
      case (Right(startsAtValue),
            Right(endsAtValue),
            Left(_: AttributeNotFoundError)) =>
        val attributeValues = (startsAtValue, Some(endsAtValue), None)
        Right(attributeValues)
      case (Right(startsAtValue),
            Left(_: AttributeNotFoundError),
            Right(lengthValue)) =>
        val attributeValues = (startsAtValue, None, Some(lengthValue))
        Right(attributeValues)
      case _ =>
        UnrecoverableError.collectAllErrors(startsAtAttributeValues,
                                            endsAtAtAttributeValues,
                                            lengthAttributeValues)
    }

  def apply(
      startsAt: Int,
      endsAtOpt: Option[Int],
      lengthOpt: Option[Int]
  ): Either[UnrecoverableError, FixedPosition] =
    if (startsAt < 1) {
      FixedLengthPositionErrorMessages.startsAtIsLessThanOne
    } else {
      (endsAtOpt, lengthOpt) match {
        case (None, None) =>
          FixedLengthPositionErrorMessages.endsAtAndLengthAreMissing // dead code
        case (Some(endsAt), None) =>
          FixedLengthPositionWithEndsAt(startsAt, endsAt)

        case (None, Some(length)) =>
          FixedLengthPositionWithLength(startsAt, length)

        case (Some(endsAt), Some(length)) =>
          val calculatedLength = calculateLength(startsAt, endsAt)
          if (calculatedLength == length) {
            val fixedLengthPosition = FixedPosition(startsAt, endsAt)
            Right(fixedLengthPosition)
          } else {
            FixedLengthPositionErrorMessages.incoherentEndsAtAndLengthValues
          }
      }
    }

  private def FixedLengthPositionWithEndsAt(
      startsAt: Int,
      endsAt: Int
  ): Either[UnrecoverableError, FixedPosition] =
    if (startsAt <= endsAt) {
      val fixedLengthPosition = FixedPosition(startsAt, endsAt)
      Right(fixedLengthPosition)
    } else {
      FixedLengthPositionErrorMessages.endsAtIsLessThanStartsAt
    }

  private def FixedLengthPositionWithLength(
      startsAt: Int,
      length: Int
  ): Either[UnrecoverableError, FixedPosition] =
    if (length >= 1) {
      val endsAt = calculateEndsAs(startsAt, length)
      val fixedLengthPosition = FixedPosition(startsAt, endsAt)
      Right(fixedLengthPosition)
    } else {
      FixedLengthPositionErrorMessages.lengthIsLessThanOne
    }

  private def calculateLength(startsAt: Int, endsAt: Int) =
    endsAt - startsAt + 1
  private def calculateEndsAs(startsAt: Int, length: Int) =
    startsAt + length - 1

  private val startsAtKey =
    AttributeValueExtractor("startsAt", AttributeValueConversion.toInt)
  private val endsAtKey =
    AttributeValueExtractor("endsAt", AttributeValueConversion.toInt)
  private val lengthKey =
    AttributeValueExtractor("length", AttributeValueConversion.toInt)
  val listOfAttributesKeys = List(startsAtKey, endsAtKey, lengthKey)
}
