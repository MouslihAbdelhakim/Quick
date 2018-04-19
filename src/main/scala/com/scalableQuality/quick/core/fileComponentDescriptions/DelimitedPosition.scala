package com.scalableQuality.quick.core.fileComponentDescriptions

import com.scalableQuality.quick.core.fileComponentDescriptions.errorMessages.DelimitedPositionErrorMessages
import com.scalableQuality.quick.mantle.constructFromXml._
import com.scalableQuality.quick.mantle.error.UnrecoverableError

import scala.xml.MetaData

case class DelimitedPosition(
    position: Int,
    override val toString: String
) {
  def extractColumnValue(row: Vector[String]): Option[String] =
    row.lift(this.position)
}

object DelimitedPosition {

  val positionAttribute =
    AttributeValueExtractor("position", AttributeValueConversion.toInt)

  val listOfAttributesKeys = List(positionAttribute)

  def apply(
      metaData: MetaData
  ): Either[UnrecoverableError, DelimitedPosition] = {
    val attributesValues =
      AttributesValuesExtractor(metaData, positionAttribute)
    val positionAttributeValue = attributesValues.get(positionAttribute)
    val validatedPositionAttributeValue = validateExtractedPosition(
      positionAttributeValue)

    validatedPositionAttributeValue match {
      case Left(error) =>
        DelimitedPositionErrorMessages.invalidAttributes(error)
      case Right(position) =>
        val delimitedPosition = DelimitedPosition(position)
        Right(delimitedPosition)
    }
  }

  def apply(
      position: Int
  ) =
    new DelimitedPosition(
      position - 1,
      "%4d".format(position)
    )

  private def validateExtractedPosition(
      extractedPosition: Either[UnrecoverableError, Int]
  ): Either[UnrecoverableError, Int] = extractedPosition match {
    case Right(position) =>
      if (position < 1)
        DelimitedPositionErrorMessages.positionIsLessThanOne
      else
        Right(position)
    case Left(_) => extractedPosition
  }
}
