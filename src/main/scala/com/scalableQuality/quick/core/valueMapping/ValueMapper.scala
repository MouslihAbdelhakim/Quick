package com.scalableQuality.quick.core.valueMapping

import com.scalableQuality.quick.core.valueMapping.errorMessages.ValueMapperErrorMessages
import com.scalableQuality.quick.mantle.constructFromXml._
import com.scalableQuality.quick.mantle.error.UnrecoverableError

import scala.xml.MetaData

class ValueMapper private[core] (
    transformations: List[ValueMapperFunction]
) {

  def apply(value: Option[String]): Option[String] =
    transformations.foldLeft(value) {
      (previousVal: Option[String], transformation: ValueMapperFunction) =>
        transformation(previousVal)
    }
  //def contains(valueMapperFunction: ValueMapperFunction): Boolean = transformations.contains(valueMapperFunction)
}

object ValueMapper {

  def apply(listOfTransformation: List[ValueMapperFunction]): ValueMapper = {
    new ValueMapper(listOfTransformation)
  }

  // TODO : improve attribute extraction
  def apply(elemMetaData: MetaData): Either[UnrecoverableError, ValueMapper] = {

    val attributeValues =
      AttributesValuesExtractor(elemMetaData, listOfAttributesKeys)

    val trimValueAttribute = attributeValues.get(trimKey)
    val ignoreCaseValueAttribute = attributeValues.get(ignoreCaseKey)

    val classParameters =
      validateAttributeValues(trimValueAttribute, ignoreCaseValueAttribute)

    classParameters match {
      case Right((shouldTrim, shouldIgnoreCase)) =>
        val valueMappersFunctions
          : List[ValueMapperFunction] = Trim(shouldTrim) ::: ToUpperCase(
          shouldIgnoreCase)
        val valueMapper = ValueMapper(valueMappersFunctions)
        Right(valueMapper)

      case Left(errorMessages) =>
        ValueMapperErrorMessages.invalidAttributes(errorMessages)
    }
  }

  private def validateAttributeValues(
      trimValueAttribute: Either[UnrecoverableError, Boolean],
      ignoreCaseValueAttribute: Either[UnrecoverableError, Boolean]
  ): Either[List[UnrecoverableError], (Boolean, Boolean)] = {
    (trimValueAttribute, ignoreCaseValueAttribute) match {
      case (Right(trim), Right(ignoreCase)) =>
        val classParameters = (trim, ignoreCase)
        Right(classParameters)

      case _ =>
        UnrecoverableError.collectAllErrors(trimValueAttribute,
                                            ignoreCaseValueAttribute)
    }
  }

  private val defaultValueMapperInclusion = Right(false)
  private val trimKey = AttributeValueExtractor(
    "trimComparisonValue",
    AttributeValueConversion.toBoolean,
    defaultValueMapperInclusion)
  private val ignoreCaseKey = AttributeValueExtractor(
    "ignoreComparisonValueCase",
    AttributeValueConversion.toBoolean,
    defaultValueMapperInclusion)
  val listOfAttributesKeys = List(trimKey, ignoreCaseKey)

}
