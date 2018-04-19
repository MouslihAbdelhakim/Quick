package com.scalableQuality.quick.mantle.error

trait UnrecoverableError {
  override def toString: String
}

object UnrecoverableError {
  def collectAllErrors[OutType](
      probableErrors: Either[UnrecoverableError, Any]*
  ): Either[List[UnrecoverableError], OutType] = {
    val errorsList = probableErrors.collect {
      case Left(error) => error
    }.toList
    Left(errorsList)
  }

  def collectAllErrorsToList[InType](
      probableErrors: Either[UnrecoverableError, InType]*
  ): List[UnrecoverableError] =
    probableErrors.collect {
      case Left(error) => error
    }.toList
}
