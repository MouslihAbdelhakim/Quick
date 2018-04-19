package com.scalableQuality.quick.core.fileComponentDescriptions.errorMessages

import com.scalableQuality.quick.mantle.error.{
  DependencyError,
  EncounteredError,
  UnrecoverableError
}

object FixedLengthPositionErrorMessages {
  val actionDescription = "validating column position attributes"

  val invalidFixedPositionHeadLine = "Invalid FixedPosition"

  def invalidAttributes(encounteredErrors: List[UnrecoverableError]) = {
    val errorMessage = DependencyError(
      invalidFixedPositionHeadLine,
      encounteredErrors
    )
    Left(errorMessage)
  }

  val startsAtIsLessThanOne = {
    val errorMessage = EncounteredError(
      actionDescription,
      "the provided startsAt position is less than 1",
      s"provide a startsAt value in the range of [1,${Int.MaxValue}]"
    )
    Left(errorMessage)
  }

  val endsAtAndLengthAreMissing = {
    val errorMessage = EncounteredError(
      actionDescription,
      "both attributes, endsAt and length, are missing",
      "provide either an endsAt or a length attribute"
    )
    Left(errorMessage)
  }

  val incoherentEndsAtAndLengthValues = {
    val errorMessage = EncounteredError(
      actionDescription,
      "the length specified by the attribute does not match the length calculated using (endsAt - startsAt + 1)",
      "please review the values of attributes startsAt, endsAt and length"
    )
    Left(errorMessage)
  }

  val endsAtIsLessThanStartsAt = {
    val errorMessage = EncounteredError(
      actionDescription,
      "endsAt is less than startsAt",
      s"please provide an endsAt in the range of [startsAt,${Int.MaxValue}] "
    )
    Left(errorMessage)
  }

  val lengthIsLessThanOne = {
    val errorMessage = EncounteredError(
      actionDescription,
      "the length is less than 1",
      s"please provide a length in the range of [1,${Int.MaxValue}]"
    )
    Left(errorMessage)
  }
}
