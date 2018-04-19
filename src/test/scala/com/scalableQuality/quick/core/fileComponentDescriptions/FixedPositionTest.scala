package com.scalableQuality.quick.core.fileComponentDescriptions

import com.scalableQuality.quick.mantle.parsing.RawRow
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class FixedPositionTest
    extends FlatSpec
    with Matchers
    with GeneratorDrivenPropertyChecks {

  "FixedPosition.apply" should
    "return Right[ErrorMessage,FixedPosition] when ( startsAt > 0 && startsAt <= endsAt)" in forAll(
    maxDiscardedFactor(20.0)) { (startsAt: Int, endsAt: Int) =>
    whenever(startsAt > 0 && startsAt <= endsAt) {
      val columnDescriptionXmlElem =
        <ColumnDescription startsAt={startsAt.toString} endsAt={endsAt.toString} />
      val fixedLengthPositionTest =
        FixedPosition(columnDescriptionXmlElem.attributes)
      fixedLengthPositionTest shouldBe a[Right[_, _]]
    }
  }

  it should "return Right[ErrorMessage,FixedPosition] when ( startsAt > 0 && length > 0)" in forAll(
    maxDiscardedFactor(20.0)) { (startsAt: Int, length: Int) =>
    whenever(startsAt > 0 && length > 0) {
      val columnDescriptionXmlElem =
        <ColumnDescription startsAt={startsAt.toString} length={length.toString} />
      val fixedLengthPositionTest =
        FixedPosition(columnDescriptionXmlElem.attributes)
      fixedLengthPositionTest shouldBe a[Right[_, _]]
    }
  }

  it should "return Left[ErrorMessage,FixedPosition] when (startsAt < 1 )" in forAll {
    (startsAt: Int) =>
      whenever(startsAt < 1) {
        val columnDescriptionXmlElem =
          <ColumnDescription startsAt={startsAt.toString} endsAt="2" />
        val fixedLengthPositionTest =
          FixedPosition(columnDescriptionXmlElem.attributes)
        fixedLengthPositionTest shouldBe a[Left[_, _]]
      }
  }

  it should "return Left[ErrorMessage,FixedPosition] when (startsAt > endsAt )" in forAll {
    (endsAt: Int) =>
      whenever(endsAt < Int.MaxValue) {
        val startsAt = Int.MaxValue
        val columnDescriptionXmlElem =
          <ColumnDescription startsAt={startsAt.toString} endsAt={endsAt.toString} />
        val fixedLengthPositionTest =
          FixedPosition(columnDescriptionXmlElem.attributes)
        fixedLengthPositionTest shouldBe a[Left[_, _]]
      }
  }

  it should "return Left[ErrorMessage,FixedPosition] when (length < 1 )" in forAll {
    (length: Int) =>
      whenever(length < 1) {
        val startsAt = 1
        val columnDescriptionXmlElem =
          <ColumnDescription startsAt={startsAt.toString} length={length.toString} />
        val fixedLengthPositionTest =
          FixedPosition(columnDescriptionXmlElem.attributes)
        fixedLengthPositionTest shouldBe a[Left[_, _]]
      }
  }

  it should "return Left[ErrorMessage,FixedPosition] when the given length and endsAt are incoherent" in {
    val columnDescriptionXmlElem =
      <ColumnDescription startsAt="1" endsAt ="3" length="10" />
    val fixedLengthPositionTest =
      FixedPosition(columnDescriptionXmlElem.attributes)
    fixedLengthPositionTest shouldBe a[Left[_, _]]
  }

  it should "return Left[ErrorMessage,FixedPosition] when startsAt is an invalid int" in {
    val columnDescriptionXmlElem =
      <ColumnDescription startsAt="bla" endsAt ="3" length="3" />
    val fixedLengthPositionTest =
      FixedPosition(columnDescriptionXmlElem.attributes)
    fixedLengthPositionTest shouldBe a[Left[_, _]]
  }

  it should "return Left[ErrorMessage,FixedPosition] when endsAt is an invalid int" in {
    val columnDescriptionXmlElem =
      <ColumnDescription startsAt="1" endsAt ="dude" length="3" />
    val fixedLengthPositionTest =
      FixedPosition(columnDescriptionXmlElem.attributes)
    fixedLengthPositionTest shouldBe a[Left[_, _]]
  }

  it should "return Left[ErrorMessage,FixedPosition] when length is an invalid int" in {
    val columnDescriptionXmlElem =
      <ColumnDescription startsAt="1" endsAt ="3" length="bib" />
    val fixedLengthPositionTest =
      FixedPosition(columnDescriptionXmlElem.attributes)
    fixedLengthPositionTest shouldBe a[Left[_, _]]
  }

  it should "return Left[ErrorMessage,FixedPosition] when startsAt is missing" in {
    val columnDescriptionXmlElem = <ColumnDescription endsAt ="3" length="3" />
    val fixedLengthPositionTest =
      FixedPosition(columnDescriptionXmlElem.attributes)
    fixedLengthPositionTest shouldBe a[Left[_, _]]
  }

  it should "return Left[ErrorMessage,FixedPosition] when endsAt and length are missing" in {
    val columnDescriptionXmlElem = <ColumnDescription startsAt="1" />
    val fixedLengthPositionTest =
      FixedPosition(columnDescriptionXmlElem.attributes)
    fixedLengthPositionTest shouldBe a[Left[_, _]]
  }

  it should "return Left[ErrorMessage,FixedPosition] when both endsAt and length are missing" in {
    val fixedLengthPositionTest =
      FixedPosition(1,None, None)
    fixedLengthPositionTest shouldBe a[Left[_, _]]
  }

  "FixedPosition.extractColumnValue" should "return Some(subString) where the first char is at (startsAt - 1) and the last at (endsAt - 1)" in {
    val fixedLengthPositionTestEither = FixedPosition(
      7,
      Some(12),
      None
    )
    val row = RawRow("PraiseTheSun", 1)
    val expectedColumnValue = Some("TheSun")
    fixedLengthPositionTestEither match {
      case Left(_) =>
        fail
      case Right(fixedLengthPosition) =>
        fixedLengthPosition.extractColumnValue(row) shouldBe expectedColumnValue
    }
  }

  it should "return Some(subString) where the first char is at (startsAt - 1) and have a length of length" in {
    val fixedLengthPositionTestEither = FixedPosition(
      1,
      None,
      Some(12)
    )
    val row = RawRow("PraiseTheSun", 1)
    val expectedColumnValue = Some("PraiseTheSun")
    fixedLengthPositionTestEither match {
      case Left(_) =>
        fail
      case Right(fixedLengthPosition) =>
        fixedLengthPosition.extractColumnValue(row) shouldBe expectedColumnValue
    }
  }

  it should "return Some(subString) one char when length is 1" in {
    val fixedLengthPositionTestEither = FixedPosition(
      1,
      None,
      Some(1)
    )
    val row = RawRow("PraiseTheSun", 1)
    val expectedColumnValue = Some("P")
    fixedLengthPositionTestEither match {
      case Left(_) =>
        fail
      case Right(fixedLengthPosition) =>
        fixedLengthPosition.extractColumnValue(row) shouldBe expectedColumnValue
    }
  }

  it should "return None if the string length < startsAt" in {
    val fixedLengthPositionTestEither = FixedPosition(
      13,
      Some(13),
      None
    )
    val row = RawRow("PraiseTheSun", 1)
    val expectedColumnValue = None
    fixedLengthPositionTestEither match {
      case Left(_) =>
        fail
      case Right(fixedLengthPosition) =>
        fixedLengthPosition.extractColumnValue(row) shouldBe expectedColumnValue
    }
  }

  it should "return None if the string length < endsAt - 1" in {
    val fixedLengthPositionTestEither = FixedPosition(
      3,
      Some(13),
      None
    )
    val row = RawRow("PraiseTheSun", 1)
    val expectedColumnValue = Some("aiseTheSun")
    fixedLengthPositionTestEither match {
      case Left(_) =>
        fail
      case Right(fixedLengthPosition) =>
        fixedLengthPosition.extractColumnValue(row) shouldBe expectedColumnValue
    }
  }

  it should "return subString from startAt to the end of string if the string's length is less than column's length" in {
    val fixedLengthPositionTestEither = FixedPosition(
      3,
      None,
      Some(13)
    )
    val row = RawRow("PraiseTheSun", 1)
    val expectedColumnValue = Some("aiseTheSun")
    fixedLengthPositionTestEither match {
      case Left(_) =>
        fail
      case Right(fixedLengthPosition) =>
        fixedLengthPosition.extractColumnValue(row) shouldBe expectedColumnValue
    }
  }
}
