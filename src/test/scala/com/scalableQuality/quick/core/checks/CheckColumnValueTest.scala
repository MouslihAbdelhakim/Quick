package com.scalableQuality.quick.core.checks

import org.scalatest.prop.{
  GeneratorDrivenPropertyChecks,
  TableDrivenPropertyChecks
}
import org.scalatest.{FlatSpec, Matchers}

class CheckColumnValueTest
    extends FlatSpec
    with Matchers
    with GeneratorDrivenPropertyChecks
    with TableDrivenPropertyChecks {
  "CheckColumnValue.apply(metaData)" should "return Right(CheckColumnValue) when attributes checkColumnValueMatches and checkColumnValueExists are not present" in {
    val columnDescription = <ColumnDescription />
    val columnValueCheckEither = CheckColumnValue(columnDescription.attributes)
    columnValueCheckEither shouldBe a[Right[_, _]]
  }

  it should "return Right(CheckColumnValue) when only checkColumnValueMatches is present" in {
    val columnDescription = <ColumnDescription checkColumnValueMatches="." />
    val columnValueCheckEither = CheckColumnValue(columnDescription.attributes)
    columnValueCheckEither shouldBe a[Right[_, _]]
  }

  it should "return Right(CheckColumnValue) when only checkColumnValueExists is present" in {
    val columnDescription = <ColumnDescription checkColumnValueExists="false" />
    val columnValueCheckEither = CheckColumnValue(columnDescription.attributes)
    columnValueCheckEither shouldBe a[Right[_, _]]
  }

  it should "return Right(CheckColumnValue) when both checkColumnValueMatches and checkColumnValueExists are present" in {
    val columnDescription =
      <ColumnDescription checkColumnValueMatches="." checkColumnValueExists="false" />
    val columnValueCheckEither = CheckColumnValue(columnDescription.attributes)
    columnValueCheckEither shouldBe a[Right[_, _]]
  }

  it should "return Left(Error) if checkColumnValueMatches contains an invalid regex" in {
    val columnDescription =
      <ColumnDescription checkColumnValueMatches="*" checkColumnValueExists="false" />
    val columnValueCheckEither = CheckColumnValue(columnDescription.attributes)
    columnValueCheckEither shouldBe a[Left[_, _]]
  }

  it should "return Left(Error) if checkColumnValueExists contains an invalid boolean" in {
    val columnDescription =
      <ColumnDescription checkColumnValueMatches=".*" checkColumnValueExists="noBoolean" />
    val columnValueCheckEither = CheckColumnValue(columnDescription.attributes)
    columnValueCheckEither shouldBe a[Left[_, _]]
  }

  "CheckColumnValue.apply(Option[String])" should "checks only existence if no check attributes were supplied" in
    forAll(columnValuesTable) { (columnValue: Option[String]) =>
      {
        val columnDescription = <ColumnDescription />
        val columnValueCheckEither =
          CheckColumnValue(columnDescription.attributes)
        columnValueCheckEither match {
          case Right(checkColumnValue) =>
            checkColumnValue(None) shouldBe false
          case Left(_) =>
            fail()
        }
      }
    }

  val columnValuesTable = Table("columnValue", Some("value"), None)
  it should "always return true if checkColumnValueExists is false and checkColumnValueMatches is absent" in forAll(
    columnValuesTable) { (columnValue: Option[String]) =>
    {
      val columnDescription =
        <ColumnDescription checkColumnValueExists="false" />
      val columnValueCheckEither =
        CheckColumnValue(columnDescription.attributes)
      columnValueCheckEither match {
        case Right(checkColumnValue) =>
          checkColumnValue(columnValue) shouldBe true
        case Left(_) =>
          fail()
      }
    }

  }

  it should "return false if no column value exists and checkColumnValueExists is true and checkColumnValueMatches is absent" in {
    val columnDescription =
      <ColumnDescription checkColumnValueExists="true" />
    val columnValueCheckEither =
      CheckColumnValue(columnDescription.attributes)
    columnValueCheckEither match {
      case Right(checkColumnValue) =>
        checkColumnValue(None) shouldBe false
      case Left(_) =>
        fail()
    }
  }

  it should "return true if the column's value matches the checkColumnValueMatches pattern " in {
    val columnDescription =
      <ColumnDescription checkColumnValueMatches="[0-9]+" />
    val columnValueCheckEither =
      CheckColumnValue(columnDescription.attributes)
    columnValueCheckEither match {
      case Right(checkColumnValue) =>
        checkColumnValue(Some("10092398")) shouldBe true
      case Left(_) =>
        fail()
    }
  }

  it should "return false if the column's value does no match the checkColumnValueMatches pattern " in {
    val columnDescription =
      <ColumnDescription checkColumnValueMatches="[0-9]+" />
    val columnValueCheckEither =
      CheckColumnValue(columnDescription.attributes)
    columnValueCheckEither match {
      case Right(checkColumnValue) =>
        checkColumnValue(Some("letters")) shouldBe false
      case Left(_) =>
        fail()
    }
  }

  it should "return false if the column's value does not exist and checkColumnValueMatches contains a pattern " in {
    val columnDescription =
      <ColumnDescription checkColumnValueMatches="[0-9]+" checkColumnValueExists="false" />
    val columnValueCheckEither =
      CheckColumnValue(columnDescription.attributes)
    columnValueCheckEither match {
      case Right(checkColumnValue) =>
        checkColumnValue(None) shouldBe false
      case Left(_) =>
        fail()
    }
  }

  "CheckColumnValue.checksAreDefined" should "return true when at least one check is defined" in {
    val columnDescription =
        <ColumnDescription checkColumnValueMatches="[0-9]+" checkColumnValueExists="false" />
    val columnValueCheckEither =
      CheckColumnValue(columnDescription.attributes)
    columnValueCheckEither match {
      case Right(checkColumnValue) =>
        checkColumnValue.checksAreDefined shouldBe true
      case Left(_) =>
        fail()
    }
  }

  it should "return true when at more than one check is defined" in {
    val columnDescription =
        <ColumnDescription checkColumnValueMatches="[0-9]+" />
    val columnValueCheckEither =
      CheckColumnValue(columnDescription.attributes)
    columnValueCheckEither match {
      case Right(checkColumnValue) =>
        checkColumnValue.checksAreDefined shouldBe true
      case Left(_) =>
        fail()
    }
  }

  it should "return false when no check is defined" in {
    val columnDescription =
        <ColumnDescription checkColumnValueExists="false"/>
    val columnValueCheckEither =
      CheckColumnValue(columnDescription.attributes)
    columnValueCheckEither match {
      case Right(checkColumnValue) =>
        checkColumnValue.checksAreDefined shouldBe false
      case Left(_) =>
        fail()
    }
  }

}
