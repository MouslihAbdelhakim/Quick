package com.scalableQuality.quick.core.fileComponentDescriptions

import com.scalableQuality.quick.core.Reporting.{InvalidColumns, IrrelevantColumns, ReportingColumns, ValidColumns}
import com.scalableQuality.quick.core.checks.CheckColumnValue
import com.scalableQuality.quick.core.phases.{MatchingStage, ReportingStage, ShouldUseDuring, ValidationStage}
import com.scalableQuality.quick.core.valueMapping.ValueMapper
import com.scalableQuality.quick.mantle.parsing.RawRow
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class FixedRowDividerTest
    extends FlatSpec
    with Matchers
    with GeneratorDrivenPropertyChecks {

  "FixedRowDivider.isMatchable" should
    "return true if at least one could should be used in matching and false otherwise" in forAll {
    (firstColumnMatching: Boolean,
     secondColumnMatching: Boolean,
     thirdColumnMatching: Boolean) =>
      val firstColumnDescriptionElem = <ColumnDescription
        label="firstColumn"
        startsAt="1"
        endsAt="3"
        useDuringMatching={firstColumnMatching.toString}/>
      val secondColumnDescriptionElem = <ColumnDescription
        label="secondColumn"
        startsAt="4"
        endsAt="7"
        useDuringMatching={secondColumnMatching.toString}/>

      val thirdColumnDescriptionElem = <ColumnDescription
        label="thirdColumn"
        startsAt="8"
        endsAt="12"
        useDuringMatching={thirdColumnMatching.toString}/>
      val firstColumnDescriptionEither =
        FixedColumnDescription(firstColumnDescriptionElem.attributes)
      val secondColumnDescriptionEither =
        FixedColumnDescription(secondColumnDescriptionElem.attributes)
      val thirdColumnDescriptionEither =
        FixedColumnDescription(thirdColumnDescriptionElem.attributes)

      (firstColumnDescriptionEither,
       secondColumnDescriptionEither,
       thirdColumnDescriptionEither) match {
        case (Right(firstColumnDescription),
              Right(secondColumnDescription),
              Right(thirdColumnDescription)) =>
          val columnDescriptionList = List(firstColumnDescription,
                                           secondColumnDescription,
                                           thirdColumnDescription)
          val fixedRowDivider = FixedRowDivider(columnDescriptionList)
          fixedRowDivider.isMatchable shouldBe (firstColumnMatching || secondColumnMatching || thirdColumnMatching)
        case _ => fail
      }
  }

  "FixedRowDivider.keepOnlyColumnsDescriptionsUsedIn" should
    "return an FixedRowDivider containing all columns used during validation " in {
    val firstColumnDescriptionElem = <ColumnDescription
      label="firstColumn"
      startsAt="1"
      endsAt="3"
      useDuringValidation="true"
      />
    val secondColumnDescriptionElem = <ColumnDescription
      label="secondColumn"
      startsAt="4"
      endsAt="7"
      useDuringValidation="true"
      />

    val thirdColumnDescriptionElem = <ColumnDescription
      label="thirdColumn"
      startsAt="8"
      endsAt="12"
      />
    val firstColumnDescriptionEither =
      FixedColumnDescription(firstColumnDescriptionElem.attributes)
    val secondColumnDescriptionEither =
      FixedColumnDescription(secondColumnDescriptionElem.attributes)
    val thirdColumnDescriptionEither =
      FixedColumnDescription(thirdColumnDescriptionElem.attributes)

    (firstColumnDescriptionEither,
     secondColumnDescriptionEither,
     thirdColumnDescriptionEither) match {
      case (Right(firstColumnDescription),
            Right(secondColumnDescription),
            Right(thirdColumnDescription)) =>
        val columnDescriptionList = List(firstColumnDescription,
                                         secondColumnDescription,
                                         thirdColumnDescription)
        val fixedRowDivider = FixedRowDivider(columnDescriptionList)

        val expectedColumnDescriptionList =
          List(firstColumnDescription, secondColumnDescription)
        val expectedFixedRowDivider =
          FixedRowDivider(expectedColumnDescriptionList)
        fixedRowDivider.keepOnlyColumnsDescriptionsUsedIn(ValidationStage) shouldBe expectedFixedRowDivider
      case _ => fail
    }
  }

  it should
    "return an FixedRowDivider containing all columns used during Matching " in {
    val firstColumnDescriptionElem = <ColumnDescription
      label="firstColumn"
      startsAt="1"
      endsAt="3"
      useDuringMatching="true"
      />
    val secondColumnDescriptionElem = <ColumnDescription
      label="secondColumn"
      startsAt="4"
      endsAt="7"
      />

    val thirdColumnDescriptionElem = <ColumnDescription
      label="thirdColumn"
      startsAt="8"
      endsAt="12"
      useDuringMatching="true"
      />
    val firstColumnDescriptionEither =
      FixedColumnDescription(firstColumnDescriptionElem.attributes)
    val secondColumnDescriptionEither =
      FixedColumnDescription(secondColumnDescriptionElem.attributes)
    val thirdColumnDescriptionEither =
      FixedColumnDescription(thirdColumnDescriptionElem.attributes)

    (firstColumnDescriptionEither,
     secondColumnDescriptionEither,
     thirdColumnDescriptionEither) match {
      case (Right(firstColumnDescription),
            Right(secondColumnDescription),
            Right(thirdColumnDescription)) =>
        val columnDescriptionList = List(firstColumnDescription,
                                         secondColumnDescription,
                                         thirdColumnDescription)
        val fixedRowDivider = FixedRowDivider(columnDescriptionList)

        val expectedColumnDescriptionList =
          List(firstColumnDescription, thirdColumnDescription)
        val expectedFixedRowDivider =
          FixedRowDivider(expectedColumnDescriptionList)

        fixedRowDivider.keepOnlyColumnsDescriptionsUsedIn(MatchingStage) shouldBe expectedFixedRowDivider
      case _ => fail
    }
  }

  it should
    "return an FixedRowDivider containing all columns used during Reporting " in {
    val firstColumnDescriptionElem = <ColumnDescription
      label="firstColumn"
      startsAt="1"
      endsAt="3"
      />
    val secondColumnDescriptionElem = <ColumnDescription
      label="secondColumn"
      startsAt="4"
      endsAt="7"
      useDuringReporting="true"
      />

    val thirdColumnDescriptionElem = <ColumnDescription
      label="thirdColumn"
      startsAt="8"
      endsAt="12"
      useDuringReporting="true"
      />
    val firstColumnDescriptionEither =
      FixedColumnDescription(firstColumnDescriptionElem.attributes)
    val secondColumnDescriptionEither =
      FixedColumnDescription(secondColumnDescriptionElem.attributes)
    val thirdColumnDescriptionEither =
      FixedColumnDescription(thirdColumnDescriptionElem.attributes)

    (firstColumnDescriptionEither,
     secondColumnDescriptionEither,
     thirdColumnDescriptionEither) match {
      case (Right(firstColumnDescription),
            Right(secondColumnDescription),
            Right(thirdColumnDescription)) =>
        val columnDescriptionList = List(firstColumnDescription,
                                         secondColumnDescription,
                                         thirdColumnDescription)
        val fixedRowDivider = FixedRowDivider(columnDescriptionList)

        val expectedColumnDescriptionList =
          List(secondColumnDescription, thirdColumnDescription)
        val expectedFixedRowDivider =
          FixedRowDivider(expectedColumnDescriptionList)

        fixedRowDivider.keepOnlyColumnsDescriptionsUsedIn(ReportingStage) shouldBe expectedFixedRowDivider
      case _ => fail
    }
  }

  it should
    "return an FixedRowDivider containing all columns used during from different stages " in {
    val firstColumnDescriptionElem = <ColumnDescription
      label="firstColumn"
      startsAt="1"
      endsAt="3"
      useDuringValidation="true"
      />
    val secondColumnDescriptionElem = <ColumnDescription
      label="secondColumn"
      startsAt="4"
      endsAt="7"
      useDuringMatching="true"
      />

    val thirdColumnDescriptionElem = <ColumnDescription
      label="thirdColumn"
      startsAt="8"
      endsAt="12"
      useDuringReporting="true"
      />
    val firstColumnDescriptionEither =
      FixedColumnDescription(firstColumnDescriptionElem.attributes)
    val secondColumnDescriptionEither =
      FixedColumnDescription(secondColumnDescriptionElem.attributes)
    val thirdColumnDescriptionEither =
      FixedColumnDescription(thirdColumnDescriptionElem.attributes)

    (firstColumnDescriptionEither,
     secondColumnDescriptionEither,
     thirdColumnDescriptionEither) match {
      case (Right(firstColumnDescription),
            Right(secondColumnDescription),
            Right(thirdColumnDescription)) =>
        val columnDescriptionList = List(firstColumnDescription,
                                         secondColumnDescription,
                                         thirdColumnDescription)
        val fixedRowDivider = FixedRowDivider(columnDescriptionList)

        val expectedColumnDescriptionList = List(firstColumnDescription,
                                                 secondColumnDescription,
                                                 thirdColumnDescription)
        val expectedFixedRowDivider =
          FixedRowDivider(expectedColumnDescriptionList)

        fixedRowDivider.keepOnlyColumnsDescriptionsUsedIn(
          ValidationStage,
          MatchingStage,
          ReportingStage) shouldBe expectedFixedRowDivider
      case _ => fail
    }
  }

  "FixedRowDivider.columnsComparisonValuesFor" should "extract column values of all 3 validation columns" in {
    val rawRow = RawRow("FirstColumnSecondColumnThirdColumn", 1)
    val firstColumnDescriptionElem = <ColumnDescription
      label="FirstColumn"
      startsAt="1"
      endsAt="11"
      useDuringValidation="true"
      />
    val secondColumnDescriptionElem = <ColumnDescription
      label="SecondColumn"
      startsAt="12"
      endsAt="23"
      useDuringValidation="true"
      />

    val thirdColumnDescriptionElem = <ColumnDescription
      label="ThirdColumn"
      startsAt="24"
      endsAt="34"
      useDuringValidation="true"
      />

    val firstColumnDescriptionEither =
      FixedColumnDescription(firstColumnDescriptionElem.attributes)
    val secondColumnDescriptionEither =
      FixedColumnDescription(secondColumnDescriptionElem.attributes)
    val thirdColumnDescriptionEither =
      FixedColumnDescription(thirdColumnDescriptionElem.attributes)

    (firstColumnDescriptionEither,
     secondColumnDescriptionEither,
     thirdColumnDescriptionEither) match {
      case (Right(firstColumnDescription),
            Right(secondColumnDescription),
            Right(thirdColumnDescription)) =>
        val columnDescriptionList = List(firstColumnDescription,
                                         secondColumnDescription,
                                         thirdColumnDescription)
        val fixedRowDivider = FixedRowDivider(columnDescriptionList)
        val expectedColumnValues =
          List(Some("FirstColumn"), Some("SecondColumn"), Some("ThirdColumn"))
        fixedRowDivider.columnsComparisonValuesFor(ValidationStage, rawRow) shouldBe expectedColumnValues
      case _ => fail
    }
  }

  it should "extract column values of only validation columns" in {
    val rawRow = RawRow("FirstColumnSecondColumnThirdColumn", 1)
    val firstColumnDescriptionElem = <ColumnDescription
      label="FirstColumn"
      startsAt="1"
      endsAt="11"
      useDuringValidation="true"
      />
    val secondColumnDescriptionElem = <ColumnDescription
      label="SecondColumn"
      startsAt="12"
      endsAt="23"
      useDuringValidation="false"
      />

    val thirdColumnDescriptionElem = <ColumnDescription
      label="ThirdColumn"
      startsAt="24"
      endsAt="34"
      useDuringValidation="true"
      />

    val firstColumnDescriptionEither =
      FixedColumnDescription(firstColumnDescriptionElem.attributes)
    val secondColumnDescriptionEither =
      FixedColumnDescription(secondColumnDescriptionElem.attributes)
    val thirdColumnDescriptionEither =
      FixedColumnDescription(thirdColumnDescriptionElem.attributes)

    (firstColumnDescriptionEither,
     secondColumnDescriptionEither,
     thirdColumnDescriptionEither) match {
      case (Right(firstColumnDescription),
            Right(secondColumnDescription),
            Right(thirdColumnDescription)) =>
        val columnDescriptionList = List(firstColumnDescription,
                                         secondColumnDescription,
                                         thirdColumnDescription)
        val fixedRowDivider = FixedRowDivider(columnDescriptionList)

        val expectedColumnValues =
          List(Some("FirstColumn"), Some("ThirdColumn"))
        fixedRowDivider.columnsComparisonValuesFor(ValidationStage, rawRow) shouldBe expectedColumnValues
      case _ => fail
    }
  }

  it should "the extracted column values should include None for every validation column that does not exist" in {
    val rawRow = RawRow("FirstColumn", 1)
    val firstColumnDescriptionElem = <ColumnDescription
      label="FirstColumn"
      startsAt="1"
      endsAt="11"
      useDuringValidation="true"
      />
    val secondColumnDescriptionElem = <ColumnDescription
      label="SecondColumn"
      startsAt="12"
      endsAt="23"
      useDuringValidation="true"
      />

    val thirdColumnDescriptionElem = <ColumnDescription
      label="ThirdColumn"
      startsAt="24"
      endsAt="34"
      useDuringValidation="true"
      />

    val firstColumnDescriptionEither =
      FixedColumnDescription(firstColumnDescriptionElem.attributes)
    val secondColumnDescriptionEither =
      FixedColumnDescription(secondColumnDescriptionElem.attributes)
    val thirdColumnDescriptionEither =
      FixedColumnDescription(thirdColumnDescriptionElem.attributes)

    (firstColumnDescriptionEither,
     secondColumnDescriptionEither,
     thirdColumnDescriptionEither) match {
      case (Right(firstColumnDescription),
            Right(secondColumnDescription),
            Right(thirdColumnDescription)) =>
        val columnDescriptionList = List(firstColumnDescription,
                                         secondColumnDescription,
                                         thirdColumnDescription)
        val fixedRowDivider = FixedRowDivider(columnDescriptionList)
        val expectedColumnValues = List(Some("FirstColumn"), None, None)

        fixedRowDivider.columnsComparisonValuesFor(ValidationStage, rawRow) shouldBe expectedColumnValues
      case _ => fail
    }
  }

  it should "extract column values of all 3 matching columns" in {
    val rawRow = RawRow("FirstColumnSecondColumnThirdColumn", 1)
    val firstColumnDescriptionElem = <ColumnDescription
      label="FirstColumn"
      startsAt="1"
      endsAt="11"
      useDuringMatching="true"
      />
    val secondColumnDescriptionElem = <ColumnDescription
      label="SecondColumn"
      startsAt="12"
      endsAt="23"
      useDuringMatching="true"
      />

    val thirdColumnDescriptionElem = <ColumnDescription
      label="ThirdColumn"
      startsAt="24"
      endsAt="34"
      useDuringMatching="true"
      />

    val firstColumnDescriptionEither =
      FixedColumnDescription(firstColumnDescriptionElem.attributes)
    val secondColumnDescriptionEither =
      FixedColumnDescription(secondColumnDescriptionElem.attributes)
    val thirdColumnDescriptionEither =
      FixedColumnDescription(thirdColumnDescriptionElem.attributes)

    (firstColumnDescriptionEither,
     secondColumnDescriptionEither,
     thirdColumnDescriptionEither) match {
      case (Right(firstColumnDescription),
            Right(secondColumnDescription),
            Right(thirdColumnDescription)) =>
        val columnDescriptionList = List(firstColumnDescription,
                                         secondColumnDescription,
                                         thirdColumnDescription)
        val fixedRowDivider = FixedRowDivider(columnDescriptionList)
        val expectedColumnValues =
          List(Some("FirstColumn"), Some("SecondColumn"), Some("ThirdColumn"))
        fixedRowDivider.columnsComparisonValuesFor(MatchingStage, rawRow) shouldBe expectedColumnValues
      case _ => fail
    }
  }

  it should "extract column values of only matching columns" in {
    val rawRow = RawRow("FirstColumnSecondColumnThirdColumn", 1)
    val firstColumnDescriptionElem = <ColumnDescription
      label="FirstColumn"
      startsAt="1"
      endsAt="11"
      useDuringMatching="true"
      />
    val secondColumnDescriptionElem = <ColumnDescription
      label="SecondColumn"
      startsAt="12"
      endsAt="23"
      useDuringMatching="false"
      />

    val thirdColumnDescriptionElem = <ColumnDescription
      label="ThirdColumn"
      startsAt="24"
      endsAt="34"
      useDuringMatching="true"
      />

    val firstColumnDescriptionEither =
      FixedColumnDescription(firstColumnDescriptionElem.attributes)
    val secondColumnDescriptionEither =
      FixedColumnDescription(secondColumnDescriptionElem.attributes)
    val thirdColumnDescriptionEither =
      FixedColumnDescription(thirdColumnDescriptionElem.attributes)

    (firstColumnDescriptionEither,
     secondColumnDescriptionEither,
     thirdColumnDescriptionEither) match {
      case (Right(firstColumnDescription),
            Right(secondColumnDescription),
            Right(thirdColumnDescription)) =>
        val columnDescriptionList = List(firstColumnDescription,
                                         secondColumnDescription,
                                         thirdColumnDescription)
        val fixedRowDivider = FixedRowDivider(columnDescriptionList)

        val expectedColumnValues =
          List(Some("FirstColumn"), Some("ThirdColumn"))
        fixedRowDivider.columnsComparisonValuesFor(MatchingStage, rawRow) shouldBe expectedColumnValues
      case _ => fail
    }
  }

  it should "the extracted column values should include None for every matching column that does not exist" in {
    val rawRow = RawRow("FirstColumn", 1)
    val firstColumnDescriptionElem = <ColumnDescription
      label="FirstColumn"
      startsAt="1"
      endsAt="11"
      useDuringMatching="true"
      />
    val secondColumnDescriptionElem = <ColumnDescription
      label="SecondColumn"
      startsAt="12"
      endsAt="23"
      useDuringMatching="true"
      />

    val thirdColumnDescriptionElem = <ColumnDescription
      label="ThirdColumn"
      startsAt="24"
      endsAt="34"
      useDuringMatching="true"
      />

    val firstColumnDescriptionEither =
      FixedColumnDescription(firstColumnDescriptionElem.attributes)
    val secondColumnDescriptionEither =
      FixedColumnDescription(secondColumnDescriptionElem.attributes)
    val thirdColumnDescriptionEither =
      FixedColumnDescription(thirdColumnDescriptionElem.attributes)

    (firstColumnDescriptionEither,
     secondColumnDescriptionEither,
     thirdColumnDescriptionEither) match {
      case (Right(firstColumnDescription),
            Right(secondColumnDescription),
            Right(thirdColumnDescription)) =>
        val columnDescriptionList = List(firstColumnDescription,
                                         secondColumnDescription,
                                         thirdColumnDescription)
        val fixedRowDivider = FixedRowDivider(columnDescriptionList)
        val expectedColumnValues = List(Some("FirstColumn"), None, None)

        fixedRowDivider.columnsComparisonValuesFor(MatchingStage, rawRow) shouldBe expectedColumnValues
      case _ => fail
    }
  }

  "FixedRowDivider.compare" should "return ValidColumns, IrrelevantColumns, ReportingColumns and InvalidColumns" in {
    val leftRawRow = Some(RawRow("OneTwoThreeFour", 1))
    val rightRawRow = Some(RawRow("OneBwoRRreeSour", 2))
    val validColumnsDescriptionElem = <ColumnDescription
      label="valid column"
      startsAt="1"
      endsAt="3"
      useDuringValidation="true"
      useDuringReporting="false"
      />
    val irrelevantColumnsDescriptionElem = <ColumnDescription
      label="valid column"
      startsAt="4"
      endsAt="6"
      useDuringValidation="false"
      useDuringReporting="false"
      />
    val reportingColumnsDescriptionElem = <ColumnDescription
      label="reporting column"
      startsAt="7"
      endsAt="11"
      useDuringValidation="false"
      useDuringReporting="true"
      />
    val invalidColumnsDescriptionElem = <ColumnDescription
      label="invalid column"
      startsAt="12"
      endsAt="15"
      useDuringValidation="true"
      useDuringReporting="false"
      />
    val validColumnsDescriptionEither =
      FixedColumnDescription(validColumnsDescriptionElem.attributes)
    val irrelevantColumnsDescriptionEither =
      FixedColumnDescription(irrelevantColumnsDescriptionElem.attributes)
    val reportingColumnsDescriptionEither =
      FixedColumnDescription(reportingColumnsDescriptionElem.attributes)
    val invalidColumnsDescriptionEither =
      FixedColumnDescription(invalidColumnsDescriptionElem.attributes)
    (
      validColumnsDescriptionEither,
      irrelevantColumnsDescriptionEither,
      reportingColumnsDescriptionEither,
      invalidColumnsDescriptionEither
    ) match {
      case (
          Right(validColumnsDescription),
          Right(irrelevantColumnsDescription),
          Right(reportingColumnsDescription),
          Right(invalidColumnsDescription)
          ) =>
        val columnDescriptionList = List(validColumnsDescription,
                                         irrelevantColumnsDescription,
                                         reportingColumnsDescription,
                                         invalidColumnsDescription)
        val fixedRowDivider = FixedRowDivider(columnDescriptionList)

        val expectedReportingColumns = ReportingColumns(
          Some("Three"),
          Some("RRree"),
          "   7-11  ",
          "reporting column"
        )

        val expectedInvalidColumns = InvalidColumns(
          Some("Four"),
          Some("Sour"),
          "  12-15  ",
          "invalid column"
        )

        val expectedComparison = List(ValidColumns,
                                      IrrelevantColumns,
                                      expectedReportingColumns,
                                      expectedInvalidColumns)

        fixedRowDivider.compare(leftRawRow, rightRawRow) shouldBe expectedComparison
      case _ =>
        fail
    }
  }

  "FixedRowDivider.usableDuringValidation" should
    "return true if all column descriptions have default values" in {
      val firstColumnDescriptionElem = <ColumnDescription
        label="firstColumn"
        startsAt="1"
        endsAt="3"
        />
      val secondColumnDescriptionElem = <ColumnDescription
        label="secondColumn"
        startsAt="4"
        endsAt="7"
        />

      val thirdColumnDescriptionElem = <ColumnDescription
        label="thirdColumn"
        startsAt="8"
        endsAt="12"
        />
      val firstColumnDescriptionEither =
        FixedColumnDescription(firstColumnDescriptionElem.attributes)
      val secondColumnDescriptionEither =
        FixedColumnDescription(secondColumnDescriptionElem.attributes)
      val thirdColumnDescriptionEither =
        FixedColumnDescription(thirdColumnDescriptionElem.attributes)

      (firstColumnDescriptionEither,
        secondColumnDescriptionEither,
        thirdColumnDescriptionEither) match {
        case (Right(firstColumnDescription),
        Right(secondColumnDescription),
        Right(thirdColumnDescription)) =>
          val columnDescriptionList = List(firstColumnDescription,
            secondColumnDescription,
            thirdColumnDescription)
          val fixedRowDivider = FixedRowDivider(columnDescriptionList)
          fixedRowDivider.usableDuringValidation shouldBe true
        case _ => fail
      }
  }

  it should
  "return true if at least one column descriptions should be used during validation" in {
    val firstColumnDescriptionElem = <ColumnDescription
      label="firstColumn"
      startsAt="1"
      endsAt="3"
      useDuringValidation="true"
      />
    val secondColumnDescriptionElem = <ColumnDescription
      label="secondColumn"
      startsAt="4"
      endsAt="7"
      checkColumnValueExists="false"
      />

    val thirdColumnDescriptionElem = <ColumnDescription
      label="thirdColumn"
      startsAt="8"
      endsAt="12"
      checkColumnValueExists="false"
      />
    val firstColumnDescriptionEither =
      FixedColumnDescription(firstColumnDescriptionElem.attributes)
    val secondColumnDescriptionEither =
      FixedColumnDescription(secondColumnDescriptionElem.attributes)
    val thirdColumnDescriptionEither =
      FixedColumnDescription(thirdColumnDescriptionElem.attributes)

    (firstColumnDescriptionEither,
      secondColumnDescriptionEither,
      thirdColumnDescriptionEither) match {
      case (Right(firstColumnDescription),
      Right(secondColumnDescription),
      Right(thirdColumnDescription)) =>
        val columnDescriptionList = List(firstColumnDescription,
          secondColumnDescription,
          thirdColumnDescription)
        val fixedRowDivider = FixedRowDivider(columnDescriptionList)
        fixedRowDivider.usableDuringValidation shouldBe true
      case _ => fail
    }
  }

  it should
    "return true if at more than one column descriptions should be used during validation" in {
    val firstColumnDescriptionElem = <ColumnDescription
      label="firstColumn"
      startsAt="1"
      endsAt="3"
      useDuringValidation="true"
      checkColumnValueExists="false"
      />
    val secondColumnDescriptionElem = <ColumnDescription
      label="secondColumn"
      startsAt="4"
      endsAt="7"
      useDuringValidation="true"
      checkColumnValueExists="false"
      />

    val thirdColumnDescriptionElem = <ColumnDescription
      label="thirdColumn"
      startsAt="8"
      endsAt="12"
      useDuringValidation="true"
      checkColumnValueExists="false"
      />
    val firstColumnDescriptionEither =
      FixedColumnDescription(firstColumnDescriptionElem.attributes)
    val secondColumnDescriptionEither =
      FixedColumnDescription(secondColumnDescriptionElem.attributes)
    val thirdColumnDescriptionEither =
      FixedColumnDescription(thirdColumnDescriptionElem.attributes)

    (firstColumnDescriptionEither,
      secondColumnDescriptionEither,
      thirdColumnDescriptionEither) match {
      case (Right(firstColumnDescription),
      Right(secondColumnDescription),
      Right(thirdColumnDescription)) =>
        val columnDescriptionList = List(firstColumnDescription,
          secondColumnDescription,
          thirdColumnDescription)
        val fixedRowDivider = FixedRowDivider(columnDescriptionList)
        fixedRowDivider.usableDuringValidation shouldBe true
      case _ => fail
    }
  }

  it should
    "return true if at least one column descriptions should be checked" in {
    val firstColumnDescriptionElem = <ColumnDescription
      label="firstColumn"
      startsAt="1"
      endsAt="3"
      checkColumnValueMatches=".?"
      checkColumnValueExists="false"
      />
    val secondColumnDescriptionElem = <ColumnDescription
      label="secondColumn"
      startsAt="4"
      endsAt="7"
      checkColumnValueExists="false"
      />

    val thirdColumnDescriptionElem = <ColumnDescription
      label="thirdColumn"
      startsAt="8"
      endsAt="12"
      checkColumnValueExists="false"
      />
    val firstColumnDescriptionEither =
      FixedColumnDescription(firstColumnDescriptionElem.attributes)
    val secondColumnDescriptionEither =
      FixedColumnDescription(secondColumnDescriptionElem.attributes)
    val thirdColumnDescriptionEither =
      FixedColumnDescription(thirdColumnDescriptionElem.attributes)

    (firstColumnDescriptionEither,
      secondColumnDescriptionEither,
      thirdColumnDescriptionEither) match {
      case (Right(firstColumnDescription),
      Right(secondColumnDescription),
      Right(thirdColumnDescription)) =>
        val columnDescriptionList = List(firstColumnDescription,
          secondColumnDescription,
          thirdColumnDescription)
        val fixedRowDivider = FixedRowDivider(columnDescriptionList)
        fixedRowDivider.usableDuringValidation shouldBe true
      case _ => fail
    }
  }

  it should
    "return false if all columns should not be used during validation and have all checks deactivated" in {
    val firstColumnDescriptionElem = <ColumnDescription
      label="firstColumn"
      startsAt="1"
      endsAt="3"
      checkColumnValueExists="false"
      />
    val secondColumnDescriptionElem = <ColumnDescription
      label="secondColumn"
      startsAt="4"
      endsAt="7"
      checkColumnValueExists="false"
      />

    val thirdColumnDescriptionElem = <ColumnDescription
      label="thirdColumn"
      startsAt="8"
      endsAt="12"
      checkColumnValueExists="false"
      />
    val firstColumnDescriptionEither =
      FixedColumnDescription(firstColumnDescriptionElem.attributes)
    val secondColumnDescriptionEither =
      FixedColumnDescription(secondColumnDescriptionElem.attributes)
    val thirdColumnDescriptionEither =
      FixedColumnDescription(thirdColumnDescriptionElem.attributes)

    (firstColumnDescriptionEither,
      secondColumnDescriptionEither,
      thirdColumnDescriptionEither) match {
      case (Right(firstColumnDescription),
      Right(secondColumnDescription),
      Right(thirdColumnDescription)) =>
        val columnDescriptionList = List(firstColumnDescription,
          secondColumnDescription,
          thirdColumnDescription)
        val fixedRowDivider = FixedRowDivider(columnDescriptionList)
        fixedRowDivider.usableDuringValidation shouldBe false
      case _ => fail
    }
  }

  it should
    "return false if rowdevider contains no column description" in {
        val columnDescriptionList = Nil
        val fixedRowDivider = FixedRowDivider(columnDescriptionList)
        fixedRowDivider.usableDuringValidation shouldBe false
  }

  "FixedRowDivider.equals" should "return false if passed an object of an other type" in {
    val columnDescriptionList = Nil
    val fixedRowDivider = FixedRowDivider(columnDescriptionList)
    fixedRowDivider.equals("string") shouldBe false
  }

  it should "return true if it has the same columnDescriptions List" in {
    val columnDescription = List(
      FixedColumnDescription(
        ColumnDescriptionMetaData("", "", ShouldUseDuring(false, false, false)),
        FixedPosition(1, Some(2), None).right.get,
        ValueMapper(Nil),
        CheckColumnValue(Nil)
      )
    )

    val firstFixedRowDivider = FixedRowDivider(columnDescription)
    val secondFixedRowDivider = FixedRowDivider(columnDescription)
    firstFixedRowDivider.equals(secondFixedRowDivider) shouldBe true
  }

  it should "return false if two column descriptions list are different" in {
    val firstColumnDescriptionList = Nil
    val secondColumnDescriptionList = List(
      FixedColumnDescription(
        ColumnDescriptionMetaData("", "", ShouldUseDuring(false, false, false)),
        FixedPosition(1, Some(2), None).right.get,
        ValueMapper(Nil),
        CheckColumnValue(Nil)
      )
    )
    val firstFixedRowDivider = FixedRowDivider(firstColumnDescriptionList)
    val secondFixedRowDivider = FixedRowDivider(secondColumnDescriptionList)
    firstFixedRowDivider.equals(secondFixedRowDivider) shouldBe false
  }

}
