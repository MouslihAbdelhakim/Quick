package com.scalableQuality.quick.core.fileComponentDescriptions

import com.scalableQuality.quick.mantle.parsing.{LiteralDelimiter, RawRow}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class OrderedRowDescriptionTest
    extends FlatSpec
    with Matchers
    with GeneratorDrivenPropertyChecks {

  "Fixed OrderedRowDescription.validationSignatureOf" should "return the signature of all 3 validation columns" in {
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
        val orderedRowDescription =
          OrderedRowDescription(fixedRowDivider, "label")

        val expectedSignature = List(
          Some(List(-116, 54, 9, -51, -17, 108, -101, -69, -70, 113, 67, -10,
            -19, 108, 97, -93, -36, 65, 99, 40)))

        orderedRowDescription.validationSignatureOf(rawRow) shouldBe expectedSignature
      case _ => fail
    }
  }

  it should "return the validation signature while ignoring leading and trailing white for each column" in {
    val firstRawRow = RawRow("First      Second           Third ", 1)
    val secondRawRow = RawRow("    First  Second       Third     ", 1)

    val firstColumnDescriptionElem = <ColumnDescription
      label="FirstColumn"
      startsAt="1"
      endsAt="11"
      useDuringValidation="true"
      trimComparisonValue="true"
      />
    val secondColumnDescriptionElem = <ColumnDescription
      label="SecondColumn"
      startsAt="12"
      endsAt="23"
      useDuringValidation="true"
      trimComparisonValue="true"
      />

    val thirdColumnDescriptionElem = <ColumnDescription
      label="ThirdColumn"
      startsAt="24"
      endsAt="34"
      useDuringValidation="true"
      trimComparisonValue="true"
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
        val orderedRowDescription =
          OrderedRowDescription(fixedRowDivider, "label")

        orderedRowDescription.validationSignatureOf(firstRawRow) shouldBe
          orderedRowDescription.validationSignatureOf(secondRawRow)
      case _ => fail
    }
  }

  it should "return the validation signature while ignoring the case of the two rows" in {
    val firstRawRow = RawRow("FirstColumnSecondColumnThirdColumn", 1)
    val secondRawRow = RawRow("FIRSTCOLUMNSECONDCOLUMNTHIRDCOLUMN", 1)

    val firstColumnDescriptionElem = <ColumnDescription
      label="FirstColumn"
      startsAt="1"
      endsAt="11"
      useDuringValidation="true"
      ignoreComparisonValueCase="true"
      />
    val secondColumnDescriptionElem = <ColumnDescription
      label="SecondColumn"
      startsAt="12"
      endsAt="23"
      useDuringValidation="true"
      ignoreComparisonValueCase="true"
      />

    val thirdColumnDescriptionElem = <ColumnDescription
      label="ThirdColumn"
      startsAt="24"
      endsAt="34"
      useDuringValidation="true"
      ignoreComparisonValueCase="true"
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
        val orderedRowDescription =
          OrderedRowDescription(fixedRowDivider, "label")

        orderedRowDescription.validationSignatureOf(firstRawRow) shouldBe
          orderedRowDescription.validationSignatureOf(secondRawRow)
      case _ => fail
    }
  }

  it should "return the validation signature while ignoring leading and trailing white spcae and the case for each column" in {
    val firstRawRow = RawRow("First      Second           Third ", 1)
    val secondRawRow = RawRow("    FIRST  SECOND       THIRD     ", 1)

    val firstColumnDescriptionElem = <ColumnDescription
      label="FirstColumn"
      startsAt="1"
      endsAt="11"
      useDuringValidation="true"
      trimComparisonValue="true"
      ignoreComparisonValueCase="true"
      />
    val secondColumnDescriptionElem = <ColumnDescription
      label="SecondColumn"
      startsAt="12"
      endsAt="23"
      useDuringValidation="true"
      trimComparisonValue="true"
      ignoreComparisonValueCase="true"
      />

    val thirdColumnDescriptionElem = <ColumnDescription
      label="ThirdColumn"
      startsAt="24"
      endsAt="34"
      useDuringValidation="true"
      trimComparisonValue="true"
      ignoreComparisonValueCase="true"
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
        val orderedRowDescription =
          OrderedRowDescription(fixedRowDivider, "label")

        orderedRowDescription.validationSignatureOf(firstRawRow) shouldBe
          orderedRowDescription.validationSignatureOf(secondRawRow)
      case _ => fail
    }
  }

  it should "return the validation signature of only validation columns" in {
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
        val orderedRowDescription =
          OrderedRowDescription(fixedRowDivider, "label")

        val expectedSignature = List(
          Some(List(97, -35, -42, -100, 114, 86, 61, 15, 103, -92, -59, 0, -115,
            21, -4, -44, 39, -93, 90, -117)))

        orderedRowDescription.validationSignatureOf(rawRow) shouldBe expectedSignature
      case _ => fail
    }
  }

  it should "the validation signature should include None for every validation column that does not exist" in {
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
        val orderedRowDescription =
          OrderedRowDescription(fixedRowDivider, "label")

        val expectedSignature =
          List(Some(
                 List(-34, 6, -107, 11, -85, -124, -114, -75, 104, -5, -21, -57,
                   26, 27, 9, 38, -21, 44, -79, 94)),
               None,
               None)

        orderedRowDescription.validationSignatureOf(rawRow) shouldBe expectedSignature
      case _ => fail
    }
  }

  "Fixed OrderedRowDescription.matchingSignatureOf" should "return the matching signature of all 3 matching columns" in {
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
        val orderedRowDescription =
          OrderedRowDescription(fixedRowDivider, "label")

        val expectedSignature = List(
          Some(List(-116, 54, 9, -51, -17, 108, -101, -69, -70, 113, 67, -10,
            -19, 108, 97, -93, -36, 65, 99, 40)))

        orderedRowDescription.matchingSignatureOf(rawRow) shouldBe expectedSignature
      case _ => fail
    }
  }

  it should "return the matching signature while ignoring leading and trailing white for each column" in {
    val firstRawRow = RawRow("First      Second           Third ", 1)
    val secondRawRow = RawRow("    First  Second       Third     ", 1)

    val firstColumnDescriptionElem = <ColumnDescription
      label="FirstColumn"
      startsAt="1"
      endsAt="11"
      useDuringMatching="true"
      trimComparisonValue="true"
      />
    val secondColumnDescriptionElem = <ColumnDescription
      label="SecondColumn"
      startsAt="12"
      endsAt="23"
      useDuringMatching="true"
      trimComparisonValue="true"
      />

    val thirdColumnDescriptionElem = <ColumnDescription
      label="ThirdColumn"
      startsAt="24"
      endsAt="34"
      useDuringMatching="true"
      trimComparisonValue="true"
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
        val orderedRowDescription =
          OrderedRowDescription(fixedRowDivider, "label")

        orderedRowDescription.matchingSignatureOf(firstRawRow) shouldBe
          orderedRowDescription.matchingSignatureOf(secondRawRow)
      case _ => fail
    }
  }

  it should "return the signature while ignoring the case of the two rows" in {
    val firstRawRow = RawRow("FirstColumnSecondColumnThirdColumn", 1)
    val secondRawRow = RawRow("FIRSTCOLUMNSECONDCOLUMNTHIRDCOLUMN", 1)

    val firstColumnDescriptionElem = <ColumnDescription
      label="FirstColumn"
      startsAt="1"
      endsAt="11"
      useDuringMatching="true"
      ignoreComparisonValueCase="true"
      />
    val secondColumnDescriptionElem = <ColumnDescription
      label="SecondColumn"
      startsAt="12"
      endsAt="23"
      useDuringMatching="true"
      ignoreComparisonValueCase="true"
      />

    val thirdColumnDescriptionElem = <ColumnDescription
      label="ThirdColumn"
      startsAt="24"
      endsAt="34"
      useDuringMatching="true"
      ignoreComparisonValueCase="true"
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
        val orderedRowDescription =
          OrderedRowDescription(fixedRowDivider, "label")

        orderedRowDescription.matchingSignatureOf(firstRawRow) shouldBe
          orderedRowDescription.matchingSignatureOf(secondRawRow)
      case _ => fail
    }
  }

  it should "return the signature while ignoring leading and trailing white spcae and the case for each column" in {
    val firstRawRow = RawRow("First      Second           Third ", 1)
    val secondRawRow = RawRow("    FIRST  SECOND       THIRD     ", 1)

    val firstColumnDescriptionElem = <ColumnDescription
      label="FirstColumn"
      startsAt="1"
      endsAt="11"
      useDuringMatching="true"
      trimComparisonValue="true"
      ignoreComparisonValueCase="true"
      />
    val secondColumnDescriptionElem = <ColumnDescription
      label="SecondColumn"
      startsAt="12"
      endsAt="23"
      useDuringMatching="true"
      trimComparisonValue="true"
      ignoreComparisonValueCase="true"
      />

    val thirdColumnDescriptionElem = <ColumnDescription
      label="ThirdColumn"
      startsAt="24"
      endsAt="34"
      useDuringMatching="true"
      trimComparisonValue="true"
      ignoreComparisonValueCase="true"
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
        val orderedRowDescription =
          OrderedRowDescription(fixedRowDivider, "label")

        orderedRowDescription.matchingSignatureOf(firstRawRow) shouldBe
          orderedRowDescription.matchingSignatureOf(secondRawRow)
      case _ => fail
    }
  }

  it should "return the signature of only matching columns" in {
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
        val orderedRowDescription =
          OrderedRowDescription(fixedRowDivider, "label")

        val expectedSignature = List(
          Some(List(97, -35, -42, -100, 114, 86, 61, 15, 103, -92, -59, 0, -115,
            21, -4, -44, 39, -93, 90, -117)))

        orderedRowDescription.matchingSignatureOf(rawRow) shouldBe expectedSignature
      case _ => fail
    }
  }

  it should "the signature should include None for every matching column that does not exist" in {
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
        val orderedRowDescription =
          OrderedRowDescription(fixedRowDivider, "label")

        val expectedSignature =
          List(Some(
                 List(-34, 6, -107, 11, -85, -124, -114, -75, 104, -5, -21, -57,
                   26, 27, 9, 38, -21, 44, -79, 94)),
               None,
               None)

        orderedRowDescription.matchingSignatureOf(rawRow) shouldBe expectedSignature
      case _ => fail
    }
  }

  "Delimited OrderedRowDescription.validationSignatureOf" should "return the signature of all 3 validation columns" in {
    val rawRow = RawRow("FirstColumn;SecondColumn;ThirdColumn", 1)
    val firstColumnDescriptionElem = <ColumnDescription
      label="FirstColumn"
      position="1"
      useDuringValidation="true"
      />
    val secondColumnDescriptionElem = <ColumnDescription
      label="SecondColumn"
      position="2"
      useDuringValidation="true"
      />

    val thirdColumnDescriptionElem = <ColumnDescription
      label="ThirdColumn"
      position="3"
      useDuringValidation="true"
      />

    val firstColumnDescriptionEither =
      DelimitedColumnDescription(firstColumnDescriptionElem.attributes)
    val secondColumnDescriptionEither =
      DelimitedColumnDescription(secondColumnDescriptionElem.attributes)
    val thirdColumnDescriptionEither =
      DelimitedColumnDescription(thirdColumnDescriptionElem.attributes)
    val delimitedEither = LiteralDelimiter(";")
    (firstColumnDescriptionEither,
     secondColumnDescriptionEither,
     thirdColumnDescriptionEither,
     delimitedEither) match {
      case (Right(firstColumnDescription),
            Right(secondColumnDescription),
            Right(thirdColumnDescription),
            Right(delimited)) =>
        val columnDescriptionList = List(firstColumnDescription,
                                         secondColumnDescription,
                                         thirdColumnDescription)
        val delimitedRowDivider =
          DelimitedRowDivider(columnDescriptionList, delimited)
        val orderedRowDescription =
          OrderedRowDescription(delimitedRowDivider, "label")

        val expectedSignature = List(
          Some(List(-116, 54, 9, -51, -17, 108, -101, -69, -70, 113, 67, -10,
            -19, 108, 97, -93, -36, 65, 99, 40)))

        orderedRowDescription.validationSignatureOf(rawRow) shouldBe expectedSignature
      case _ => fail
    }
  }

  it should "return the validation signature while ignoring leading and trailing white for each column" in {
    val firstRawRow = RawRow("First      ;Second       ;    Third ", 1)
    val secondRawRow = RawRow("    First  ;Second       ;Third     ", 1)

    val firstColumnDescriptionElem = <ColumnDescription
      label="FirstColumn"
      position="1"
      useDuringValidation="true"
      trimComparisonValue="true"
      />
    val secondColumnDescriptionElem = <ColumnDescription
      label="SecondColumn"
      position="2"
      useDuringValidation="true"
      trimComparisonValue="true"
      />

    val thirdColumnDescriptionElem = <ColumnDescription
      label="ThirdColumn"
      position="3"
      useDuringValidation="true"
      trimComparisonValue="true"
      />

    val firstColumnDescriptionEither =
      DelimitedColumnDescription(firstColumnDescriptionElem.attributes)
    val secondColumnDescriptionEither =
      DelimitedColumnDescription(secondColumnDescriptionElem.attributes)
    val thirdColumnDescriptionEither =
      DelimitedColumnDescription(thirdColumnDescriptionElem.attributes)
    val delimitedEither = LiteralDelimiter(";")

    (firstColumnDescriptionEither,
     secondColumnDescriptionEither,
     thirdColumnDescriptionEither,
     delimitedEither) match {
      case (Right(firstColumnDescription),
            Right(secondColumnDescription),
            Right(thirdColumnDescription),
            Right(delimited)) =>
        val columnDescriptionList = List(firstColumnDescription,
                                         secondColumnDescription,
                                         thirdColumnDescription)
        val delimitedRowDivider =
          DelimitedRowDivider(columnDescriptionList, delimited)
        val orderedRowDescription =
          OrderedRowDescription(delimitedRowDivider, "label")

        orderedRowDescription.validationSignatureOf(firstRawRow) shouldBe
          orderedRowDescription.validationSignatureOf(secondRawRow)
      case _ => fail
    }
  }

  it should "return the validation signature while ignoring the case of the two rows" in {
    val firstRawRow = RawRow("FirstColumn;SecondColumn;ThirdColumn", 1)
    val secondRawRow = RawRow("FIRSTCOLUMN;SECONDCOLUMN;THIRDCOLUMN", 1)

    val firstColumnDescriptionElem = <ColumnDescription
      label="FirstColumn"
      position="1"
      useDuringValidation="true"
      ignoreComparisonValueCase="true"
      />
    val secondColumnDescriptionElem = <ColumnDescription
      label="SecondColumn"
      position="2"
      useDuringValidation="true"
      ignoreComparisonValueCase="true"
      />

    val thirdColumnDescriptionElem = <ColumnDescription
      label="ThirdColumn"
      position="3"
      useDuringValidation="true"
      ignoreComparisonValueCase="true"
      />

    val firstColumnDescriptionEither =
      DelimitedColumnDescription(firstColumnDescriptionElem.attributes)
    val secondColumnDescriptionEither =
      DelimitedColumnDescription(secondColumnDescriptionElem.attributes)
    val thirdColumnDescriptionEither =
      DelimitedColumnDescription(thirdColumnDescriptionElem.attributes)
    val delimitedEither = LiteralDelimiter(";")

    (firstColumnDescriptionEither,
     secondColumnDescriptionEither,
     thirdColumnDescriptionEither,
     delimitedEither) match {
      case (Right(firstColumnDescription),
            Right(secondColumnDescription),
            Right(thirdColumnDescription),
            Right(delimited)) =>
        val columnDescriptionList = List(firstColumnDescription,
                                         secondColumnDescription,
                                         thirdColumnDescription)
        val delimitedRowDivider =
          DelimitedRowDivider(columnDescriptionList, delimited)
        val orderedRowDescription =
          OrderedRowDescription(delimitedRowDivider, "label")

        orderedRowDescription.validationSignatureOf(firstRawRow) shouldBe
          orderedRowDescription.validationSignatureOf(secondRawRow)
      case _ => fail
    }
  }

  it should "return the validation signature while ignoring leading and trailing white spcae and the case for each column" in {
    val firstRawRow = RawRow("First      ;Second       ;    Third ", 1)
    val secondRawRow = RawRow("    FIRST  ;SECOND       ;THIRD     ", 1)

    val firstColumnDescriptionElem = <ColumnDescription
      label="FirstColumn"
      position="1"
      useDuringValidation="true"
      trimComparisonValue="true"
      ignoreComparisonValueCase="true"
      />
    val secondColumnDescriptionElem = <ColumnDescription
      label="SecondColumn"
      position="2"
      useDuringValidation="true"
      trimComparisonValue="true"
      ignoreComparisonValueCase="true"
      />

    val thirdColumnDescriptionElem = <ColumnDescription
      label="ThirdColumn"
      position="3"
      useDuringValidation="true"
      trimComparisonValue="true"
      ignoreComparisonValueCase="true"
      />

    val firstColumnDescriptionEither =
      DelimitedColumnDescription(firstColumnDescriptionElem.attributes)
    val secondColumnDescriptionEither =
      DelimitedColumnDescription(secondColumnDescriptionElem.attributes)
    val thirdColumnDescriptionEither =
      DelimitedColumnDescription(thirdColumnDescriptionElem.attributes)
    val delimitedEither = LiteralDelimiter(";")

    (firstColumnDescriptionEither,
     secondColumnDescriptionEither,
     thirdColumnDescriptionEither,
     delimitedEither) match {
      case (Right(firstColumnDescription),
            Right(secondColumnDescription),
            Right(thirdColumnDescription),
            Right(delimited)) =>
        val columnDescriptionList = List(firstColumnDescription,
                                         secondColumnDescription,
                                         thirdColumnDescription)
        val delimitedRowDivider =
          DelimitedRowDivider(columnDescriptionList, delimited)
        val orderedRowDescription =
          OrderedRowDescription(delimitedRowDivider, "label")

        orderedRowDescription.validationSignatureOf(firstRawRow) shouldBe
          orderedRowDescription.validationSignatureOf(secondRawRow)
      case _ => fail
    }
  }

  it should "return the validation signature of only validation columns" in {
    val rawRow = RawRow("FirstColumn;SecondColumn;ThirdColumn", 1)
    val firstColumnDescriptionElem = <ColumnDescription
      label="FirstColumn"
      position="1"
      useDuringValidation="true"
      />
    val secondColumnDescriptionElem = <ColumnDescription
      label="SecondColumn"
      position="2"
      useDuringValidation="false"
      />

    val thirdColumnDescriptionElem = <ColumnDescription
      label="ThirdColumn"
      position="3"
      useDuringValidation="true"
      />

    val firstColumnDescriptionEither =
      DelimitedColumnDescription(firstColumnDescriptionElem.attributes)
    val secondColumnDescriptionEither =
      DelimitedColumnDescription(secondColumnDescriptionElem.attributes)
    val thirdColumnDescriptionEither =
      DelimitedColumnDescription(thirdColumnDescriptionElem.attributes)
    val delimitedEither = LiteralDelimiter(";")

    (firstColumnDescriptionEither,
     secondColumnDescriptionEither,
     thirdColumnDescriptionEither,
     delimitedEither) match {
      case (Right(firstColumnDescription),
            Right(secondColumnDescription),
            Right(thirdColumnDescription),
            Right(delimited)) =>
        val columnDescriptionList = List(firstColumnDescription,
                                         secondColumnDescription,
                                         thirdColumnDescription)
        val delimitedRowDivider =
          DelimitedRowDivider(columnDescriptionList, delimited)
        val orderedRowDescription =
          OrderedRowDescription(delimitedRowDivider, "label")

        val expectedSignature = List(
          Some(List(97, -35, -42, -100, 114, 86, 61, 15, 103, -92, -59, 0, -115,
            21, -4, -44, 39, -93, 90, -117)))

        orderedRowDescription.validationSignatureOf(rawRow) shouldBe expectedSignature
      case _ => fail
    }
  }

  it should "the validation signature should include None for every validation column that does not exist" in {
    val rawRow = RawRow("FirstColumn", 1)
    val firstColumnDescriptionElem = <ColumnDescription
      label="FirstColumn"
      position="1"
      useDuringValidation="true"
      />
    val secondColumnDescriptionElem = <ColumnDescription
      label="SecondColumn"
      position="2"
      useDuringValidation="true"
      />

    val thirdColumnDescriptionElem = <ColumnDescription
      label="ThirdColumn"
      position="3"
      useDuringValidation="true"
      />

    val firstColumnDescriptionEither =
      DelimitedColumnDescription(firstColumnDescriptionElem.attributes)
    val secondColumnDescriptionEither =
      DelimitedColumnDescription(secondColumnDescriptionElem.attributes)
    val thirdColumnDescriptionEither =
      DelimitedColumnDescription(thirdColumnDescriptionElem.attributes)
    val delimitedEither = LiteralDelimiter(";")

    (firstColumnDescriptionEither,
     secondColumnDescriptionEither,
     thirdColumnDescriptionEither,
     delimitedEither) match {
      case (Right(firstColumnDescription),
            Right(secondColumnDescription),
            Right(thirdColumnDescription),
            Right(delimited)) =>
        val columnDescriptionList = List(firstColumnDescription,
                                         secondColumnDescription,
                                         thirdColumnDescription)
        val delimitedRowDivider =
          DelimitedRowDivider(columnDescriptionList, delimited)
        val orderedRowDescription =
          OrderedRowDescription(delimitedRowDivider, "label")

        val expectedSignature =
          List(Some(
                 List(-34, 6, -107, 11, -85, -124, -114, -75, 104, -5, -21, -57,
                   26, 27, 9, 38, -21, 44, -79, 94)),
               None,
               None)

        orderedRowDescription.validationSignatureOf(rawRow) shouldBe expectedSignature
      case _ => fail
    }
  }

  "Delimited OrderedRowDescription.matchingSignatureOf" should "return the matching signature of all 3 matching columns" in {
    val rawRow = RawRow("FirstColumn;SecondColumn;ThirdColumn", 1)
    val firstColumnDescriptionElem = <ColumnDescription
      label="FirstColumn"
      position="1"
      useDuringMatching="true"
      />
    val secondColumnDescriptionElem = <ColumnDescription
      label="SecondColumn"
      position="2"
      useDuringMatching="true"
      />

    val thirdColumnDescriptionElem = <ColumnDescription
      label="ThirdColumn"
      position="3"
      useDuringMatching="true"
      />

    val firstColumnDescriptionEither =
      DelimitedColumnDescription(firstColumnDescriptionElem.attributes)
    val secondColumnDescriptionEither =
      DelimitedColumnDescription(secondColumnDescriptionElem.attributes)
    val thirdColumnDescriptionEither =
      DelimitedColumnDescription(thirdColumnDescriptionElem.attributes)
    val delimitedEither = LiteralDelimiter(";")

    (firstColumnDescriptionEither,
     secondColumnDescriptionEither,
     thirdColumnDescriptionEither,
     delimitedEither) match {
      case (Right(firstColumnDescription),
            Right(secondColumnDescription),
            Right(thirdColumnDescription),
            Right(delimited)) =>
        val columnDescriptionList = List(firstColumnDescription,
                                         secondColumnDescription,
                                         thirdColumnDescription)
        val delimitedRowDivider =
          DelimitedRowDivider(columnDescriptionList, delimited)
        val orderedRowDescription =
          OrderedRowDescription(delimitedRowDivider, "label")

        val expectedSignature = List(
          Some(List(-116, 54, 9, -51, -17, 108, -101, -69, -70, 113, 67, -10,
            -19, 108, 97, -93, -36, 65, 99, 40)))

        orderedRowDescription.matchingSignatureOf(rawRow) shouldBe expectedSignature
      case _ => fail
    }
  }

  it should "return the matching signature while ignoring leading and trailing white for each column" in {
    val firstRawRow = RawRow("First      ;Second      ;     Third ", 1)
    val secondRawRow = RawRow("    First  ;Second      ;Third     ", 1)

    val firstColumnDescriptionElem = <ColumnDescription
      label="FirstColumn"
      position="1"
      useDuringMatching="true"
      trimComparisonValue="true"
      />
    val secondColumnDescriptionElem = <ColumnDescription
      label="SecondColumn"
      position="2"
      useDuringMatching="true"
      trimComparisonValue="true"
      />

    val thirdColumnDescriptionElem = <ColumnDescription
      label="ThirdColumn"
      position="3"
      useDuringMatching="true"
      trimComparisonValue="true"
      />

    val firstColumnDescriptionEither =
      DelimitedColumnDescription(firstColumnDescriptionElem.attributes)
    val secondColumnDescriptionEither =
      DelimitedColumnDescription(secondColumnDescriptionElem.attributes)
    val thirdColumnDescriptionEither =
      DelimitedColumnDescription(thirdColumnDescriptionElem.attributes)
    val delimitedEither = LiteralDelimiter(";")

    (firstColumnDescriptionEither,
     secondColumnDescriptionEither,
     thirdColumnDescriptionEither,
     delimitedEither) match {
      case (Right(firstColumnDescription),
            Right(secondColumnDescription),
            Right(thirdColumnDescription),
            Right(delimited)) =>
        val columnDescriptionList = List(firstColumnDescription,
                                         secondColumnDescription,
                                         thirdColumnDescription)
        val delimitedRowDivider =
          DelimitedRowDivider(columnDescriptionList, delimited)
        val orderedRowDescription =
          OrderedRowDescription(delimitedRowDivider, "label")

        orderedRowDescription.matchingSignatureOf(firstRawRow) shouldBe
          orderedRowDescription.matchingSignatureOf(secondRawRow)
      case _ => fail
    }
  }

  it should "return the signature while ignoring the case of the two rows" in {
    val firstRawRow = RawRow("FirstColumn;SecondColumn;ThirdColumn", 1)
    val secondRawRow = RawRow("FIRSTCOLUMN;SECONDCOLUMN;THIRDCOLUMN", 1)

    val firstColumnDescriptionElem = <ColumnDescription
      label="FirstColumn"
      position="1"
      useDuringMatching="true"
      ignoreComparisonValueCase="true"
      />
    val secondColumnDescriptionElem = <ColumnDescription
      label="SecondColumn"
      position="2"
      useDuringMatching="true"
      ignoreComparisonValueCase="true"
      />

    val thirdColumnDescriptionElem = <ColumnDescription
      label="ThirdColumn"
      position="3"
      useDuringMatching="true"
      ignoreComparisonValueCase="true"
      />

    val firstColumnDescriptionEither =
      DelimitedColumnDescription(firstColumnDescriptionElem.attributes)
    val secondColumnDescriptionEither =
      DelimitedColumnDescription(secondColumnDescriptionElem.attributes)
    val thirdColumnDescriptionEither =
      DelimitedColumnDescription(thirdColumnDescriptionElem.attributes)
    val delimitedEither = LiteralDelimiter(";")

    (firstColumnDescriptionEither,
     secondColumnDescriptionEither,
     thirdColumnDescriptionEither,
     delimitedEither) match {
      case (Right(firstColumnDescription),
            Right(secondColumnDescription),
            Right(thirdColumnDescription),
            Right(delimited)) =>
        val columnDescriptionList = List(firstColumnDescription,
                                         secondColumnDescription,
                                         thirdColumnDescription)
        val delimitedRowDivider =
          DelimitedRowDivider(columnDescriptionList, delimited)
        val orderedRowDescription =
          OrderedRowDescription(delimitedRowDivider, "label")

        orderedRowDescription.matchingSignatureOf(firstRawRow) shouldBe
          orderedRowDescription.matchingSignatureOf(secondRawRow)
      case _ => fail
    }
  }

  it should "return the signature while ignoring leading and trailing white spcae and the case for each column" in {
    val firstRawRow = RawRow("First      ;Second       ;    Third ", 1)
    val secondRawRow = RawRow("    FIRST  ;SECOND       ;THIRD     ", 1)

    val firstColumnDescriptionElem = <ColumnDescription
      label="FirstColumn"
      position="1"
      useDuringMatching="true"
      trimComparisonValue="true"
      ignoreComparisonValueCase="true"
      />
    val secondColumnDescriptionElem = <ColumnDescription
      label="SecondColumn"
      position="2"
      useDuringMatching="true"
      trimComparisonValue="true"
      ignoreComparisonValueCase="true"
      />

    val thirdColumnDescriptionElem = <ColumnDescription
      label="ThirdColumn"
      position="3"
      useDuringMatching="true"
      trimComparisonValue="true"
      ignoreComparisonValueCase="true"
      />

    val firstColumnDescriptionEither =
      DelimitedColumnDescription(firstColumnDescriptionElem.attributes)
    val secondColumnDescriptionEither =
      DelimitedColumnDescription(secondColumnDescriptionElem.attributes)
    val thirdColumnDescriptionEither =
      DelimitedColumnDescription(thirdColumnDescriptionElem.attributes)
    val delimitedEither = LiteralDelimiter(";")

    (firstColumnDescriptionEither,
     secondColumnDescriptionEither,
     thirdColumnDescriptionEither,
     delimitedEither) match {
      case (Right(firstColumnDescription),
            Right(secondColumnDescription),
            Right(thirdColumnDescription),
            Right(delimited)) =>
        val columnDescriptionList = List(firstColumnDescription,
                                         secondColumnDescription,
                                         thirdColumnDescription)
        val delimitedRowDivider =
          DelimitedRowDivider(columnDescriptionList, delimited)
        val orderedRowDescription =
          OrderedRowDescription(delimitedRowDivider, "label")

        orderedRowDescription.matchingSignatureOf(firstRawRow) shouldBe
          orderedRowDescription.matchingSignatureOf(secondRawRow)
      case _ => fail
    }
  }

  it should "return the signature of only matching columns" in {
    val rawRow = RawRow("FirstColumn;SecondColumn;ThirdColumn", 1)
    val firstColumnDescriptionElem = <ColumnDescription
      label="FirstColumn"
      position="1"
      useDuringMatching="true"
      />
    val secondColumnDescriptionElem = <ColumnDescription
      label="SecondColumn"
      position="2"
      useDuringMatching="false"
      />

    val thirdColumnDescriptionElem = <ColumnDescription
      label="ThirdColumn"
      position="3"
      useDuringMatching="true"
      />

    val firstColumnDescriptionEither =
      DelimitedColumnDescription(firstColumnDescriptionElem.attributes)
    val secondColumnDescriptionEither =
      DelimitedColumnDescription(secondColumnDescriptionElem.attributes)
    val thirdColumnDescriptionEither =
      DelimitedColumnDescription(thirdColumnDescriptionElem.attributes)
    val delimitedEither = LiteralDelimiter(";")

    (firstColumnDescriptionEither,
     secondColumnDescriptionEither,
     thirdColumnDescriptionEither,
     delimitedEither) match {
      case (Right(firstColumnDescription),
            Right(secondColumnDescription),
            Right(thirdColumnDescription),
            Right(delimited)) =>
        val columnDescriptionList = List(firstColumnDescription,
                                         secondColumnDescription,
                                         thirdColumnDescription)
        val delimitedRowDivider =
          DelimitedRowDivider(columnDescriptionList, delimited)
        val orderedRowDescription =
          OrderedRowDescription(delimitedRowDivider, "label")

        val expectedSignature = List(
          Some(List(97, -35, -42, -100, 114, 86, 61, 15, 103, -92, -59, 0, -115,
            21, -4, -44, 39, -93, 90, -117)))

        orderedRowDescription.matchingSignatureOf(rawRow) shouldBe expectedSignature
      case _ => fail
    }
  }

  it should "the signature should include None for every matching column that does not exist" in {
    val rawRow = RawRow("FirstColumn", 1)
    val firstColumnDescriptionElem = <ColumnDescription
      label="FirstColumn"
      position="1"
      useDuringMatching="true"
      />
    val secondColumnDescriptionElem = <ColumnDescription
      label="SecondColumn"
      position="2"
      useDuringMatching="true"
      />

    val thirdColumnDescriptionElem = <ColumnDescription
      label="ThirdColumn"
      position="3"
      useDuringMatching="true"
      />

    val firstColumnDescriptionEither =
      DelimitedColumnDescription(firstColumnDescriptionElem.attributes)
    val secondColumnDescriptionEither =
      DelimitedColumnDescription(secondColumnDescriptionElem.attributes)
    val thirdColumnDescriptionEither =
      DelimitedColumnDescription(thirdColumnDescriptionElem.attributes)
    val delimitedEither = LiteralDelimiter(";")

    (firstColumnDescriptionEither,
     secondColumnDescriptionEither,
     thirdColumnDescriptionEither,
     delimitedEither) match {
      case (Right(firstColumnDescription),
            Right(secondColumnDescription),
            Right(thirdColumnDescription),
            Right(delimited)) =>
        val columnDescriptionList = List(firstColumnDescription,
                                         secondColumnDescription,
                                         thirdColumnDescription)
        val delimitedRowDivider =
          DelimitedRowDivider(columnDescriptionList, delimited)
        val orderedRowDescription =
          OrderedRowDescription(delimitedRowDivider, "label")

        val expectedSignature =
          List(Some(
                 List(-34, 6, -107, 11, -85, -124, -114, -75, 104, -5, -21, -57,
                   26, 27, 9, 38, -21, 44, -79, 94)),
               None,
               None)

        orderedRowDescription.matchingSignatureOf(rawRow) shouldBe expectedSignature
      case _ => fail
    }
  }

  "OrderedRowDescription.check" should "check fixed length column value exists by default" in {
    val rawRow = RawRow("FirstColumnSecond", 1)
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
        val orderedRowDescription =
          OrderedRowDescription(fixedRowDivider, "label")

        val expectedCheckResult = false

        orderedRowDescription.check(rawRow) shouldBe expectedCheckResult
      case _ => fail
    }
  }

  it should "check delimited column value exists by default" in {
    val rawRow = RawRow("FirstColumn;Second", 1)
    val firstColumnDescriptionElem = <ColumnDescription
      label="FirstColumn"
      position="1"
      useDuringValidation="true"
      />
    val secondColumnDescriptionElem = <ColumnDescription
      label="SecondColumn"
      position="2"
      useDuringValidation="true"
      />

    val thirdColumnDescriptionElem = <ColumnDescription
      label="ThirdColumn"
      position="3"
      useDuringValidation="true"
      />

    val firstColumnDescriptionEither =
      DelimitedColumnDescription(firstColumnDescriptionElem.attributes)
    val secondColumnDescriptionEither =
      DelimitedColumnDescription(secondColumnDescriptionElem.attributes)
    val thirdColumnDescriptionEither =
      DelimitedColumnDescription(thirdColumnDescriptionElem.attributes)
    val delimiterEither = LiteralDelimiter(";")

    (firstColumnDescriptionEither,
      secondColumnDescriptionEither,
      thirdColumnDescriptionEither,
      delimiterEither) match {
      case (Right(firstColumnDescription),
      Right(secondColumnDescription),
      Right(thirdColumnDescription),
        Right(delimiter)) =>
        val columnDescriptionList = List(firstColumnDescription,
          secondColumnDescription,
          thirdColumnDescription)
        val fixedRowDivider = DelimitedRowDivider(columnDescriptionList, delimiter)
        val orderedRowDescription =
          OrderedRowDescription(fixedRowDivider, "label")

        val expectedCheckResult = false

        orderedRowDescription.check(rawRow) shouldBe expectedCheckResult
      case _ => fail
    }
  }

  it should "return true if the first fixed length column exists" in {
    val rawRow = RawRow("FirstColumnSecondColumnThirdColumn", 1)
    val firstColumnDescriptionElem = <ColumnDescription
      label="FirstColumn"
      startsAt="1"
      endsAt="11"
      useDuringValidation="true"
      checkColumnValueExists="true"
      />
    val secondColumnDescriptionElem = <ColumnDescription
      label="SecondColumn"
      startsAt="12"
      endsAt="23"
      useDuringValidation="true"
      checkColumnValueExists="false"
      />

    val thirdColumnDescriptionElem = <ColumnDescription
      label="ThirdColumn"
      startsAt="24"
      endsAt="34"
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
        val orderedRowDescription =
          OrderedRowDescription(fixedRowDivider, "label")

        val expectedCheckResult = true

        orderedRowDescription.check(rawRow) shouldBe expectedCheckResult
      case _ => fail
    }
  }

  it should "return true if the first delimited column exists" in {
    val rawRow = RawRow("FirstColumn;SecondColumn;ThirdColumn", 1)
    val firstColumnDescriptionElem = <ColumnDescription
      label="FirstColumn"
      position="1"
      useDuringValidation="true"
      checkColumnValueExists="true"
      />
    val secondColumnDescriptionElem = <ColumnDescription
      label="SecondColumn"
      position="2"
      useDuringValidation="true"
      checkColumnValueExists="false"
      />

    val thirdColumnDescriptionElem = <ColumnDescription
      label="ThirdColumn"
      position="3"
      useDuringValidation="true"
      checkColumnValueExists="false"
      />

    val firstColumnDescriptionEither =
      DelimitedColumnDescription(firstColumnDescriptionElem.attributes)
    val secondColumnDescriptionEither =
      DelimitedColumnDescription(secondColumnDescriptionElem.attributes)
    val thirdColumnDescriptionEither =
      DelimitedColumnDescription(thirdColumnDescriptionElem.attributes)
    val delimiterEither = LiteralDelimiter(";")

    (firstColumnDescriptionEither,
      secondColumnDescriptionEither,
      thirdColumnDescriptionEither,
      delimiterEither) match {
      case (Right(firstColumnDescription),
      Right(secondColumnDescription),
      Right(thirdColumnDescription),
      Right(delimiter)) =>
        val columnDescriptionList = List(firstColumnDescription,
          secondColumnDescription,
          thirdColumnDescription)
        val fixedRowDivider = DelimitedRowDivider(columnDescriptionList, delimiter)
        val orderedRowDescription =
          OrderedRowDescription(fixedRowDivider, "label")

        val expectedCheckResult = true

        orderedRowDescription.check(rawRow) shouldBe expectedCheckResult
      case _ => fail
    }
  }

  it should "return true if the first fixed length and the last columns exist" in {
    val rawRow = RawRow("FirstColumnSecondColumnThirdColumn", 1)
    val firstColumnDescriptionElem = <ColumnDescription
      label="FirstColumn"
      startsAt="1"
      endsAt="11"
      useDuringValidation="true"
      checkColumnValueExists="true"
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
      checkColumnValueExists="true"
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
        val orderedRowDescription =
          OrderedRowDescription(fixedRowDivider, "label")

        val expectedCheckResult = true

        orderedRowDescription.check(rawRow) shouldBe expectedCheckResult
      case _ => fail
    }
  }

  it should "return true if the first delimited and the last columns exist" in {
    val rawRow = RawRow("FirstColumn;SecondColumn;ThirdColumn", 1)
    val firstColumnDescriptionElem = <ColumnDescription
      label="FirstColumn"
      position="1"
      useDuringValidation="true"
      checkColumnValueExists="true"
      />
    val secondColumnDescriptionElem = <ColumnDescription
      label="SecondColumn"
      position="2"
      useDuringValidation="true"
      checkColumnValueExists="false"
      />

    val thirdColumnDescriptionElem = <ColumnDescription
      label="ThirdColumn"
      position="3"
      useDuringValidation="true"
      checkColumnValueExists="true"
      />

    val firstColumnDescriptionEither =
      DelimitedColumnDescription(firstColumnDescriptionElem.attributes)
    val secondColumnDescriptionEither =
      DelimitedColumnDescription(secondColumnDescriptionElem.attributes)
    val thirdColumnDescriptionEither =
      DelimitedColumnDescription(thirdColumnDescriptionElem.attributes)
    val delimiterEither = LiteralDelimiter(";")

    (firstColumnDescriptionEither,
      secondColumnDescriptionEither,
      thirdColumnDescriptionEither,
      delimiterEither) match {
      case (Right(firstColumnDescription),
      Right(secondColumnDescription),
      Right(thirdColumnDescription),
      Right(delimiter)) =>
        val columnDescriptionList = List(firstColumnDescription,
          secondColumnDescription,
          thirdColumnDescription)
        val fixedRowDivider = DelimitedRowDivider(columnDescriptionList, delimiter)
        val orderedRowDescription =
          OrderedRowDescription(fixedRowDivider, "label")

        val expectedCheckResult = true

        orderedRowDescription.check(rawRow) shouldBe expectedCheckResult
      case _ => fail
    }
  }

  it should "return false if the last fixed length column does not exist" in {
    val rawRow = RawRow("FirstColumnSecondColumn", 1)
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
      checkColumnValueExists="true"
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
        val orderedRowDescription =
          OrderedRowDescription(fixedRowDivider, "label")

        val expectedCheckResult = false

        orderedRowDescription.check(rawRow) shouldBe expectedCheckResult
      case _ => fail
    }
  }

  it should "return false if the last delimited column does not exist" in {
    val rawRow = RawRow("FirstColumn;SecondColumn", 1)
    val firstColumnDescriptionElem = <ColumnDescription
      label="FirstColumn"
      position="1"
      useDuringValidation="true"
      checkColumnValueExists="false"
      />
    val secondColumnDescriptionElem = <ColumnDescription
      label="SecondColumn"
      position="2"
      useDuringValidation="true"
      checkColumnValueExists="false"
      />

    val thirdColumnDescriptionElem = <ColumnDescription
      label="ThirdColumn"
      position="3"
      useDuringValidation="true"
      checkColumnValueExists="true"
      />

    val firstColumnDescriptionEither =
      DelimitedColumnDescription(firstColumnDescriptionElem.attributes)
    val secondColumnDescriptionEither =
      DelimitedColumnDescription(secondColumnDescriptionElem.attributes)
    val thirdColumnDescriptionEither =
      DelimitedColumnDescription(thirdColumnDescriptionElem.attributes)
    val delimiterEither = LiteralDelimiter(";")

    (firstColumnDescriptionEither,
      secondColumnDescriptionEither,
      thirdColumnDescriptionEither,
      delimiterEither) match {
      case (Right(firstColumnDescription),
      Right(secondColumnDescription),
      Right(thirdColumnDescription),
      Right(delimiter)) =>
        val columnDescriptionList = List(firstColumnDescription,
          secondColumnDescription,
          thirdColumnDescription)
        val fixedRowDivider = DelimitedRowDivider(columnDescriptionList, delimiter)
        val orderedRowDescription =
          OrderedRowDescription(fixedRowDivider, "label")

        val expectedCheckResult = false

        orderedRowDescription.check(rawRow) shouldBe expectedCheckResult
      case _ => fail
    }
  }

  it should "return false if the first fixed length column exists and last column does not exist" in {
    val rawRow = RawRow("FirstColumnSecondColumn", 1)
    val firstColumnDescriptionElem = <ColumnDescription
      label="FirstColumn"
      startsAt="1"
      endsAt="11"
      useDuringValidation="true"
      checkColumnValueExists="true"
      />
    val secondColumnDescriptionElem = <ColumnDescription
      label="SecondColumn"
      startsAt="12"
      endsAt="23"
      useDuringValidation="false"
      checkColumnValueExists="false"
      />

    val thirdColumnDescriptionElem = <ColumnDescription
      label="ThirdColumn"
      startsAt="24"
      endsAt="34"
      useDuringValidation="true"
      checkColumnValueExists="true"
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
        val orderedRowDescription =
          OrderedRowDescription(fixedRowDivider, "label")

        val expectedCheckResult = false

        orderedRowDescription.check(rawRow) shouldBe expectedCheckResult
      case _ => fail
    }
  }

  it should "return false if the first delimited column exists and last column does not exist" in {
    val rawRow = RawRow("FirstColumn;SecondColumn", 1)
    val firstColumnDescriptionElem = <ColumnDescription
      label="FirstColumn"
      position="1"
      useDuringValidation="true"
      checkColumnValueExists="true"
      />
    val secondColumnDescriptionElem = <ColumnDescription
      label="SecondColumn"
      position="2"
      useDuringValidation="true"
      checkColumnValueExists="false"
      />

    val thirdColumnDescriptionElem = <ColumnDescription
      label="ThirdColumn"
      position="3"
      useDuringValidation="true"
      checkColumnValueExists="true"
      />

    val firstColumnDescriptionEither =
      DelimitedColumnDescription(firstColumnDescriptionElem.attributes)
    val secondColumnDescriptionEither =
      DelimitedColumnDescription(secondColumnDescriptionElem.attributes)
    val thirdColumnDescriptionEither =
      DelimitedColumnDescription(thirdColumnDescriptionElem.attributes)
    val delimiterEither = LiteralDelimiter(";")

    (firstColumnDescriptionEither,
      secondColumnDescriptionEither,
      thirdColumnDescriptionEither,
      delimiterEither) match {
      case (Right(firstColumnDescription),
      Right(secondColumnDescription),
      Right(thirdColumnDescription),
      Right(delimiter)) =>
        val columnDescriptionList = List(firstColumnDescription,
          secondColumnDescription,
          thirdColumnDescription)
        val fixedRowDivider = DelimitedRowDivider(columnDescriptionList, delimiter)
        val orderedRowDescription =
          OrderedRowDescription(fixedRowDivider, "label")

        val expectedCheckResult = false

        orderedRowDescription.check(rawRow) shouldBe expectedCheckResult
      case _ => fail
    }
  }

  it should "return true if the first fixed length column matches the regex" in {
    val rawRow = RawRow("FirstColumnSecondColumnThirdColumn", 1)
    val firstColumnDescriptionElem = <ColumnDescription
      label="FirstColumn"
      startsAt="1"
      endsAt="11"
      useDuringValidation="true"
      checkColumnValueMatches="[a-zA-Z]+"
      checkColumnValueExists="false"
      />
    val secondColumnDescriptionElem = <ColumnDescription
      label="SecondColumn"
      startsAt="12"
      endsAt="23"
      useDuringValidation="true"
      checkColumnValueExists="false"
      />

    val thirdColumnDescriptionElem = <ColumnDescription
      label="ThirdColumn"
      startsAt="24"
      endsAt="34"
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
        val orderedRowDescription =
          OrderedRowDescription(fixedRowDivider, "label")

        val expectedCheckResult = true

        orderedRowDescription.check(rawRow) shouldBe expectedCheckResult
      case _ => fail
    }
  }

  it should "return true if the first delimited column matches the regex" in {
    val rawRow = RawRow("FirstColumn;SecondColumn;ThirdColumn", 1)
    val firstColumnDescriptionElem = <ColumnDescription
      label="FirstColumn"
      position="1"
      useDuringValidation="true"
      checkColumnValueExists="false"
      checkColumnValueMatches=".*"
      />
    val secondColumnDescriptionElem = <ColumnDescription
      label="SecondColumn"
      position="2"
      useDuringValidation="true"
      checkColumnValueExists="false"
      />

    val thirdColumnDescriptionElem = <ColumnDescription
      label="ThirdColumn"
      position="3"
      useDuringValidation="true"
      checkColumnValueExists="false"
      />

    val firstColumnDescriptionEither =
      DelimitedColumnDescription(firstColumnDescriptionElem.attributes)
    val secondColumnDescriptionEither =
      DelimitedColumnDescription(secondColumnDescriptionElem.attributes)
    val thirdColumnDescriptionEither =
      DelimitedColumnDescription(thirdColumnDescriptionElem.attributes)
    val delimiterEither = LiteralDelimiter(";")

    (firstColumnDescriptionEither,
      secondColumnDescriptionEither,
      thirdColumnDescriptionEither,
      delimiterEither) match {
      case (Right(firstColumnDescription),
      Right(secondColumnDescription),
      Right(thirdColumnDescription),
      Right(delimiter)) =>
        val columnDescriptionList = List(firstColumnDescription,
          secondColumnDescription,
          thirdColumnDescription)
        val fixedRowDivider = DelimitedRowDivider(columnDescriptionList, delimiter)
        val orderedRowDescription =
          OrderedRowDescription(fixedRowDivider, "label")

        val expectedCheckResult = true

        orderedRowDescription.check(rawRow) shouldBe expectedCheckResult
      case _ => fail
    }
  }

  it should "return true if the first fixed length and the last match the supplied regexes" in {
    val rawRow = RawRow("FirstColumnSecondColumnThirdColumn", 1)
    val firstColumnDescriptionElem = <ColumnDescription
      label="FirstColumn"
      startsAt="1"
      endsAt="11"
      useDuringValidation="true"
      checkColumnValueMatches="[a-zA-Z]+"
      checkColumnValueExists="false"
      />
    val secondColumnDescriptionElem = <ColumnDescription
      label="SecondColumn"
      startsAt="12"
      endsAt="23"
      useDuringValidation="true"
      checkColumnValueExists="false"
      />

    val thirdColumnDescriptionElem = <ColumnDescription
      label="ThirdColumn"
      startsAt="24"
      endsAt="34"
      useDuringValidation="true"
      checkColumnValueMatches="[a-zA-Z]+"
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
        val orderedRowDescription =
          OrderedRowDescription(fixedRowDivider, "label")

        val expectedCheckResult = true

        orderedRowDescription.check(rawRow) shouldBe expectedCheckResult
      case _ => fail
    }
  }

  it should "return true if the first delimited column and the last match the supplied regexes" in {
    val rawRow = RawRow("FirstColumn;SecondColumn;ThirdColumn", 1)
    val firstColumnDescriptionElem = <ColumnDescription
      label="FirstColumn"
      position="1"
      useDuringValidation="true"
      checkColumnValueExists="false"
      checkColumnValueMatches=".*"
      />
    val secondColumnDescriptionElem = <ColumnDescription
      label="SecondColumn"
      position="2"
      useDuringValidation="true"
      checkColumnValueExists="false"
      />

    val thirdColumnDescriptionElem = <ColumnDescription
      label="ThirdColumn"
      position="3"
      useDuringValidation="true"
      checkColumnValueExists="false"
      checkColumnValueMatches=".*"
      />

    val firstColumnDescriptionEither =
      DelimitedColumnDescription(firstColumnDescriptionElem.attributes)
    val secondColumnDescriptionEither =
      DelimitedColumnDescription(secondColumnDescriptionElem.attributes)
    val thirdColumnDescriptionEither =
      DelimitedColumnDescription(thirdColumnDescriptionElem.attributes)
    val delimiterEither = LiteralDelimiter(";")

    (firstColumnDescriptionEither,
      secondColumnDescriptionEither,
      thirdColumnDescriptionEither,
      delimiterEither) match {
      case (Right(firstColumnDescription),
      Right(secondColumnDescription),
      Right(thirdColumnDescription),
      Right(delimiter)) =>
        val columnDescriptionList = List(firstColumnDescription,
          secondColumnDescription,
          thirdColumnDescription)
        val fixedRowDivider = DelimitedRowDivider(columnDescriptionList, delimiter)
        val orderedRowDescription =
          OrderedRowDescription(fixedRowDivider, "label")

        val expectedCheckResult = true

        orderedRowDescription.check(rawRow) shouldBe expectedCheckResult
      case _ => fail
    }
  }

  it should "return false if the last fixed length column does not matche the regex" in {
    val rawRow = RawRow("FirstColumnSecondColumnThirdColumn", 1)
    val firstColumnDescriptionElem = <ColumnDescription
      label="FirstColumn"
      startsAt="1"
      endsAt="11"
      useDuringValidation="true"
      checkColumnValueExists="false"
      />
    val secondColumnDescriptionElem = <ColumnDescription
      label="SecondColumn"
      startsAt="12"
      endsAt="23"
      useDuringValidation="true"
      checkColumnValueExists="false"
      />

    val thirdColumnDescriptionElem = <ColumnDescription
      label="ThirdColumn"
      startsAt="24"
      endsAt="34"
      useDuringValidation="true"
      checkColumnValueMatches="[0-9]+"
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
        val orderedRowDescription =
          OrderedRowDescription(fixedRowDivider, "label")

        val expectedCheckResult = false

        orderedRowDescription.check(rawRow) shouldBe expectedCheckResult
      case _ => fail
    }
  }

  it should "return false if the last delimited column does not matche the regex" in {
    val rawRow = RawRow("FirstColumn;SecondColumn;ThirdColumn", 1)
    val firstColumnDescriptionElem = <ColumnDescription
      label="FirstColumn"
      position="1"
      useDuringValidation="true"
      checkColumnValueExists="false"
      />
    val secondColumnDescriptionElem = <ColumnDescription
      label="SecondColumn"
      position="2"
      useDuringValidation="true"
      checkColumnValueExists="false"
      />

    val thirdColumnDescriptionElem = <ColumnDescription
      label="ThirdColumn"
      position="3"
      useDuringValidation="true"
      checkColumnValueExists="false"
      checkColumnValueMatches="[0-9]+"
      />

    val firstColumnDescriptionEither =
      DelimitedColumnDescription(firstColumnDescriptionElem.attributes)
    val secondColumnDescriptionEither =
      DelimitedColumnDescription(secondColumnDescriptionElem.attributes)
    val thirdColumnDescriptionEither =
      DelimitedColumnDescription(thirdColumnDescriptionElem.attributes)
    val delimiterEither = LiteralDelimiter(";")

    (firstColumnDescriptionEither,
      secondColumnDescriptionEither,
      thirdColumnDescriptionEither,
      delimiterEither) match {
      case (Right(firstColumnDescription),
      Right(secondColumnDescription),
      Right(thirdColumnDescription),
      Right(delimiter)) =>
        val columnDescriptionList = List(firstColumnDescription,
          secondColumnDescription,
          thirdColumnDescription)
        val fixedRowDivider = DelimitedRowDivider(columnDescriptionList, delimiter)
        val orderedRowDescription =
          OrderedRowDescription(fixedRowDivider, "label")

        val expectedCheckResult = false

        orderedRowDescription.check(rawRow) shouldBe expectedCheckResult
      case _ => fail
    }
  }

  it should "return false if the first fixed length column exists and last columns do not match the regexes" in {
    val rawRow = RawRow("FirstColumnSecondColumn", 1)
    val firstColumnDescriptionElem = <ColumnDescription
      label="FirstColumn"
      startsAt="1"
      endsAt="11"
      useDuringValidation="true"
      checkColumnValueMatches="[0-9]+"
      checkColumnValueExists="false"
      />
    val secondColumnDescriptionElem = <ColumnDescription
      label="SecondColumn"
      startsAt="12"
      endsAt="23"
      useDuringValidation="true"
      checkColumnValueExists="false"
      />

    val thirdColumnDescriptionElem = <ColumnDescription
      label="ThirdColumn"
      startsAt="24"
      endsAt="34"
      useDuringValidation="true"
      checkColumnValueMatches="[0-9]+"
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
        val orderedRowDescription =
          OrderedRowDescription(fixedRowDivider, "label")

        val expectedCheckResult = false

        orderedRowDescription.check(rawRow) shouldBe expectedCheckResult
      case _ => fail
    }
  }

  it should "return false if the first delimited column exists and last columns do not match the regexes" in {
    val rawRow = RawRow("FirstColumn;SecondColumn;ThirdColumn", 1)
    val firstColumnDescriptionElem = <ColumnDescription
      label="FirstColumn"
      position="1"
      useDuringValidation="true"
      checkColumnValueExists="true"
      />
    val secondColumnDescriptionElem = <ColumnDescription
      label="SecondColumn"
      position="2"
      useDuringValidation="true"
      checkColumnValueExists="false"
      />

    val thirdColumnDescriptionElem = <ColumnDescription
      label="ThirdColumn"
      position="3"
      useDuringValidation="true"
      checkColumnValueExists="false"
      checkColumnValueMatches="[0-9]+"
      />

    val firstColumnDescriptionEither =
      DelimitedColumnDescription(firstColumnDescriptionElem.attributes)
    val secondColumnDescriptionEither =
      DelimitedColumnDescription(secondColumnDescriptionElem.attributes)
    val thirdColumnDescriptionEither =
      DelimitedColumnDescription(thirdColumnDescriptionElem.attributes)
    val delimiterEither = LiteralDelimiter(";")

    (firstColumnDescriptionEither,
      secondColumnDescriptionEither,
      thirdColumnDescriptionEither,
      delimiterEither) match {
      case (Right(firstColumnDescription),
      Right(secondColumnDescription),
      Right(thirdColumnDescription),
      Right(delimiter)) =>
        val columnDescriptionList = List(firstColumnDescription,
          secondColumnDescription,
          thirdColumnDescription)
        val fixedRowDivider = DelimitedRowDivider(columnDescriptionList, delimiter)
        val orderedRowDescription =
          OrderedRowDescription(fixedRowDivider, "label")

        val expectedCheckResult = false

        orderedRowDescription.check(rawRow) shouldBe expectedCheckResult
      case _ => fail
    }
  }

}
