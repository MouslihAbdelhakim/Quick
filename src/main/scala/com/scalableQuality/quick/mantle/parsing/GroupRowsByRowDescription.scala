package com.scalableQuality.quick.mantle.parsing

import com.scalableQuality.quick.core.fileComponentDescriptions.OrderedRowDescription
import com.scalableQuality.quick.core.fileProcessingPhase.{
  RowsProcessingPhase,
  CheckAndValidateAndMatchRows,
  ValidateAndMatchTwoFiles
}
import com.scalableQuality.quick.mantle.constructFromXml._
import com.scalableQuality.quick.mantle.error.{
  BunchOfErrors,
  UnrecoverableError
}
import com.scalableQuality.quick.mantle.parsing.errorMessages.{
  FileDescriptionElemErrorMessages,
  GroupRowsByRowDescriptionErrorMessages
}
import com.scalableQuality.quick.surface.commandLineOptions.QuickState

import scala.annotation.tailrec
import scala.xml.{Elem, MetaData}
import scala.collection.mutable

class GroupRowsByRowDescription(
    val listOfRowIdentifier: List[RowToRowDescriptionMatcher],
    leftFileLabel: Option[String],
    rightFileLabel: Option[String],
    quickState: QuickState
) {
  def validateAndMatchTheseTwoFiles(
      leftFileRows: Stream[RawRow],
      rightFileRows: Stream[RawRow]
  ): Either[UnrecoverableError, ValidateAndMatchTwoFiles] = {

    val groupLeftFileRowsByRowDescriptionEither =
      groupRowsByRowDescription(
        leftFileRows.toList,
        listOfRowIdentifier,
        quickState.ignoreUnknownRows,
        GroupRowsByRowDescriptionErrorMessages
          .unknownRow(quickState.leftFile,
                      quickState.descriptionFile,
                      quickState.descriptionId,
                      _)
      )
    val groupRightFileRowsByRowDescriptionEither =
      groupRowsByRowDescription(
        rightFileRows.toList,
        listOfRowIdentifier,
        quickState.ignoreUnknownRows,
        GroupRowsByRowDescriptionErrorMessages
          .unknownRow(quickState.leftFile,
                      quickState.descriptionFile,
                      quickState.descriptionId,
                      _)
      )

    (groupLeftFileRowsByRowDescriptionEither,
     groupRightFileRowsByRowDescriptionEither) match {
      case (Right(groupLeftFileRowsByRowDescription),
            Right(groupRightFileRowsByRowDescription)) =>
        val matchedGroups = matchGroupsRowsByRowDescription(
          groupLeftFileRowsByRowDescription,
          groupRightFileRowsByRowDescription)
        val validationAndMatchingProcesses: List[RowsProcessingPhase] =
          matchedGroups.map { rowsProcessingPhaseParameters =>
            quickState.rowsProcessingPhase(
              rowsProcessingPhaseParameters._1,
              rowsProcessingPhaseParameters._2,
              rowsProcessingPhaseParameters._3,
              this.leftFileLabel,
              this.rightFileLabel
            )
          }
        val validateAndMatchTwoFiles = ValidateAndMatchTwoFiles(
          validationAndMatchingProcesses)
        Right(validateAndMatchTwoFiles)
      case (Left(leftFileError), _) =>
        Left(leftFileError)
      case (_, Left(rightFileError)) =>
        Left(rightFileError)
    }
  }

  private def matchGroupsRowsByRowDescription(
      leftFileRowGroups: List[(OrderedRowDescription, List[RawRow])],
      rightFileRowGroups: List[(OrderedRowDescription, List[RawRow])]
  ): List[(OrderedRowDescription, List[RawRow], List[RawRow])] = {
    @tailrec def loop(
        leftFileRowGroups: List[(OrderedRowDescription, List[RawRow])],
        rightFileRowGroups: List[(OrderedRowDescription, List[RawRow])],
        accumulator: List[(OrderedRowDescription, List[RawRow], List[RawRow])]
    ): List[(OrderedRowDescription, List[RawRow], List[RawRow])] =
      leftFileRowGroups match {
        case Nil =>
          val remainingRightFileRowGroups =
            rightFileRowGroups.map(rowGroup => (rowGroup._1, Nil, rowGroup._2))
          remainingRightFileRowGroups ::: accumulator

        case (orderedRowDescription, leftFileRows) :: restOfLeftFileRowGroups =>
          val (matchingRightGroups, restOfRightFileRowGroups) =
            rightFileRowGroups.partition(_._1 == orderedRowDescription)
          val groupedByRowDescription = matchingRightGroups match {
            case Nil =>
              (orderedRowDescription, leftFileRows, Nil)

            case (_, rightFileRows) :: _ =>
              (orderedRowDescription, leftFileRows, rightFileRows)
          }
          loop(
            restOfLeftFileRowGroups,
            restOfRightFileRowGroups,
            groupedByRowDescription :: accumulator
          )
      }

    loop(leftFileRowGroups, rightFileRowGroups, Nil)
  }

  def groupRowsByRowDescription(
      rows: List[RawRow],
      rowIdentifiers: List[RowToRowDescriptionMatcher],
      ignoreUnknownRows: Boolean,
      unknownRowError: Int => Either[
        UnrecoverableError,
        List[(OrderedRowDescription, List[RawRow])]]
  ): Either[UnrecoverableError, List[(OrderedRowDescription, List[RawRow])]] = {

    type RowDescriptionToRowsHashMap = mutable.HashMap[
      OrderedRowDescription,
      mutable.Set[RawRow]] with mutable.MultiMap[OrderedRowDescription, RawRow]

    @tailrec def loop(
        rows: List[RawRow],
        rowIdentifiers: List[RowToRowDescriptionMatcher],
        rowDescriptionToRowsHashMap: RowDescriptionToRowsHashMap,
        ignoreUnknownRows: Boolean,
        unknownRowError: Int => Either[UnrecoverableError,
                                       List[(OrderedRowDescription,
                                             List[RawRow])]]
    ): Either[UnrecoverableError, List[(OrderedRowDescription, List[RawRow])]] =
      rows match {
        case Nil =>
          val rowDescriptionAndRows = for {
            orderedRowAndItsRows <- rowDescriptionToRowsHashMap
          } yield (orderedRowAndItsRows._1, orderedRowAndItsRows._2.toList)
          Right(rowDescriptionAndRows.toList)
        case row :: restOfRows =>
          val orderedRowDescriptionList =
            rowIdentifiers.collect {
              case orderedRowDesc if orderedRowDesc.canIdentify(row) =>
                orderedRowDesc.orderedRowDescription
            }
          orderedRowDescriptionList match {
            case Nil =>
              if (ignoreUnknownRows)
                loop(
                  restOfRows,
                  rowIdentifiers,
                  rowDescriptionToRowsHashMap,
                  ignoreUnknownRows,
                  unknownRowError
                )
              else
                unknownRowError(row.lineNumber)
            case _ =>
              orderedRowDescriptionList.foreach(
                rowDesc => rowDescriptionToRowsHashMap.addBinding(rowDesc, row)
              )
              loop(
                restOfRows,
                rowIdentifiers,
                rowDescriptionToRowsHashMap,
                ignoreUnknownRows,
                unknownRowError
              )
          }
      }

    val rowDescriptionToRowsHashMap =
      new mutable.HashMap[OrderedRowDescription, mutable.Set[RawRow]]
      with mutable.MultiMap[OrderedRowDescription, RawRow]
    loop(rows,
         rowIdentifiers,
         rowDescriptionToRowsHashMap,
         ignoreUnknownRows,
         unknownRowError)
  }
}

object GroupRowsByRowDescription {

  private def apply(
      listOfRowIdentifier: List[RowToRowDescriptionMatcher],
      leftFileLabel: Option[String],
      rightFileLabel: Option[String],
      quickState: QuickState
  ): Either[UnrecoverableError, GroupRowsByRowDescription] =
    listOfRowIdentifier match {
      case Nil =>
        GroupRowsByRowDescriptionErrorMessages.noRowDescriptionIsProvided
      case _ =>
        val groupRowsByRowDescription = new GroupRowsByRowDescription(
          listOfRowIdentifier,
          leftFileLabel,
          rightFileLabel,
          quickState)
        Right(groupRowsByRowDescription)
    }

  private def apply(
      fileDescriptionElem: Elem,
      leftFileLabel: Option[String],
      rightFileLabel: Option[String],
      quickState: QuickState
  ): Either[UnrecoverableError, GroupRowsByRowDescription] = {

    @tailrec def loop(
        rowDescriptionsElems: List[Elem],
        rowIdentifierAccumulator: List[RowToRowDescriptionMatcher]
    ): Either[UnrecoverableError, List[RowToRowDescriptionMatcher]] =
      rowDescriptionsElems match {
        case Nil =>
          Right(rowIdentifierAccumulator)

        case rowDesc :: restOfRowDescriptions =>
          val rowIdentifier = RowToRowDescriptionMatcher(rowDesc)
          rowIdentifier match {
            case Left(errorMessage) =>
              Left(errorMessage)
            case Right(validRowIdentifier) =>
              loop(restOfRowDescriptions,
                   validRowIdentifier :: rowIdentifierAccumulator)
          }
      }

    val listOfRowIdentifier =
      loop(XMLHelperFunctions.collectElemChildren(fileDescriptionElem), Nil)
    listOfRowIdentifier match {
      case Right(list) =>
        GroupRowsByRowDescription(list,
                                  leftFileLabel,
                                  rightFileLabel,
                                  quickState)
      case Left(errorMessage) =>
        Left(errorMessage)
    }

  }

  def apply(
      fileDescriptionElem: FileDescriptionElem,
      leftFileLabel: Option[String],
      RightFileLabel: Option[String],
      quickState: QuickState
  ): Either[UnrecoverableError, GroupRowsByRowDescription] = {
    fileDescriptionElem.elem match {
      case Right(fileDescElem) =>
        val groupRowsByRowDescription = GroupRowsByRowDescription(
          fileDescElem,
          leftFileLabel,
          RightFileLabel,
          quickState)
        groupRowsByRowDescription match {
          case Right(_) => groupRowsByRowDescription
          case Left(errorMessage) =>
            FileDescriptionElemErrorMessages.ErrorValidatingFileDescription(
              fileDescriptionElem,
              errorMessage)
        }
      case Left(errorMessage) =>
        Left(errorMessage)
    }
  }

}
