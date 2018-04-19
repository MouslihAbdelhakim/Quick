package com.scalableQuality.quick.surface.main

import com.scalableQuality.quick.mantle.error.UnrecoverableError
import com.scalableQuality.quick.mantle.labels.FileLabelsFromPaths
import com.scalableQuality.quick.mantle.parsing.GroupRowsByRowDescription
import com.scalableQuality.quick.mantle.reportInterptations.textReport.ValidationAndMatchingTextReport
import com.scalableQuality.quick.surface.commandLineOptions.CommandLineParser
import com.scalableQuality.quick.surface.input.{ReadRowsFromFile, ReadXmlFile}
import com.scalableQuality.quick.surface.output.{
  ExitWithStatus,
  WriteTextReportToStdout,
  WriteToStderr
}

object Quick extends App {

  val quickStateOpt = CommandLineParser(args)
  quickStateOpt match {
    case Some(quickState) =>
      val leftFilePath = quickState.leftFile
      val rightFilePath = quickState.rightFile

      val (leftFileLabel, rightFileLabel) = FileLabelsFromPaths(quickState)

      val leftFileEither = ReadRowsFromFile(leftFilePath)
      val rightFileEither = ReadRowsFromFile(rightFilePath)
      val fileDescriptionEither = ReadXmlFile(quickState)

      (leftFileEither, rightFileEither, fileDescriptionEither) match {
        case (Right(leftFileRows),
              Right(rightFileRows),
              Right(fileDescriptionElem)) =>
          val rowToRowDescriptionMatcherEither = GroupRowsByRowDescription(
            fileDescriptionElem,
            leftFileLabel,
            rightFileLabel,
            quickState
          )
          rowToRowDescriptionMatcherEither match {
            case Right(rowDescriptionMatcher) =>
              val validationAndMatchingProcessesEither =
                rowDescriptionMatcher.validateAndMatchTheseTwoFiles(
                  leftFileRows,
                  rightFileRows)
              validationAndMatchingProcessesEither match {
                case Right(validationAndMatchingProcesses) =>
                  val validationAndMatchingReports =
                    validationAndMatchingProcesses.validationAndMatchingReports

                  val validationAndMatchingTextReports =
                    validationAndMatchingReports.map(
                      ValidationAndMatchingTextReport(_))

                  validationAndMatchingTextReports.foreach(
                    WriteTextReportToStdout(_))
                  ExitWithStatus.exitWithReport(validationAndMatchingReports)
                case Left(unknownRowErrorMessage) =>
                  WriteToStderr(unknownRowErrorMessage)
                  ExitWithStatus.interruptedByAnError
              }
            case Left(errorMessage) =>
              WriteToStderr(errorMessage)
              ExitWithStatus.interruptedByAnError
          }

        case _ =>
          val errorMessages = UnrecoverableError.collectAllErrorsToList(
            leftFileEither,
            rightFileEither,
            fileDescriptionEither)
          errorMessages.foreach(WriteToStderr(_))
          ExitWithStatus.interruptedByAnError
      }
    case None =>
      ExitWithStatus.interruptedByAnError
  }

}
