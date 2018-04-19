package com.scalableQuality.quick.surface.input

import java.io.File

import com.scalableQuality.quick.mantle.error.{EncounteredError, UnrecoverableError}
import com.scalableQuality.quick.mantle.parsing.RawRow

import scala.annotation.tailrec
import scala.io.Source
import scala.util.{Failure, Success, Try}

object ReadRowsFromFile {

  def apply(filePath: String): Either[UnrecoverableError, Stream[RawRow]] = if(fileExists(filePath)){
    getFileStream(filePath, supportedEncodings)
  } else {
    val error = EncounteredError(
      s"reading $filePath",
      "path does not point to a readable file",
      "verify that the paths points to a readable file"
    )
    Left(error)
  }


  @tailrec private def getFileStream(
      filePath: String,
      encodings: List[String]): Either[UnrecoverableError, Stream[RawRow]] =
    encodings match {
      case Nil =>
        val errorMessage = EncounteredError(
          s"reading $filePath",
          "file's encoding is unknown",
          "This is a bug, post it on https://github.com/MouslihAbdelhakim/Quick/issues"
        )
        Left(errorMessage)

      case enc :: restOfEncoding =>
        Try(Source.fromFile(filePath, enc).getLines().toStream) match {
          case Success(fileStream) =>
            Right(mapToRawRow(fileStream))
          case Failure(_) =>
            getFileStream(filePath, restOfEncoding)
        }
    }

  private def mapToRawRow(fileRowsStream: Stream[String]): Stream[RawRow] = {
    fileRowsStream.zipWithIndex.map { rowIndex =>
      RawRow(rowIndex._1, rowIndex._2 + 1)
    }
  }

  private def fileExists(path: String): Boolean = {
    val file = new File(path)
    file.exists() && file.canRead
  }

  private val supportedEncodings = List("ISO-8859-1","UTF-8", "UTF-16")
}
