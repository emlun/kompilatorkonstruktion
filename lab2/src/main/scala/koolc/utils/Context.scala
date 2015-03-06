package koolc
package utils

import java.io.File
import scala.io.Source

case class Context(
  val reporter: Reporter,
  val outDir: Option[File],
  val file: Option[File],
  val source: Source
)

object Context {
  def apply(reporter: Reporter, outDir: Option[File], file: File) =
    new Context(reporter, outDir, Some(file), Source fromFile file)
}
