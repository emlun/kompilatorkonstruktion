package koolc
package utils

import java.io.File
import scala.io.Source

case class Context(
  val reporter: Reporter,
  val outDir: Option[File],
  val file: Option[File]
){
  var recursionCount: Int = 0
}
