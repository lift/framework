package net.liftweb
package documentation

import java.io.File

import scala.io.Source
import scala.xml.{Elem,NodeSeq}

import common._
import util.Html5
import util.Helpers._

sealed trait ExamplePart
case class ExampleInput(input: String) extends ExamplePart
case class ExampleFunction(function: String) extends ExamplePart
case class ExampleOutput(output: String) extends ExamplePart

case class FileContents(filename: String, contents: String)
case class ExampleContents(filename: String, exampleLabel: String, exampleParts: List[ExamplePart])

object ExtractCssSelectorExamples extends App {
  private def contentsToProcess(basePath: String): Box[List[FileContents]] = {
    val docsFile = new File(s"$basePath")

    for {
      docsDir <-
        ((Full(docsFile)
          .filter(_.exists) ?~ s"'$docsFile' should be a directory, but does not exist.")
          .filter(_.isDirectory) ?~ s"'$docsFile' should be a directory, not a file.")
    } yield {
      for {
        file <- docsDir.listFiles.toList
          if file.getName.endsWith(".html")
        fileContents <- tryo(Source.fromFile(file).mkString)
      } yield {
        FileContents(file.getName, fileContents)
      }
    }
  }

  private def extractPart(partBuilder: (String)=>ExamplePart)(ns: NodeSeq): Option[ExamplePart] = {
    var part: Option[ExamplePart] = None

    val partExtractor =
      "code" #> { ns: NodeSeq => ns match {
        case codeElement: Elem if codeElement.label == "code" =>
          part = Some(partBuilder(codeElement.text))
          codeElement
        case other => other
      } }

    partExtractor(ns)

    part
  }

  private def hasClass_?(element: Elem, className: String) = {
    element.attribute("class") match {
      case Some(thing) =>
        charSplit(thing.text, ' ').exists(_ == className)
      case _ =>
        false
    }
  }

  private def extractExamplesFromContents(fileContents: FileContents): List[ExampleContents] = {
    Html5.parse(fileContents.contents).toList.flatMap { html =>
      var exampleContents = List[ExampleContents]()

      val contentExtractor =
        ".selectors" #> { exampleNodes: NodeSeq =>
          var parts = List[ExamplePart]()
          var exampleLabel = "No label"

          var labelExtractor =
            ".title" #> { title: NodeSeq => title match {
              case titleElement: Elem if titleElement.label == "div" =>
                exampleLabel = titleElement.text

                titleElement
              case other => other
            }}
          val partExtractor =
            ".listingblock" #> { part: NodeSeq =>
              var specializedPartExtractor =
                part match {
                  case inputBlock: Elem if hasClass_?(inputBlock, "input") =>
                    Some(extractPart(ExampleInput(_)) _)
                  case selectorBlock: Elem if hasClass_?(selectorBlock, "selector") =>
                    Some(extractPart(ExampleFunction(_)) _)
                  case outputBlock: Elem if hasClass_?(outputBlock, "output") =>
                    Some(extractPart(ExampleOutput(_)) _)
                  case _ => None
                }

              for {
                extractor <- specializedPartExtractor
                extractedPart <- extractor(part)
              } {
                parts ::= extractedPart
              }

              part
            }

          (labelExtractor & partExtractor)(exampleNodes)

          exampleContents ::= ExampleContents(fileContents.filename, exampleLabel, parts.reverse)

          exampleNodes
        }

      contentExtractor(html)

      exampleContents.reverse
    }
  }

  if (args.length < 1) {
    Console.err.println(
      "Expected one argument: the base directory of the Lift project."
    )
  } else {
    val thingies =
      for {
        extractedContents <- contentsToProcess(args(0))
      } yield {
        extractedContents.map(extractExamplesFromContents _)
      }

    thingies match {
      case Failure(message, _, _) => Console.err.println(message)
      case Full(thingie) => println(thingie)
      case _ => Console.err.println("Unknown error.")
    }
  }
}
