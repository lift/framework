package net.liftweb
package documentation

import java.io.{File,PrintStream}

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
case class ExampleContents(filename: String, exampleLabel: String, setupCode: String, exampleParts: List[ExamplePart])

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
        FileContents(file.getName.replace('.', '-'), fileContents)
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
      var setupCode: String = ""

      val setupExtractor =
        ".setup" #> {
          "code *" #> { codeContents: NodeSeq =>
            setupCode = codeContents.text

            codeContents
          }
        }

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

          exampleContents ::= ExampleContents(fileContents.filename, exampleLabel, setupCode, parts.reverse)

          exampleNodes
        }

      setupExtractor(html)
      contentExtractor(html)

      exampleContents.reverse
    }
  }

  if (args.length < 2) {
    Console.err.println(
      "Expected two arguments: the base directory of generated HTML and the base directory of the Lift project."
    )
  } else {
    val examples =
      for {
        extractedContents <- contentsToProcess(args(0))
      } yield {
        extractedContents.flatMap(extractExamplesFromContents _)
      }

    examples match {
      case Full(exampleContents) =>
        val testPath = s"${args(1)}/core/documentation-helpers/src/test/scala/net/liftweb/documentation"
        (new File(testPath)).mkdirs

        for {
          (normalizedFilename, contents) <- exampleContents.groupBy(_.filename)
        } {
          val filename = camelify(normalizedFilename.replace('-','_'))

          val examples =
            for {
              ExampleContents(_, exampleLabel, setupCode, exampleParts) <- contents
              i <- (0 to (exampleParts.length / 3))
              ExampleInput(input) <- exampleParts.lift(i)
              ExampleFunction(function) <- exampleParts.lift(i + 1)
              ExampleOutput(output) <- exampleParts.lift(i + 2)
            } yield {
              s"""
              |    ""\"$exampleLabel""\" in {
              |      $setupCode
              |
              |      val input = Html5.parse(""\"<div>$input</div>""\")
              |      val function: (NodeSeq)=>NodeSeq = $function
              |      val output = Html5.parse(""\"<div>$output</div>""\")
              |
              |      // The function returns a NodeSeq; we assume it will
              |      // contain a single element and unwrap it.
              |      input.map { html => function(html) } must beLike {
              |       case Full(rendered) =>
              |         rendered must ==/(output.toOption.get)
              |      }
              |    }""".stripMargin('|')
            }

          val file = new File(s"$testPath/${filename}Test.scala")

          var stream: PrintStream = null
          try {
            stream = new PrintStream(file)
            stream.println(s"""
            |package net.liftweb
            |package documentation
            |
            |import scala.xml._
            |
            |import org.specs2.matcher.XmlMatchers
            |import org.specs2.mutable.Specification
            |
            |import common._
            |import util.Html5
            |import util.Helpers._
            |
            |object $filename extends Specification with XmlMatchers {
            |  ""\"${filename} examples""\" should {
            |${examples.mkString("\n")}
            |  }
            |}""".stripMargin('|'))
          } finally {
            Option(stream).map(_.close)
          }
        }

      case Failure(message, _, _) => Console.err.println(message)
      case _ => Console.err.println("Unknown error.")
    }
  }
}
