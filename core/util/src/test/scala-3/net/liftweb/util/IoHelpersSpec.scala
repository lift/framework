/*
 * Copyright 2017-2026 Lift Committers and Contributors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package net.liftweb
package util

import org.specs2.mutable.Specification

import common._
import Helpers._

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

class IoHelpersSpec extends Specification with IoHelpers {
  "IoHelpers Specification".title

  "Io helpers" should {

    "readWholeFile properly" in {
      // Copy a resource file to the tmp directory so we can refer to it as a Path
      val resourceAsPath: Box[Path] = {
        for {
          bytes <- tryo(readWholeStream(getClass.getResourceAsStream("IoHelpersSpec.txt"))).filter(_ ne null)
          text <- tryo(new String(bytes))
          path = {
            val tempFile = Files.createTempFile(s"IoHelpersSpec_${nextFuncName}", ".tmp") 
            Files.write(tempFile, text.getBytes(StandardCharsets.UTF_8))
            tempFile
          }
        } yield path
      }

      resourceAsPath.isDefined === true

      resourceAsPath.foreach { path =>
        val pathContents = new String(readWholeFile(path)).trim
        Files.delete(path)
        pathContents === "IoHelpersSpec"
      }

      success
    }
  }
}
