/*
 * Copyright 2010 WorldWide Conferencing, LLC
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

package net.liftweb {
package imaging {


import _root_.org.specs._
import _root_.org.specs.runner._
import _root_.org.specs.Sugar._

class ImageResizerTest extends JUnit4(ImageResizerSpec)
object ImageResizerSpec extends Specification  {
  
  "ImageResizer scaledMaxDim function" should {
    "be invariant" in {
      val range  = (1 to 10)
      for {w <- range
           h <- range
           maxW <- range
           maxH <- range}{
        val (scaledWidth, scaledHeight) = ImageResizer.scaledMaxDim(w,h, maxW,maxH)
        
        (scaledWidth) must beLessThanOrEqualTo(maxW)
        (scaledHeight) must beLessThanOrEqualTo(maxH)
        (scaledWidth == maxW || scaledHeight == maxH) must beTrue
      }
    }
  } 
}

}
}