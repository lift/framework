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


import java.awt.{Graphics2D, Graphics, Transparency, AlphaComposite, RenderingHints}
import java.awt.geom.AffineTransform 
import java.awt.image.{AffineTransformOp, BufferedImage, ColorModel, IndexColorModel}
import javax.imageio.{IIOImage, ImageIO, ImageWriteParam}

import org.apache.sanselan.ImageReadException
import org.apache.sanselan.Sanselan
import org.apache.sanselan.ImageFormat
import org.apache.sanselan.common.IImageMetadata
import org.apache.sanselan.common.RationalNumber
import org.apache.sanselan.formats.jpeg.JpegImageMetadata
import org.apache.sanselan.formats.tiff.TiffField
import org.apache.sanselan.formats.tiff.TiffImageMetadata
import org.apache.sanselan.formats.tiff.constants.TagInfo
import org.apache.sanselan.formats.tiff.constants.TiffConstants
import org.apache.sanselan.formats.tiff.constants.TiffTagConstants

import java.io.{InputStream,ByteArrayOutputStream,ByteArrayInputStream}

import net.liftweb.common.{Box, Full, Empty}
import net.liftweb.util.Helpers

object ImageOutFormat extends Enumeration("png", "jpg", "gif", "bmp"){
  val png,jpeg,gif,bmp = Value
}

//Degrees are positive going clockwise (270 is same as -90)
object ImageOrientation extends Enumeration(1) {
  val ok = Value(1, "OK")
  val mirrored = Value(2, "Mirror")
  
  val rotate180 = Value(3, "Rotate 180")
  val rotate180mirror = Value(4, "Rotate 180 Mirror")
  
  val rotate270mirror = Value(5, "Rotate 270 Mirror")
  val rotate270 = Value(6, "Rotate 270")
  
  val rotate90mirror = Value(7, "Rotate 90 Mirror")
  val rotate90 = Value(8, "Rotate 90")
  
  def valueOf(v: Int):Box[Value] = {
    if (v > 0 && v <= 8) Full(apply(v)) else Empty
  }
}

case class ImageWithMetaData(image:BufferedImage, orientation:Box[ImageOrientation.Value], format:ImageOutFormat.Value)

object ImageResizer extends ImageResizer(Map(RenderingHints.KEY_INTERPOLATION -> RenderingHints.VALUE_INTERPOLATION_BILINEAR), true)

class ImageResizer(renderingHintsMap:Map[java.awt.RenderingHints.Key,Any], multiStepDownScale:Boolean) {
  
  val renderingHints = {
    val h = new RenderingHints(null) 
    renderingHintsMap.foreach(p => h.put(p._1, p._2))
    h
  }

  def getOrientation(imageBytes:Array[Byte]):Box[ImageOrientation.Value] = Helpers.tryo {
    Sanselan.getMetadata(imageBytes) match {
      case metaJpg:JpegImageMetadata => 
        val exifValue = metaJpg.findEXIFValue(TiffTagConstants.TIFF_TAG_ORIENTATION)
        if (exifValue != null) ImageOrientation.valueOf(exifValue.getIntValue) else Empty
      case _ => Empty
    }
  }.flatMap(x=>x)
  
  def getImageFromStream(is:java.io.InputStream):ImageWithMetaData = {
    val imageBytes = Helpers.readWholeStream(is)
    val orientation = getOrientation(imageBytes)
    val format = Sanselan.guessFormat(imageBytes) match {
      case ImageFormat.IMAGE_FORMAT_JPEG => ImageOutFormat.jpeg
      case ImageFormat.IMAGE_FORMAT_GIF => ImageOutFormat.gif
      case ImageFormat.IMAGE_FORMAT_PNG => ImageOutFormat.png
      case ImageFormat.IMAGE_FORMAT_BMP => ImageOutFormat.bmp
      case f => throw new RuntimeException("Unsupported image format: " + f)
    }
    ImageWithMetaData(ImageIO.read(new java.io.ByteArrayInputStream(imageBytes)), orientation, format)
  }
  
  def imageToStream(format:ImageOutFormat.Value, image:BufferedImage):InputStream = {
    new ByteArrayInputStream(imageToBytes(format, image, 0.8f))
  }
  
  def imageToBytes(format: ImageOutFormat.Value, image: BufferedImage, jpegQuality: Float):Array[Byte] = {
    val outputStream = new ByteArrayOutputStream()
    format match {
      case ImageOutFormat.jpeg =>
        val imageWriter = ImageIO.getImageWritersByFormatName("jpeg").next()
        val iwp = imageWriter.getDefaultWriteParam()
        iwp.setCompressionMode(ImageWriteParam.MODE_EXPLICIT)
        iwp.setCompressionQuality(jpegQuality)
        imageWriter.setOutput(ImageIO.createImageOutputStream(outputStream))
        imageWriter.write(null, new IIOImage(image, null, null), iwp)
        imageWriter.dispose()
      case _ =>
        ImageIO.write(image, format.toString, outputStream)
    }
    outputStream.toByteArray()
  }
  
  /**
   * Resize to a square
   * Will preserve the aspect ratio of the original and than center crop the larger dimension.
   * A image of (200w,240h) squared to (100) will first resize to (100w,120h) and then take then crop
   * 10 pixels from the top and bottom of the image to produce (100w,100h)
   */
  def square(orientation:Box[ImageOrientation.Value], originalImage:BufferedImage, max:Int):BufferedImage = {
    val image = {
      val height = originalImage.getHeight
      val width = originalImage.getWidth
      val ratio:Double = width.doubleValue/height
      
      //set smaller dimension to the max
      val (scaledWidth, scaledHeight) = if (width < height) {
        (max,(max.doubleValue/ratio).intValue)
      } else {
        ((max.doubleValue*ratio).intValue, max)
      }
      resize(orientation, originalImage, scaledWidth, scaledHeight)
    }
    
    def halfDiff(dim:Int):Int = (dim-max)/2
    
    if (image.getHeight > max) {
      image.getSubimage(0,halfDiff(image.getHeight), image.getWidth, max)
    } else if (image.getWidth > max) {
      image.getSubimage(halfDiff(image.getWidth),0, max, image.getHeight)
    } else image
  }
  

  def scaledMaxDim(width:Int, height:Int , maxWidth:Int, maxHeight:Int):(Int,Int) = {
    val ratio:Double = width.doubleValue/height

    val scaleW = (maxWidth, (maxWidth.doubleValue/ratio).intValue)
    val scaleH = ((maxHeight.doubleValue*ratio).intValue,maxHeight)

    if (width > height && scaleW._2 <= maxHeight) 
      scaleW 
    else if (scaleH._1 <= maxWidth)
      scaleH
    else scaleW
  }
  
  /**
   * Resize to maximum dimension preserving the aspect ratio.  This is basically equivalent to what you would expect by setting
   * "max-width" and "max-height" CSS attributes but will scale up an image if necessary
   */
  def max(orientation:Box[ImageOrientation.Value],originalImage:BufferedImage, maxWidth:Int, maxHeight:Int):BufferedImage = {
    val (scaledWidth, scaledHeight) = scaledMaxDim(originalImage.getWidth, originalImage.getHeight, maxWidth, maxHeight)
    resize(orientation, originalImage, scaledWidth, scaledHeight)
  }
  
  /**
   * Algorithm adapted from example in Filthy Rich Clients http://filthyrichclients.org/
   * Resize an image and account of its orientation.  This will not preserve aspect ratio.
   */
  def resize(orientation:Box[ImageOrientation.Value], img:BufferedImage, targetWidth:Int, targetHeight:Int): BufferedImage = {
    val imgType = if (img.getTransparency() == Transparency.OPAQUE) BufferedImage.TYPE_INT_RGB else BufferedImage.TYPE_INT_ARGB
    var ret = img
    var scratchImage:BufferedImage = null
    var  g2:Graphics2D = null
    var w = img.getWidth
    var h = img.getHeight
    var prevW = ret.getWidth
    var prevH = ret.getHeight

    val isTranslucent:Boolean = img.getTransparency !=  Transparency.OPAQUE

    //If we're resizing down by more than a factor of two, resize in multiple steps to preserve image quality
    do {
      if (w > targetWidth && multiStepDownScale) {
        w /= 2
        if (w < targetWidth) {
          w = targetWidth
        }
      } else w = targetWidth

      if (h > targetHeight && multiStepDownScale) {
        h /= 2
        if (h < targetHeight) {
          h = targetHeight
        }
      } else h = targetHeight

      if (scratchImage == null || isTranslucent) {
        scratchImage = new BufferedImage(w, h, imgType);
        g2 = scratchImage.createGraphics
      }
      g2.setRenderingHints(renderingHints)
      g2.drawImage(ret, 0, 0, w, h, 0, 0, prevW, prevH, null)
      prevW = w
      prevH = h

      ret = scratchImage
    } while (w != targetWidth || h != targetHeight)

    if (g2 != null) {
      g2.dispose
    }

    // If we used a scratch buffer that is larger than our target size,
    // create an image of the right size and copy the results into it
    // If there is an orientation value other than the default, rotate the image appropriately
    if (targetWidth != ret.getWidth || targetHeight != ret.getHeight || orientation.map(_ != ImageOrientation.ok).getOrElse(false)) {

      val (tW, tH, rotFunc) =  orientation match {
        case Full(ImageOrientation.rotate180) =>
          (targetWidth, targetHeight, (g2:Graphics2D) => {
            g2.rotate(Math.Pi)
            g2.translate(-targetWidth, -targetHeight)
          })
        case Full(ImageOrientation.rotate270) =>
          (targetHeight, targetWidth, (g2:Graphics2D) => {
            g2.rotate(Math.Pi/2)
            g2.translate(0, -targetHeight)
          })
        case Full(ImageOrientation.rotate90) =>
          (targetHeight, targetWidth, (g2:Graphics2D) => {
            g2.rotate(-Math.Pi/2)
            g2.translate(-targetWidth, 0)
          })
        case _ => (targetWidth, targetHeight, (g2:Graphics2D) => {})
      }
      scratchImage = new BufferedImage(tW, tH, imgType)
      g2 = scratchImage.createGraphics
      rotFunc(g2)
      g2.drawImage(ret, 0, 0, null)
      g2.dispose
      ret = scratchImage
    }

    ret
  }
     
} //ImageResizer
} //imaging
} //net.liftweb