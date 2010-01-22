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

import java.awt.{Graphics, RenderingHints, Transparency} 
import java.awt.geom.AffineTransform 
import java.awt.image.{AffineTransformOp, BufferedImage, ColorModel, IndexColorModel} 

/** 
 * Helpers for manipulating images 
 *
 * @author Ross M
 */ 
object ImageHelpers { 
  
  /** 
   * Rendering hints set up for the highest quality rendering 
   */ 
  val highQualityHints = { 
    val h = new RenderingHints(null) 
    h.put(RenderingHints.KEY_ALPHA_INTERPOLATION, RenderingHints.VALUE_ALPHA_INTERPOLATION_QUALITY) 
    h.put(RenderingHints.KEY_COLOR_RENDERING, RenderingHints.VALUE_COLOR_RENDER_QUALITY) 
    h.put(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BICUBIC) 
    h.put(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON) 
    h.put(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY) 
    h 
  }
  
  /** 
   * Resize an image of the given source type by the given ratios, properly handling GIF transparency, giving back the resized 
   * image and the new image format type that should be used. 
   * 
   * The image type might change if the input type is an indexed color model, because it is a hard problem to choose an optimized 
   * palette, and currently we don't. This function will return "png" as the new type in this case. 
   * 
   * If the input image is not using an indexed color model with transparency, then the target format and color model will be 
   * identical to the source. 
   */ 
  def resize(source: BufferedImage, inputFormat: String, dx: Double, dy: Double): (BufferedImage, String) = { 
    var sourceColorModel = source.getColorModel 
    val targetColorModel = source.getColorModel 
    val standardColorModel = ColorModel.getRGBdefault 
    val (targetWidth, targetHeight) = (((source.getWidth: Double) * dx).asInstanceOf[Int], ((source.getHeight: Double) * dy).asInstanceOf[Int]) 
    
    def resize(src: BufferedImage, dst: BufferedImage) { 
      val g = dst.createGraphics 
      try { 
        g.setRenderingHints(highQualityHints) 
        g.drawImage(src, new AffineTransformOp(AffineTransform.getScaleInstance(dx, dy), AffineTransformOp.TYPE_BICUBIC), 0, 0) 
      } finally { 
        g.dispose 
      } 
    } 
    
    // GIF support in Java is very ornery. For GIFs we have to manually do the masking 
    // on input, and then just punt on outputting GIFs and instead output PNGs. 
    if (sourceColorModel.isInstanceOf[IndexColorModel] && 
        sourceColorModel.hasAlpha && 
        sourceColorModel.getTransparency == Transparency.BITMASK && 
        sourceColorModel.asInstanceOf[IndexColorModel].getTransparentPixel >= 0){ 
      
      val indexColorModel = sourceColorModel.asInstanceOf[IndexColorModel] 
      val transparent = indexColorModel.getRGB(indexColorModel.getTransparentPixel) 
      val masked = new BufferedImage(standardColorModel, standardColorModel.createCompatibleWritableRaster(source.getWidth, source.getHeight), standardColorModel.isAlphaPremultiplied, null) 
      var w = masked.getWidth 
      var h = masked.getHeight 
      var y = 0 
      val buf  = new Array[Int](w) 
      
      while (y < h) { 
        source.getRGB(0, y, w, 1, buf,  0, 1) 
        var x = 0 
        while (x < w) { 
          val c = buf(x) 
          if (c == transparent) { 
            buf(x) = 0 
          } 
          x += 1 
        } 
        masked.setRGB(0, y, w, 1, buf, 0, 1) 
        y += 1 
      }
      
      val resized = new BufferedImage(
        standardColorModel, 
        standardColorModel.createCompatibleWritableRaster(targetWidth, targetHeight), 
        standardColorModel.isAlphaPremultiplied, null)
      
      resize(masked, resized) 
      (resized, "png") 
    } else if (sourceColorModel.isInstanceOf[IndexColorModel]){ 
      // The input color model is indexed, and we know we won't be able 
      // to generate a tolerable palette to make another indexed color model, 
      // so use sRGB and upgrade to PNG. 
      val resized = new BufferedImage(
        standardColorModel, 
        standardColorModel.createCompatibleWritableRaster(targetWidth, targetHeight), 
        standardColorModel.isAlphaPremultiplied, null) 
      
      resize(source, resized) 
      (resized, "png") 
    } else { 
      val resized = new BufferedImage(
        targetColorModel, 
        targetColorModel.createCompatibleWritableRaster(targetWidth, targetHeight), 
        targetColorModel.isAlphaPremultiplied, null) 
      
      resize(source, resized) 
      (resized, inputFormat) 
    } 
  } 
}

}
}
