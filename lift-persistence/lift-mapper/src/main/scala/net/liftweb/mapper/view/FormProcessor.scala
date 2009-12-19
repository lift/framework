package net.liftweb.mapper.view

import scala.xml.{Group, Elem, NodeSeq}
import scala.collection.mutable.{Map => mMap}

import net.liftweb.http.SHtml
import net.liftweb.util.{Helpers}
import net.liftweb.common.{Full, Empty}


/**
 * This class can be used to act on all submitted values in one block of code.
 * This enables one to use try/catch when setting values etc.
 * @author nafg
 */
abstract class FormProcessor(prefix: String) {
  
  def text(attrs: (String, String)*)(init: String, action: String=>Unit) = SHtml.text(init, action, attrs:_*)
  def text: (String,String=>Unit)=>NodeSeq  = text()_
  def checkbox(attrs: (String, String)*)(init: Boolean, action: Boolean=>Unit) = SHtml.checkbox(init, action, attrs:_*)
  def checkbox: (Boolean, Boolean=>Unit)=>NodeSeq = checkbox()_
  
  val stringValues: mMap[String, String] = mMap.empty[String, String]
  val strings = mMap.empty[String, (String,String=>Unit)=>NodeSeq] 

  val booleanValues = mMap.empty[String, Boolean]
  val booleans = mMap.empty[String, (Boolean,Boolean=>Unit)=>NodeSeq]
  

  
  
  def bind(xhtml: NodeSeq) = {
    def transform(node: NodeSeq): NodeSeq = {
      put
      node match {
      	case Elem(`prefix`, label, _, _, _*) if strings.keys contains label =>
      	  strings(label)(stringValues(label), stringValues(label) = _)
      	case Elem(`prefix`, label, _, _, _*) if booleans.keys contains label =>
          booleans(label)(booleanValues(label), booleanValues(label) = _)
       case other => other
      }
    }
    Helpers.bind(prefix, Full(transform _), Empty, xhtml) ++
      Seq(SHtml.hidden(()=>get))
    
  }

  
  def put: Unit
  def get: Unit
}
