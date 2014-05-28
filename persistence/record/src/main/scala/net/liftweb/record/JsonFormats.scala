package net.liftweb.record

import net.liftweb.json.{DefaultFormats, Formats}

trait JsonFormats {
  // override this for custom Formats
  def formats: Formats = DefaultFormats.lossless

  implicit lazy val _formats: Formats = formats

  lazy val allFormats = DefaultFormats.lossless + new DateSerializer + new DateTimeSerializer + new PatternSerializer + new UUIDSerializer
}
