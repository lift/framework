package net.liftweb.mongodb

import org.bson.codecs._

/**
  * additional codecs to register for primitive types
  */
class LongPrimitiveCodec extends LongCodec {
  override def getEncoderClass() = java.lang.Long.TYPE
}

class IntegerPrimitiveCodec extends IntegerCodec {
  override def getEncoderClass() = java.lang.Integer.TYPE
}
