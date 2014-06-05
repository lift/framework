/*
 * Copyright 2006-2011 WorldWide Conferencing, LLC
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
package mapper

import net.liftweb.util.Helpers._
import net.liftweb.util.FatLazy
import java.sql.{ResultSet, Types}
import java.lang.reflect.Method
import scala.xml.{Node, Text, NodeSeq}
import java.util.Date
import net.liftweb.http.{S}
import net.liftweb.http.S._
import net.liftweb.util._
import net.liftweb.json._
import net.liftweb.common._
import net.liftweb.http.js._

object MappedPassword {
  val blankPw = "*******"

  /**
   * Set this in boot if you want Bcrypt salt strength to be
   * something more than the default
   */
  var bcryptStrength: Box[Int] = None
}

abstract class MappedPassword[T<:Mapper[T]](val fieldOwner: T)
extends MappedField[String, T] {
  override def dbColumnCount = 2
  def dbFieldClass = classOf[String]

  override def dbColumnNames(in : String) = in.toLowerCase+"_pw" :: in.toLowerCase+"_slt" :: Nil

  override lazy val dbSelectString =
  dbColumnNames(name).
  map(cn => fieldOwner.getSingleton._dbTableNameLC + "." + cn).
  mkString(", ")

   def asJsonValue: Box[JsonAST.JValue] = Full(JsonAST.JNull)

  def salt = this.salt_i

  import scala.reflect.runtime.universe._
  def manifest: TypeTag[String] = typeTag[String]

  /**
   * Get the source field metadata for the field
   * @return the source field metadata for the field
   */
  def sourceInfoMetadata(): SourceFieldMetadata{type ST = String} =
    SourceFieldMetadataRep(name, manifest, new FieldConverter {
      /**
       * The type of the field
       */
      type T = String

      /**
       * Convert the field to a String
       * @param v the field value
       * @return the string representation of the field value
       */
      def asString(v: T): String = ""

      /**
       * Convert the field into NodeSeq, if possible
       * @param v the field value
       * @return a NodeSeq if the field can be represented as one
       */
      def asNodeSeq(v: T): Box[NodeSeq] = Empty

      /**
       * Convert the field into a JSON value
       * @param v the field value
       * @return the JSON representation of the field
       */
      def asJson(v: T): Box[JValue] = Empty

      /**
       * If the field can represent a sequence of SourceFields,
       * get that
       * @param v the field value
       * @return the field as a sequence of SourceFields
       */
      def asSeq(v: T): Box[Seq[SourceFieldInfo]] = Empty
    })


  private var password = FatLazy(defaultValue)
  private val salt_i = FatLazy(util.Safe.randomString(16))
  private var invalidPw = false
  private var invalidMsg = ""

  protected def real_i_set_!(value : String) : String = {
    value match {
      case "*" | null | MappedPassword.blankPw if (value.length < 3) =>
       invalidPw = true ; invalidMsg = S.?("password.must.be.set") ; password.set("*")
      case MappedPassword.blankPw => return "*"
      case _ if (value.length > 4) => invalidPw = false;
      val bcrypted = BCrypt.hashpw(value, MappedPassword.bcryptStrength.map(BCrypt.gensalt(_)) openOr BCrypt.gensalt())
      password.set("b;"+bcrypted.substring(0,44))
      salt_i.set(bcrypted.substring(44))
      case _ => invalidPw = true ; invalidMsg = S.?("password.too.short"); password.set("*")
    }
    this.dirty_?( true)
    "*"
  }

  def setList(in: List[String]): Boolean =
  in match {
    case x1 :: x2 :: Nil if x1 == x2 => this.set(x1) ; true
    case _ => invalidPw = true; invalidMsg = S.?("passwords.do.not.match"); false
  }


  override def setFromAny(f: Any): String = {
    f match {
      case a : Array[String] if (a.length == 2 && a(0) == a(1)) =>
        this.set(a(0))
      case l : List[_] if (l.length == 2 && l.head == l(1)) =>
        this.set(l.head.asInstanceOf[String])
      case _ => 
        invalidPw = true
        invalidMsg = S.?("passwords.do.not.match")
    }
    get
  }

  override def renderJs_? = false

  def asJsExp: JsExp = throw new NullPointerException("No way")

  /**
   * Test to see if an incoming password matches
   * @param toMatch the password to test
   * @return the matched value
   */
  def match_?(toMatch : String): Boolean = {
    if (password.get.startsWith("b;")) {
      BCrypt.checkpw(toMatch, password.get.substring(2)+salt_i.get)
    } else
    hash("{"+toMatch+"} salt={"+salt_i.get+"}") == password.get
  }

  override def validate : List[FieldError] = {
    if (!invalidPw && password.get != "*") Nil
    else if (invalidPw) List(FieldError(this, Text(invalidMsg)))
    else List(FieldError(this, Text(S.?("password.must.be.set"))))
  }

  def real_convertToJDBCFriendly(value: String): Object =
    BCrypt.hashpw(value, MappedPassword.bcryptStrength.map(BCrypt.gensalt(_)) openOr BCrypt.gensalt())

  /**
   * Get the JDBC SQL Type for this field
   */
  def targetSQLType = Types.VARCHAR

  def defaultValue = "*"

  override def writePermission_? = true
  override def readPermission_? = true

  protected def i_is_! = MappedPassword.blankPw
  protected def i_was_! = MappedPassword.blankPw
  /**
   * Called after the field is saved to the database
   */
  override protected[mapper] def doneWithSave() {
  }

  protected def i_obscure_!(in : String) : String = in

  /**
   * Create an input field for the item
   */
  override def _toForm: Box[NodeSeq] = {
    S.fmapFunc({s: List[String] => this.setFromAny(s)}){funcName =>
      Full(<span>{appendFieldId(<input type={formInputType} name={funcName}
            value={get.toString}/>)}&nbsp;{S.?("repeat")}&nbsp;<input
            type={formInputType} name={funcName}
            value={get.toString}/></span>)
    }
  }

  /**
   * When building the form field, what's the input element's
   * type attribute.
   */
  override protected def formInputType = "password"


  def jdbcFriendly(columnName : String) = {
    if (columnName.endsWith("_slt")) {
      salt_i.get
    } else if (columnName.endsWith("_pw")) {
      password.get
    } else {
      null
    }
  }

  def buildSetLongValue(accessor : Method, columnName : String) : (T, Long, Boolean) => Unit = {
    if (columnName.endsWith("_slt")) {
      {(inst : T, v: Long, isNull: Boolean ) => {val tv = getField(inst, accessor).asInstanceOf[MappedPassword[T]]; tv.salt_i() = if (isNull) null else v.toString}}
    } else if (columnName.endsWith("_pw")) {
      {(inst : T, v: Long, isNull: Boolean ) => {val tv = getField(inst, accessor).asInstanceOf[MappedPassword[T]]; tv.password() = if (isNull) null else v.toString}}
    } else {
      null
    }
  }
  def buildSetStringValue(accessor : Method, columnName : String) : (T, String) => Unit  = {
    if (columnName.endsWith("_slt")) {
      {(inst : T, v: String ) => {val tv = getField(inst, accessor).asInstanceOf[MappedPassword[T]]; tv.salt_i() = v}}
    } else if (columnName.endsWith("_pw")) {
      {(inst : T, v: String ) => {val tv = getField(inst, accessor).asInstanceOf[MappedPassword[T]]; tv.password() = v}}
    } else {
      null
    }
  }
  def buildSetDateValue(accessor : Method, columnName : String) : (T, Date) => Unit   = {
    null
  }
  def buildSetBooleanValue(accessor : Method, columnName : String) : (T, Boolean, Boolean) => Unit   = {
    null
  }

  def buildSetActualValue(accessor : Method, inst : AnyRef, columnName : String) : (T, AnyRef) => Unit = {
    if (columnName.endsWith("_slt")) {
      inst match {
        case null => {(inst : T, v : AnyRef) => {}}
        case _ => {(inst : T, v : AnyRef) => {val tv = getField(inst, accessor).asInstanceOf[MappedPassword[T]]; tv.salt_i() = (if (v == null) null else v.toString); tv.resetDirty}}
      }
    } else if (columnName.endsWith("_pw")) {
      inst match {
        case null => {(inst : T, v : AnyRef) => {}}
        case _ => {(inst : T, v : AnyRef) => {val tv = getField(inst, accessor).asInstanceOf[MappedPassword[T]]; tv.password() = (if (v == null) null else v.toString); tv.resetDirty}}
      }

    } else {
      null
    }
  }

  /**
   * Given the driver type, return the string required to create the column in the database
   */
  def fieldCreatorString(dbType: DriverType, colName: String): String = (if (colName.endsWith("_pw")) colName+" VARCHAR(48)" else colName+" VARCHAR(20)")  + notNullAppender()
}
