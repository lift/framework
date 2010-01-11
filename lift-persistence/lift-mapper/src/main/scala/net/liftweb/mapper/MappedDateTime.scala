package net.liftweb.mapper

/*
 * Copyright 2006-2010 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions
 * and limitations under the License.
 */

import _root_.java.sql.{ResultSet, Types}
import _root_.java.util.Date
import _root_.java.lang.reflect.Method

import _root_.net.liftweb._
import util._
import common._
import Helpers._
import http._
import json._
import S._
import js._

import _root_.scala.xml.{NodeSeq}

abstract class MappedDateTime[T<:Mapper[T]](val fieldOwner: T) extends MappedField[Date, T] {
  private val data = FatLazy(defaultValue)
  private val orgData = FatLazy(defaultValue)

  protected def real_i_set_!(value: Date): Date = {
    if (value != data.get) {
      data() = value
      this.dirty_?( true)
    }
    data.get
  }

  def dbFieldClass = classOf[Date]

  def asJsonValue: JsonAST.JValue = is match {
    case null => JsonAST.JNull
    case v => JsonAST.JInt(v.getTime)
  }

  def toLong: Long = is match {
    case null => 0L
    case d: Date => d.getTime / 1000L
  }

  def asJsExp = JE.Num(toLong)

  /**
   * Get the JDBC SQL Type for this field
   */
  def targetSQLType = Types.TIMESTAMP

  def defaultValue: Date = null
  // private val defaultValue_i = new Date

  override def writePermission_? = true
  override def readPermission_? = true

  protected def i_is_! = data.get
  protected def i_was_! = orgData.get
  protected[mapper] def doneWithSave() {orgData.setFrom(data)}

  protected def i_obscure_!(in : Date) : Date = {
    new Date(0L)
  }

  /**
   * Create an input field for the item
   */
  override def _toForm: Box[NodeSeq] =
  S.fmapFunc({s: List[String] => this.setFromAny(s)}){funcName =>
  Full(<input type='text' id={fieldId}
      name={funcName}
      value={toString}/>)
  }

  override def setFromAny(f: Any): Date = f match {
    case JsonAST.JNull => this.set(null)
    case JsonAST.JInt(v) => this.set(new Date(v.longValue))
    case s: String => LiftRules.parseDate(s).map(d => this.set(d)).openOr(this.is)
    case (s: String) :: _ => LiftRules.parseDate(s).map(d => this.set(d)).openOr(this.is)
    case _ => this.is
  }

  def jdbcFriendly(field : String) : Object = is match {
    case null => null
    case d => new _root_.java.sql.Timestamp(d.getTime)
  }

  def real_convertToJDBCFriendly(value: Date): Object = if (value == null) null else new _root_.java.sql.Timestamp(value.getTime)

  private def st(in: Box[Date]): Unit =
  in match {
    case Full(d) => data.set(d); orgData.set(d)
    case _ => data.set(null); orgData.set(null)
  }

  def buildSetActualValue(accessor: Method, v: AnyRef, columnName: String): (T, AnyRef) => Unit =
  (inst, v) => doField(inst, accessor, {case f: MappedDateTime[T] => f.st(toDate(v))})

  def buildSetLongValue(accessor: Method, columnName: String): (T, Long, Boolean) => Unit =
  (inst, v, isNull) => doField(inst, accessor, {case f: MappedDateTime[T] => f.st(if (isNull) Empty else Full(new Date(v)))})

  def buildSetStringValue(accessor: Method, columnName: String): (T, String) => Unit =
  (inst, v) => doField(inst, accessor, {case f: MappedDateTime[T] => f.st(toDate(v))})

  def buildSetDateValue(accessor: Method, columnName: String): (T, Date) => Unit =
  (inst, v) => doField(inst, accessor, {case f: MappedDateTime[T] => f.st(Full(v))})

  def buildSetBooleanValue(accessor: Method, columnName: String): (T, Boolean, Boolean) => Unit =
  (inst, v, isNull) => doField(inst, accessor, {case f: MappedDateTime[T] => f.st(Empty)})

  /**
   * Given the driver type, return the string required to create the column in the database
   */
  def fieldCreatorString(dbType: DriverType, colName: String): String = colName + " " + dbType.dateTimeColumnType + notNullAppender()

  def inFuture_? = data.get match {
    case null => false
    case d => d.getTime > millis
  }
  def inPast_? = data.get match {
    case null => false
    case d => d.getTime < millis
  }

  override def toString: String = LiftRules.formatDate(is)
}
