/*
 * Copyright 2010 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package net.liftweb
package squerylrecord

import RecordTypeMode._
import record.{ MetaRecord, Record, TypedField, MandatoryTypedField }
import common.{ Box, Full }
import record.field._
import record.Field
import json.JsonAST.{ JValue, JString }
import http.js.JE.Str
import org.squeryl.{ SessionFactory, Session, Schema, Optimistic }
import org.squeryl.adapters.H2Adapter
import org.squeryl.annotations.Column
import org.squeryl.internals.AutoIncremented
import org.squeryl.internals.PrimaryKey
import org.squeryl.dsl.CompositeKey2
import org.squeryl.KeyedEntity
import java.math.MathContext
import java.sql.DriverManager
import java.util.Calendar

object DBHelper {
  def initSquerylRecordWithInMemoryDB() {
    SquerylRecord.initWithSquerylSession {
      // TODO: Use mapper.StandardDBVendor
      Class.forName("org.h2.Driver")
      val session = Session.create(DriverManager.getConnection("jdbc:h2:mem:testSquerylRecordDB;DB_CLOSE_DELAY=-1;LOCK_TIMEOUT=3000"), new H2Adapter)
      //session.setLogger(statement => println(statement))
      session
    }
  }

  /**
   * Creates the test schema in a new transaction. Drops an old schema if
   * it exists.
   */
  def createSchema() {
    inTransaction {
      try {
        //MySchema.printDdl
        MySchema.dropAndCreate
        MySchema.createTestData
      } catch {
        case e => e.printStackTrace()
          throw e;
      }
    }
  }
}

/**
 * Test Record: Company. It has many different field types for test purposes.
 */
class Company private () extends Record[Company] with KeyedRecord[Long] with Optimistic{

  override def meta = Company

  @Column(name = "id")
  override val idField = new LongField(this)

  val name = new StringField(this, "")
  val description = new OptionalTextareaField(this, 1000)
  val country = new CountryField(this)
  val postCode = new PostalCodeField(this, country)
  val created = new DateTimeField(this)
  val employeeSatisfaction = new OptionalDecimalField(this, new MathContext(10), 5)

  lazy val employees = MySchema.companyToEmployees.left(this)

}
object Company extends Company with MetaRecord[Company] with CRUDify[Long, Company]{
  
  def table = MySchema.companies
  
  def idFromString(in: String) = in.toLong
  
}

object EmployeeRole extends Enumeration {
  
  type EmployeeRole = Value

  val Programmer, Manager = Value
}

/**
 * A field type that works just like a String field.
 * Only for testing that custom fields derived from
 * TypedField are also supported.
 */
class SpecialField[OwnerType <: Record[OwnerType]](rec: OwnerType)
  extends Field[String, OwnerType] with TypedField[String]
  with SquerylRecordField with MandatoryTypedField[String] {

  override def owner = rec
  override def classOfPersistentField = classOf[String]
  override def defaultValue = ""
  override def setFromString(s: String) = setBox(Full(s))
  override def setFromAny(c: Any) = c match {
    case Full(v) => setBox(Full(v.toString))
    case None => setBox(None)
    case v => setBox(Full(v.toString))
  }
  override def setFromJValue(jValue: JValue) = setBox(Full(jValue.toString))
  override def asJValue = JString(get)
  override def asJs = Str(get)
  override def toForm = Full(scala.xml.Text(get))
}

/**
 * Test record: An employee belongs to a company.
 */
class Employee private () extends Record[Employee] with KeyedRecord[Long]{

  override def meta = Employee

  @Column(name = "id")
  override val idField = new LongField(this)

  val name = new SpecialField(this)
  val companyId = new LongField(this)
  val email = new EmailField(this, 100)
  val salary = new DecimalField(this, MathContext.UNLIMITED, 2)
  val locale = new LocaleField(this)
  val timeZone = new TimeZoneField(this)
  val password = new PasswordField(this)
  val photo = new OptionalBinaryField(this)
  val admin = new BooleanField(this)
  val departmentNumber = new IntField(this)
  val role = new EnumNameField(this, EmployeeRole)

  lazy val company = MySchema.companyToEmployees.right(this)
  lazy val rooms = MySchema.roomAssignments.left(this)

}
object Employee extends Employee with MetaRecord[Employee]

/**
 * Test record: One or more employees can have a room (one-to-many-relation).
 */
class Room private() extends Record[Room] with KeyedRecord[Long] {
  override def meta = Room

  override val idField = new LongField(this)

  val name = new StringField(this, 50)

  lazy val employees = MySchema.roomAssignments.right(this)
}

object Room extends Room with MetaRecord[Room]

/**
 * Relation table for assignments of rooms to employees.
 * This must not be a Record. However, it's ok if it is not
 * a record, because we won't use a relation table for
 * a web form or similar.
 */
class RoomAssignment(val employeeId: Long, val roomId: Long) extends KeyedEntity[CompositeKey2[Long,Long]] {
  def id = compositeKey(employeeId, roomId)
}


/**
 * Schema for the test database.
 */
object MySchema extends Schema {
  val companies = table[Company]
  val employees = table[Employee]
  val rooms = table[Room]

  val companyToEmployees =
    oneToManyRelation(companies, employees).via((c, e) => c.id === e.companyId)

  val roomAssignments = manyToManyRelation(employees, rooms).
    via[RoomAssignment]((employee, room, roomAssignment) =>
      (roomAssignment.employeeId === employee.idField, roomAssignment.roomId === room.idField))

  on(employees)(e =>
    declare(e.companyId defineAs (indexed("idx_employee_companyId")),
      e.email defineAs indexed("idx_employee_email")))

  /**
   * Drops an old schema if exists and then creates
   * the new schema.
   */
  def dropAndCreate {
    drop
    create
  }

  /**
   * Creates some test instances of companies and employees
   * and saves them in the database.
   */
  def createTestData {
    import TestData._

    allCompanies.foreach(companies.insert(_))
    allEmployees.foreach(employees.insert(_))
    allRooms.foreach(rooms.insert(_))
    
    e1.rooms.associate(r1)
    e1.rooms.associate(r2)
  }

  object TestData {
    
    val c1 = Company.createRecord.name("First Company USA").
      created(Calendar.getInstance()).
      country(Countries.USA).postCode("12345")
      
    val c2 = Company.createRecord.name("Second Company USA").
      created(Calendar.getInstance()).
      country(Countries.USA).postCode("54321")
      
    val c3 = Company.createRecord.name("Company or Employee").
      created(Calendar.getInstance()).
      country(Countries.Canada).postCode("1234")

    val allCompanies = List(c1, c2, c3)

    lazy val e1 = Employee.createRecord.companyId(c1.idField.get).
      name("Peter Example").
      email("peter@example.com").salary(BigDecimal(345)).
      locale(java.util.Locale.GERMAN.toString()).
      timeZone("Europe/Berlin").password("exampletest").
      admin(false).departmentNumber(2).role(EmployeeRole.Programmer).
      photo(Array[Byte](0, 1, 2, 3, 4))

    lazy val e2 = Employee.createRecord.companyId(c2.idField.get).
      name("Company or Employee").
      email("test@example.com").salary(BigDecimal("123.123")).
      locale(java.util.Locale.US.toString()).
      timeZone("America/Los_Angeles").password("test").
      admin(true).departmentNumber(1).role(EmployeeRole.Manager).
      photo(Array[Byte](1))

    lazy val e3 = Employee.createRecord.companyId(c2.idField.get).
      name("Joe Shmo").
      email("joe@shmo.com").salary(BigDecimal("100000.00")).
      locale(java.util.Locale.US.toString()).
      timeZone("America/Los_Angeles").password("test").
      admin(false).departmentNumber(1).role(EmployeeRole.Programmer).
      photo(Array[Byte](1))

    lazy val allEmployees = List(e1, e2, e3)
    
    val r1 = Room.createRecord.name("Room 1")
    
    val r2 = Room.createRecord.name("Room 2")
    
    val r3 = Room.createRecord.name("Room 3")
    
    val allRooms = List(r1, r2, r3)
  }
}

