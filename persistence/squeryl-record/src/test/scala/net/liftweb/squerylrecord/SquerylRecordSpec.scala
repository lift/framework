/*
 * Copyright 2010-2011 WorldWide Conferencing, LLC
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

import org.squeryl.Session
import org.squeryl.dsl.ast.FunctionNode
import org.squeryl.internals.OutMapper
import org.squeryl.dsl.StringExpression
import org.squeryl.dsl.DateExpression

import org.specs2.mutable.Specification
import org.specs2.specification.AroundExample
import org.specs2.execute.{ AsResult , Result }

import record.{ BaseField, Record }
import record.field._
import RecordTypeMode._
import MySchema.{ TestData => td, _ }
import java.util.Calendar
import java.sql.Timestamp
import java.text.SimpleDateFormat
import org.squeryl.adapters.PostgreSqlAdapter
import common.Empty
import http.{LiftSession, S}
import util.Helpers

/**
 * Systems under specification for SquerylRecord.
 */
class SquerylRecordSpec extends Specification with AroundExample {
  "SquerylRecord Specification".title
  sequential

  lazy val session = new LiftSession("", Helpers.randomString(20), Empty)
  // One of these is for specs2 2.x, the other for specs2 1.x
  protected def around[T <% Result](t: =>T) = {
    S.initIfUninitted(session) {
      DBHelper.initSquerylRecordWithInMemoryDB()
      DBHelper.createSchema()
      t
    }
  }

  protected def around[T : AsResult](t: =>T) = {
    S.initIfUninitted(session) {
      DBHelper.initSquerylRecordWithInMemoryDB()
      DBHelper.createSchema()
      AsResult(t)
    }
  }

  "SquerylRecord" should {
    "load record by ID" in {
      transaction {
        S.initIfUninitted(session) {
          val company = companies.lookup(td.c2.id)
          checkCompaniesEqual(company.get, td.c2)

          val employee = employees.lookup(td.e1.id)
          checkEmployeesEqual(employee.get, td.e1)
        }
      }
    }

    "load record by string field value" in {
      transaction {
        S.initIfUninitted(session){
          val company = from(companies)(c =>
            where(c.name === td.c1.name.get) select (c))
          checkCompaniesEqual(company.single, td.c1)
        }
      }
    }

    "support order by" in {
      transaction {
        val orderedCompanies = from(companies)(c =>
          select(c) orderBy (c.name))
        val ids = orderedCompanies.map(_.id)
        // NOTE: This circumvents implicit conversion for the contents on List
        // ids must containInOrder(
        //   td.allCompanies.sortBy(_.name.get).map(_.id))
        ids.mkString("(", ",", ")") must_== td.allCompanies.sortBy(_.name.get).map(_.id).mkString("(", ",", ")")
      }
    }

    "support normal joins" in {
      transaction {
        val companiesWithEmployees = from(companies, employees)((c, e) =>
          where(c.id === e.companyId.get)
            select ((c.id, e.id))).toList
        companiesWithEmployees must haveSize(td.allEmployees.size)
        companiesWithEmployees must containAllOf(td.allEmployees map { e => (e.companyId.get, e.id) })
      }
    }

    "support left outer joins" in {
      transaction {
        S.initIfUninitted(session){
          val companiesWithEmployees = join(companies, employees.leftOuter)((c, e) =>
            select(c, e)
              on (c.id === e.map(_.companyId))
          )

          companiesWithEmployees must haveSize(4)
          // One company doesn't have an employee, two have
          companiesWithEmployees.filter(ce => ce._2.isEmpty) must haveSize(1)

          val companiesAndEmployeesWithSameName = join(companies, employees.leftOuter)((c, e) =>
            groupBy(c.id)
              compute (countDistinct(e.map(_.id)))
              on (c.name === e.map(_.name))
          )

          // There are three companies
          companiesAndEmployeesWithSameName must haveSize(3)
          // One company has the same name as an employee, two don't
          companiesAndEmployeesWithSameName.filter(ce => ce.measures == 0) must haveSize(2)

          val employeesWithSameAdminSetting = join(employees, employees.leftOuter)((e1, e2) =>
            select(e1, e2)
              on (e1.admin === e2.map(_.admin))
          )

          employeesWithSameAdminSetting.foreach { ee =>
            ee._2 must not(beEmpty)
          }

          val companiesWithSameCreationDate = join(companies, companies.leftOuter)((c1, c2) =>
            select(c1, c2)
              on (c1.created === c2.map(_.created))
          )
          companiesWithSameCreationDate must not(beEmpty)

          val employeesWithSameDepartmentNumber = join(employees, employees.leftOuter)((e1, e2) =>
            select(e1, e2)
              on (e1.departmentNumber === e2.map(_.departmentNumber))
          )
          employeesWithSameDepartmentNumber must not(beEmpty)

          val employeesWithSameRoles = join(employees, employees.leftOuter)((e1, e2) =>
            select(e1, e2)
              on (e1.role === e2.map(_.role))
          )
          employeesWithSameRoles must not(beEmpty)
        }
      }
    }

    "support one to many relations" in {
      transaction {
        val company = companies.lookup(td.c1.id)
        company must beSome[Company]
        val employees = company.get.employees
        employees must haveSize(1)
        checkEmployeesEqual(td.e1, employees.head)
        employees.associate(td.e3)
        td.e3.companyId.get must_== company.get.id
      }
    }

    "support many to many relations" in {
      transactionWithRollback {
        td.e1.rooms must haveSize(2)

        td.e2.rooms must beEmpty

        td.r1.employees must haveSize(1)
        td.r3.employees must beEmpty

        td.r3.employees.associate(td.e2)
        td.e2.rooms must haveSize(1)
      }
    }

    "support updates" in {
      val id = td.c1.id

      transactionWithRollback {
        S.initIfUninitted(session) {
          val company = companies.lookup(id).get
          company.name("New Name")
          company.postCode("11111")
          companies.update(company)

          val loaded = companies.lookup(id).get
          checkCompaniesEqual(company, loaded)

          update(companies)(c => where(c.id === id)
            set (c.name := "Name2"))
          val afterPartialUpdate = companies.lookup(id).get
          afterPartialUpdate.name.get must_== "Name2"
        }
      }

      // After rollback, the company should still be the same:
      transaction {
        S.initIfUninitted(session) {
          val company = companies.lookup(id).get
          checkCompaniesEqual(td.c1, company)
        }
      }
    }

    "support delete" in {
      transactionWithRollback {
        employees.delete(td.e2.id)
        employees.lookup(td.e2.id) must beNone
      }
    }

    "support select with properties of formerly fetched objects" in {
      transaction {
        S.initIfUninitted(session) {
          val company = companies.lookup(td.c2.id).head
          val employee = from(employees)(e =>
            where(e.companyId === company.idField) select (e)).head
          employee.id must_== td.e2.id

          val loadedCompanies = from(companies)(c =>
            where(c.created === company.created) select (c))
          loadedCompanies.size must beGreaterThanOrEqualTo(1)
        }
      }
    }

    "support many to many relations" >> {
      transactionWithRollback {
        td.e1.rooms must haveSize(2)
      }
    }

    "support date/time queries" >> {
      transaction {
        val c1 = from(companies)(c =>
          where(c.created <= Calendar.getInstance)
            select (c))
        c1.size must beGreaterThan(1)

        val c2 = from(companies)(c =>
          where(c.created <= Calendar.getInstance.getTime)
            select (c))
        c2.size must beGreaterThan(1)
      }
    }

    "support inner queries" >> {
      import record.field._

      transaction {
        // Should work with the ID function (returns a long):
        val companyId: Long = from(companies)(c => where(c.id in
          from(companies)(c2 => where(c2.id === td.c1.id) select (c2.id)))
          select (c.id)).single
        companyId must_== td.c1.id

        // It should also be possible to select the ID field directly:
        val companyIdField: LongField[Company] = from(companies)(c => where(c.idField in
          from(companies)(c2 => where(c2.id === td.c1.id) select (c2.idField)))
          select (c.idField)).single
        companyIdField.get must_== td.c1.id

        // Strings should also be selectable in inner queries
        val companyIdByName: Long = from(companies)(c => where(c.name in
          from(companies)(c2 => where(c2.name === td.c1.name) select (c2.name)))
          select (c.id)).single
        companyIdByName must_== td.c1.id

        // ...And DateTime-Fields:
        val companyIdByCreated: DateTimeField[Company] = from(companies)(c => where(c.created in
          from(companies)(c2 => where(c2.id === td.c1.id) select (c2.created)))
          select (c.created)).single
        companyIdByCreated.get must_== td.c1.created.get

        // Decimal Fields:
        val empSalary: DecimalField[Employee] = from(employees)(e => where(e.salary in
          from(employees)(e2 => where(e2.id === td.e1.id) select (e2.salary)))
          select (e.salary)).single
        empSalary.get must_== td.e1.salary.get

        // Email fields:
        val empEmail: EmailField[Employee] = from(employees)(e => where(e.email in
          from(employees)(e2 => where(e2.id === td.e1.id) select (e2.email)))
          select (e.email)).single
        empSalary.get must_== td.e1.salary.get

        // Boolean fields:
        val empAdmin: BooleanField[Employee] = from(employees)(e => where(e.admin in
          from(employees)(e2 => where(e2.id === td.e2.id) select (e2.admin)))
          select (e.admin)).single
        empAdmin.get must_== td.e2.admin.get

        // Enum fields:
        val empRoleQuery = from(employees)(e => where(e.role in
          from(employees)(e2 => where(e2.id === td.e2.id) select (e2.role)))
          select (e.role.get))
        val empRole = empRoleQuery.single
        empRole must_== td.e2.role.get
      }

    }

    "support the CRUDify trait" >> {
      transaction {
        val company = Company.create.name("CRUDify Company").
          created(Calendar.getInstance()).
          country(Countries.USA).postCode("90210")
        val bridge = Company.buildBridge(company)
        bridge.save
        val id = company.id
        company.isPersisted must_== true
        id must be_>(0l)
        company.postCode("10001")
        bridge.save
        val company2 = Company.findForParam(id.toString)
        company2.isDefined must_== true
        company2.foreach(c2 => {
          c2.postCode.get must_== "10001"
        })
        val allCompanies = Company.findForList(0, 1000)
        allCompanies.size must be_>(0)
        bridge.delete_!
        val allCompanies2 = Company.findForList(0, 1000)
        allCompanies2.size must_== (allCompanies.size - 1)
      }
    }

    "Support Optimistic Locking" >> {
      val company = Company.create.name("Optimistic Company").
        created(Calendar.getInstance()).
        country(Countries.USA).
        postCode("90210")
      //First insert the company in one transaction
      transaction {
        companies.insert(company)
      }
      //Retrieve and modify in another transaction
      val innerUpdate = new Thread(new Runnable {
        override def run() {
          transaction {
            val company2 = companies.lookup(company.id).get
            company2.created(Calendar.getInstance())
            companies.update(company2)
          }
        }
      })
      innerUpdate.start
      innerUpdate.join
      //Then in a third transaction, try to update the original object
      transaction {
        import org.squeryl.StaleUpdateException
        company.created(Calendar.getInstance())
        companies.update(company) must throwAn[StaleUpdateException]
      }
    }

    "Allow custom functions" in {
      inTransaction {
        val created =
          from(companies)(c =>
            where(c.name === "First Company USA")
              select (&(toChar(c.created, "EEE, d MMM yyyy")))
          )
        created.head must_== new SimpleDateFormat("EEE, d MMM yyyy").format(Calendar.getInstance().getTime())
      }
    }

    "Support precision and scale taken from DecimalTypedField" >> {
      val posoMetaData = companies.posoMetaData
      val fieldMetaData = posoMetaData.findFieldMetaDataForProperty("employeeSatisfaction").get
      val columnDefinition = new PostgreSqlAdapter().writeColumnDeclaration(fieldMetaData, false, MySchema)
      columnDefinition.endsWith("numeric(" + Company.employeeSatisfaction.context.getPrecision() +"," + Company.employeeSatisfaction.scale + ")") must_== true
    }

    "Properly reset the dirty_? flag after loading entities" >> inTransaction {
      val company = from(companies)(company =>
        select(company)).page(0, 1).single
      company.allFields map { f => f.dirty_? must_== false }
      success
    }
  }
  class ToChar(d: DateExpression[Timestamp], e: StringExpression[String], m: OutMapper[String])
    extends FunctionNode[String]("FORMATDATETIME", Some(m), Seq(d, e)) with StringExpression[String]

  def toChar(d: DateExpression[Timestamp], e: StringExpression[String])(implicit m: OutMapper[String]) = new ToChar(d, e, m)

  class TransactionRollbackException extends RuntimeException

  /**
   * Runs the given code in a transaction and rolls
   * back the transaction afterwards.
   */
  private def transactionWithRollback[T](code: => T): T = {

    def rollback: Unit = throw new TransactionRollbackException()

    var result: T = null.asInstanceOf[T]
    try {
      transaction {
        result = code
        rollback
      }
    } catch {
      case e: TransactionRollbackException => // OK, was rolled back
    }

    result
  }

  private def checkCompaniesEqual(c1: Company, c2: Company): Result = {
    val cmp = new RecordComparer[Company](c1, c2)
    cmp.check(_.idField)
    cmp.check(_.description)
    cmp.check(_.country)
    cmp.check(_.postCode)
    cmp.check(_.created)

    cmp.checkXHtml()
  }

  private def checkEmployeesEqual(e1: Employee, e2: Employee): Result = {
    val cmp = new RecordComparer[Employee](e1, e2)
    cmp.check(_.name)
    cmp.check(_.companyId)
    cmp.check(_.email)
    cmp.check(_.salary)
    cmp.check(_.locale)
    cmp.check(_.timeZone)
    //cmp.check(_.password)
    cmp.check(_.admin)
    cmp.check(_.departmentNumber)
    cmp.check(_.role)

    // Photo must be checked separately
    e1.photo.get match {
      case Some(p) => {
        val p2 = e2.photo.get
        p2 must beSome[Array[Byte]]
        p2.get.size must_== p.size

        (0 until p.size) map { i =>
          p2.get(i) must_== p(i)
        }
      }
      case None => e2.photo.get must beNone
    }
  }

  class RecordComparer[T <: Record[T]](val r1: T, val r2: T) {
    def check(fieldExtractor: (T) => BaseField): Result = {
      val f1 = fieldExtractor(r1)
      val f2 = fieldExtractor(r2)
      f1.get must_== f2.get
      f1.name must_== f2.name
    }

    def checkXHtml(): Result =
      r1.toXHtml must_== r2.toXHtml
  }
}
