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

import org.specs.Specification
import record.{BaseField, Record}
import RecordTypeMode._
import MySchema.{TestData => td, _}
import java.util.Calendar


/**
 * Systems under specification for SquerylRecord.
 */
object SquerylRecordSpec extends Specification("SquerylRecord Specification") {

  doBeforeSpec {
    DBHelper.initSquerylRecordWithInMemoryDB()
    DBHelper.createSchema()
  }

  // NOTE: Use explicit forExample() in the examples to avoid
  // implicit ambiguity with Specs 1.6.6 in Scala 2.8.0
  "SquerylRecord" should {

    forExample("load record by ID") in {
      transaction {
        val company = companies.lookup(td.c2.id)
        checkCompaniesEqual(company.get, td.c2)

        val employee = employees.lookup(td.e1.id)
        checkEmployeesEqual(employee.get, td.e1)
      }
    }

    forExample("load record by string field value") in {
      transaction {
        val company = from(companies)(c =>
          where(c.name === td.c1.name.is) select (c))
        checkCompaniesEqual(company.single, td.c1)
      }
    }

    forExample("support order by") in {
      transaction {
        val orderedCompanies = from(companies)(c =>
          select(c) orderBy (c.name))
        val ids = orderedCompanies.map(_.id)
        // NOTE: This circumvents implicit conversion for the contents on List
        // ids must containInOrder(
        //   td.allCompanies.sortBy(_.name.is).map(_.id))
        ids.mkString("(", ",", ")") must_== td.allCompanies.sortBy(_.name.is).map(_.id).mkString("(", ",", ")")
      }
    }

    forExample("support normal joins") in {
      transaction {
        val companiesWithEmployees = from(companies, employees)((c, e) =>
          where(c.id === e.id)
            select ((c, e)))
        val ids = companiesWithEmployees.map(entry => (entry._1.id,
          entry._2.id))
        ids must haveSize(2)
        ids must containAll(List((td.c1.id, td.e1.id),
          (td.c2.id, td.e2.id)))
      }
    }

    forExample("support left outer joins") in {
      transaction {
        val companiesWithEmployees = join(companies, employees.leftOuter)((c, e) =>
          select(c, e)
          on(c.id === e.map(_.companyId))
        )

        companiesWithEmployees must haveSize(3)
        // One company doesn't have an employee, two have
        companiesWithEmployees.filter(ce => ce._2.isEmpty) must haveSize(1)

        val companiesAndEmployeesWithSameName = join(companies, employees.leftOuter)((c, e) =>
          groupBy(c.id)
          compute(countDistinct(e.map(_.id)))
          on(c.name === e.map(_.name))
        )

        // There are three companies
        companiesAndEmployeesWithSameName must haveSize(3)
        // One company has the same name as an employee, two don't
        companiesAndEmployeesWithSameName.filter(ce => ce.measures == 0) must haveSize(2)

        val employeesWithSameAdminSetting = join(employees, employees.leftOuter)((e1, e2) =>
          select(e1, e2)
          on(e1.admin === e2.map(_.admin))
        )

        // two employees, both have distinct admin settings
        employeesWithSameAdminSetting must haveSize(2)
        employeesWithSameAdminSetting.foreach { ee =>
          ee._2 must not (beEmpty)
          ee._1.id must_== ee._2.get.id
        }

        val companiesWithSameCreationDate = join(companies, companies.leftOuter)((c1, c2) =>
          select(c1, c2)
          on(c1.created === c2.map(_.created))
        )
        companiesWithSameCreationDate must not (beEmpty)

        val employeesWithSameDepartmentNumber = join(employees, employees.leftOuter)((e1, e2) =>
          select(e1, e2)
          on(e1.departmentNumber === e2.map(_.departmentNumber))
        )
        employeesWithSameDepartmentNumber must not (beEmpty)

        val employeesWithSameRoles = join(employees, employees.leftOuter)((e1, e2) =>
          select(e1, e2)
          on(e1.role === e2.map(_.role))
        )
        employeesWithSameRoles must not (beEmpty)
      }
    }

    forExample("support one to many relations") in {
      transaction {
        val company = companies.lookup(td.c1.id)
        company must beSome[Company]
        val employees = company.get.employees
        employees must haveSize(1)
        checkEmployeesEqual(td.e1, employees.head)
      }
    }

    forExample("support many to many relations") in {
      transactionWithRollback {
        td.e1.rooms must haveSize(2)

        td.e2.rooms must beEmpty

        td.r1.employees must haveSize(1)
        td.r3.employees must beEmpty

        td.r3.employees.associate(td.e2)
        td.e2.rooms must haveSize(1)
      }
    }

    forExample("support updates") in {
      val id = td.c1.id

      transactionWithRollback {
        val company = companies.lookup(id).get
        company.name("New Name")
        company.postCode("11111")
        companies.update(company)

        val loaded = companies.lookup(id).get
        checkCompaniesEqual(company, loaded)

        update(companies)(c => where(c.id === id)
          set (c.name := "Name2"))
        val afterPartialUpdate = companies.lookup(id).get
        afterPartialUpdate.name.is must_== "Name2"
      }

      // After rollback, the company should still be the same:
      transaction {
        val company = companies.lookup(id).get
        checkCompaniesEqual(td.c1, company)
      }
    }

    forExample("support delete") in {
      transactionWithRollback {
        employees.delete(td.e2.id)
        employees.lookup(td.e2.id) must beNone
      }
    }

    forExample("support select with properties of formerly fetched objects") in {
      transaction {
        val company = companies.lookup(td.c2.id).head
        val employee = from(employees)(e =>
          where (e.companyId === company.idField) select(e)).head
        employee.id must_== td.e2.id

        val loadedCompanies = from(companies)(c =>
          where (c.created === company.created) select(c))
        loadedCompanies.size must beGreaterThanOrEqualTo(1)
      }
    }

    forExample("support associate with one-to-many relations") >> {
      transactionWithRollback {
        //td.c3.employees.associate(td.e2)
        //td.e2.company.id must_== td.c3.id
      }
    }
    
    forExample("support many to many relations") >> {
      transactionWithRollback {
        td.e1.rooms must haveSize(2)
      }
    }

    forExample("support date/time queries") >> {
      transaction {
	val c1 = from(companies)(c =>
	  where (c.created <= Calendar.getInstance)
	  select(c))
	c1.size must beGreaterThan(1)

	val c2 = from(companies)(c =>
	  where (c.created <= Calendar.getInstance.getTime)
	  select(c))
	c2.size must beGreaterThan(1)
      }
    }

    forExample("support inner queries") >> {
      import record.field._

      transaction {
        // Should work with the ID function (returns a long):
        val companyId: Long = from(companies)(c => where(c.id in 
            from(companies)(c2 => where(c2.id === td.c1.id) select(c2.id))) 
            select(c.id)).single
        companyId must_== td.c1.id
        
        // It should also be possible to select the ID field directly:
        val companyIdField: LongField[Company] = from(companies)(c => where(c.idField in 
            from(companies)(c2 => where(c2.id === td.c1.id) select(c2.idField))) 
            select(c.idField)).single
        companyIdField.is must_== td.c1.id
        
        // Strings should also be selectable in inner queries
        val companyIdByName: Long = from(companies)(c => where(c.name in 
            from(companies)(c2 => where(c2.name === td.c1.name) select(c2.name)))
            select(c.id)).single
        companyIdByName must_== td.c1.id
        
        // ...And DateTime-Fields:
        val companyIdByCreated: DateTimeField[Company] = from(companies)(c => where(c.created in 
            from(companies)(c2 => where(c2.id === td.c1.id) select(c2.created)))
            select(c.created)).single
        companyIdByCreated.is must_== td.c1.created.is
        
        // Decimal Fields:
        val empSalary: DecimalField[Employee] = from(employees)(e => where (e.salary in
            from(employees)(e2 => where(e2.id === td.e1.id) select(e2.salary)))
            select(e.salary)).single
        empSalary.is must_== td.e1.salary.is
           
        // Email fields:
        val empEmail: EmailField[Employee] = from(employees)(e => where (e.email in
            from(employees)(e2 => where(e2.id === td.e1.id) select(e2.email)))
            select(e.email)).single
        empSalary.is must_== td.e1.salary.is
        
        // Boolean fields:
        val empAdmin: BooleanField[Employee] = from(employees)(e => where (e.admin in
            from(employees)(e2 => where(e2.id === td.e1.id) select(e2.admin)))
            select(e.admin)).single
        empAdmin.is must_== td.e1.admin.is
        
        // Enum fields:
        val empRole: EnumNameField[_, _] = from(employees)(e => where (e.role in
            from(employees)(e2 => where(e2.id === td.e1.id) select(e2.role)))
            select(e.role)).single
        empRole.is must_== td.e1.role.is 
        
      }
    }

  }

  class TransactionRollbackException extends RuntimeException

  /**
   * Runs the given code in a transaction and rolls
   * back the transaction afterwards.
   */
  private def transactionWithRollback[T](code: => T): T = {
    var result: T = null.asInstanceOf[T]
    try {
      transaction {
        result = code
        throw new TransactionRollbackException()
      }
    } catch {
      case e: TransactionRollbackException => // OK, was rolled back
    }

    result
  }

  private def checkCompaniesEqual(c1: Company, c2: Company) {
    val cmp = new RecordComparer[Company](c1, c2)
    cmp.check(_.idField)
    cmp.check(_.description)
    cmp.check(_.country)
    cmp.check(_.postCode)
    cmp.check(_.created)

    cmp.checkXHtml()
  }

  private def checkEmployeesEqual(e1: Employee, e2: Employee) {
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
    e1.photo.is match {
      case Some(p) => {
        val p2 = e2.photo.is
        p2 must beSome[Array[Byte]]
        p2.get.size must_== p.size

        (0 until p.size).foreach { i =>
          p2.get(i) must_== p(i)
        }
      }
      case None => e2.photo.is must beNone
    }
  }

  class RecordComparer[T <: Record[T]](val r1: T, val r2: T) {
    def check(fieldExtractor: (T) => BaseField) {
      val f1 = fieldExtractor(r1)
      val f2 = fieldExtractor(r2)
      f1.get must_== f2.get
      f1.name must_== f2.name
    }

    def checkXHtml() {
      r1.toXHtml must_== r2.toXHtml
    }
  }
}
