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

package net.liftweb.squerylrecord

import org.specs.Specification
import org.specs.runner.JUnit4
import RecordTypeMode._
import net.liftweb.record.{ BaseField, Record }
import MySchema.{ TestData => td }
import MySchema.{ companies, employees }

/**
 * Class for running the specs tests with JUnit4.
 */
class SquerylRecordSpecsTest extends JUnit4(SquerylRecordSpecs)

object SquerylRecordSpecs extends Specification {

  doBeforeSpec {
    DBHelper.initSquerylRecordWithInMemoryDB()
    DBHelper.createSchema()
  }

  "SquerylRecord" should {

    "load record by ID" >> {
      transaction {
        val company = companies.lookup(td.c2.id)
        checkCompaniesEqual(company.get, td.c2)

        val employee = employees.lookup(td.e1.id)
        checkEmployeesEqual(employee.get, td.e1)
      }
    }

    "load record by string field value" >> {
      transaction {
        val company = from(companies)(c =>
          where(c.name === td.c1.name.is) select (c))
        checkCompaniesEqual(company.single, td.c1)
      }
    }

    "support order by" >> {
      transaction {
        val orderedCompanies = from(companies)(c =>
          select(c) orderBy (c.name))
        val ids = orderedCompanies.map(c => c.id)
        ids must containInOrder(
          td.allCompanies.sortBy(_.name.is).map(_.id))
      }
    }

    "support joins" >> {
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

    "support one to many relations" >> {
      transaction {
        val company = companies.lookup(td.c1.id)
        company must beSome[Company]
        val employees = company.get.employees
        employees must haveSize(1)
        checkEmployeesEqual(td.e1, employees.head)
      }
    }

    "support updates" >> {
      val id = td.c1.id

      transactionWithRollback {
        val company = companies.lookup(id).get
        company.name("New Name")
        company.postCode("11111")
        companies.update(company)

        val loaded = companies.lookup(id).get
        checkCompaniesEqual(company, loaded)

	update(companies)(c => where (c.id === id)
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

    "support delete" >> {
      transactionWithRollback {
	employees.delete(td.e2.id)
	employees.lookup(td.e2.id) must beNone
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
      f1.is must_== f2.is
      f1.name must_== f2.name
    }

    def checkXHtml() {
      r1.toXHtml must_== r2.toXHtml
    }
  }
}
