/*
 * Copyright 2009-2011 WorldWide Conferencing, LLC
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

import org.specs2.mutable.Specification

/**
 * Systems under specification for ManyToMany.
 */
object ManyToManySpec extends Specification  {
  "ManyToMany Specification".title
  sequential

  val provider = DbProviders.H2MemoryProvider

  private def ignoreLogger(f: => AnyRef): Unit = ()
  def setupDB {
    MapperRules.createForeignKeys_? = c => false
    provider.setupDB
    Schemifier.destroyTables_!!(ignoreLogger _,  PersonCompany, Company, Person)
    Schemifier.schemify(true, ignoreLogger _, Person, Company, PersonCompany)
  }
  def createPerson = {
    val person = new Person
    person.save
    val companies = (1 to 10).toList map { i =>
      val c = new Company
      c.name ()= i.toString
      c.save
      c
    }
    person.companies ++= companies
    person.save
    // Break some joins
    companies(3).delete_! // delete "4"
    companies(6).delete_! // delete "7"
    person.companies.refresh  // reload joins so joinEntity.company.obj isn't cached
    person
  }

  "ManyToMany" should {
    "skip broken joins in children" in {
      setupDB
      val person = createPerson
      person.companies.joins.length must_== 10
      person.companies.all.length must_== 8
    }
    "handle missing joins in insertAll" in {
      setupDB
      val person = createPerson
      val c = new Company
      c.name ()= "new"
      c.save
      person.companies.insertAll(7, Seq(c))
      person.companies(7).name.get must_== "new"
    }


// from Florian

    "count unsaved children" in {
      setupDB
      val person = new Person
      val company = new Company
      person.companies += company
      person.companies.length must_== 1
    }
    "count saved children" in {
      setupDB
      val person = new Person
      val company = new Company
      company.save
      person.companies += company
      person.companies.length must_== 1
    }
//     "count unsaved children of a saved entity" in {
//       setupDB
//       val person = new Person
//       val company = new Company
//       person.companies += company
//       person.save
//       person.companies.length must_== 1
//     }
    "count saved children of a saved entity" in {
      setupDB
      val person = new Person
      val company = new Company
      company.save
      person.companies += company
      person.save
      person.companies.length must_== 1
    }
    "count saved children of a saved entity after refresh" in {
      setupDB
      val person = new Person
      person.save
      val company = new Company
      company.save
      person.companies += company
      person.save
      person.companies.refresh
      person.companies.length must_== 1
    }
  }

}



class Person extends LongKeyedMapper[Person] with IdPK with ManyToMany {
  def getSingleton = Person
  object companies extends MappedManyToMany(PersonCompany, PersonCompany.person, PersonCompany.company, Company)
}
object Person extends Person with LongKeyedMetaMapper[Person]

class Company extends LongKeyedMapper[Company] with IdPK {
  def getSingleton = Company
  object name extends MappedString(this, 10)
}
object Company extends Company with LongKeyedMetaMapper[Company]

class PersonCompany extends Mapper[PersonCompany] {
  def getSingleton = PersonCompany
  object person extends MappedLongForeignKey(this, Person)
  object company extends MappedLongForeignKey(this, Company)

  override def toString = "PersonCompany(person.is=%s, person.obj=%s, company.is=%s, company.obj=%s)".format(person.get,person.obj,company.get,company.obj)
}
object PersonCompany extends PersonCompany with MetaMapper[PersonCompany]

