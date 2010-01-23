/*
 * Copyright 2009-2010 WorldWide Conferencing, LLC
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

package net.liftweb {
package transaction {

//import com.jteigen.scalatest.JUnit4Runner

//import org.junit.runner.RunWith
//import org.scalatest._
//import org.scalatest.matchers._                                                                                      

/**
 * @author <a href="http://jonasboner.com">Jonas Bon&#233;r</a>
 *
@RunWith(classOf[JUnit4Runner])
class TransactionMonadSpec extends Spec with ShouldMatchers {
  describe("A TransactionMonad") {
    it("should support foreach") {
      for (ctx <- TransactionContext.Required) {
        println("Context: " + ctx)
      }
    }

    it("should support map") {
      for (ctx <- TransactionContext.Required) {
        println("Context: " + ctx)
        ctx
      }
    }

    it("should support flatMap") {
      val userNames = "Bill" :: "Bob" :: "Alice" :: Nil
      val users = for {
        ctx <- TransactionContext.Required
        name <- userNames
      } yield {
        val query = ctx.getEntityManager.createNamedQuery("findUserByName")
        query.setParameter("userName", name)
        query.getSingleResult
      }
      println("Users: " + users)
    }
  }
}
*/
}
}
