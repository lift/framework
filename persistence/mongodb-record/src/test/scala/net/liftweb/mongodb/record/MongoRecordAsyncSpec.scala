/*
 * Copyright 2017-2020 WorldWide Conferencing, LLC
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
package mongodb
package record

import org.specs2.mutable.Specification

import org.specs2.concurrent.ExecutionEnv

import scala.concurrent.duration._

/**
 * Tests the asynchronous side of MongoMetaRecord.
 *
 * Every assertion here awaits a MongoDB round trip. specs2's default await
 * budget is one second, and the async driver's first write after a cold start
 * can exceed that on a loaded machine: `replaceOne asynchronously` was measured
 * taking 1.251s in total while the same example takes well under a second once
 * the driver is warm. The awaits below therefore state an explicit budget of
 * five seconds rather than relying on the one-second default, so that the test
 * fails only when the operation genuinely does not complete. The operation
 * itself is not in question: with a larger budget it succeeds, and the writes
 * are verified afterwards by reading the records back.
 */
class MongoRecordAsyncSpec(implicit ee: ExecutionEnv) extends Specification with MongoAsyncTestKit {
  "MongoRecord Async Specification".title

  import fixtures.FieldTypeTestRecord

  "MongoRecord Async" should {

    "insert asynchronously" in {
      checkMongoIsRunning

      val obj = FieldTypeTestRecord.createRecord
        .mandatoryLongField(42L)
        .mandatoryIntField(27)

      FieldTypeTestRecord.insertAsync(obj) must beEqualTo[Boolean](true).awaitFor(5.seconds)

      val fetched = FieldTypeTestRecord.find(obj.id.get)

      fetched.isDefined must_== true

      fetched.foreach { o =>
        o.id.get must_== obj.id.get
        o.mandatoryLongField.get must_== 42L
        o.mandatoryIntField.get must_== 27
      }

      success
    }

    "replaceOne asynchronously" in {
      checkMongoIsRunning

      val obj = FieldTypeTestRecord.createRecord
        .mandatoryLongField(42L)
        .mandatoryIntField(27)

      FieldTypeTestRecord.replaceOneAsync(obj) must beEqualTo[FieldTypeTestRecord](obj).awaitFor(5.seconds)

      val fetched = FieldTypeTestRecord.find(obj.id.get)

      fetched.isDefined must_== true

      fetched.foreach { o =>
        o.id.get must_== obj.id.get
        o.mandatoryLongField.get must_== 42L
        o.mandatoryIntField.get must_== 27
      }

      obj
        .mandatoryLongField(44L)
        .mandatoryIntField(29)

      FieldTypeTestRecord.replaceOneAsync(obj) must beEqualTo[FieldTypeTestRecord](obj).awaitFor(5.seconds)

      val fetched2 = FieldTypeTestRecord.find(obj.id.get)

      fetched2.isDefined must_== true

      fetched2.foreach { o =>
        o.id.get must_== obj.id.get
        o.mandatoryLongField.get must_== 44L
        o.mandatoryIntField.get must_== 29
      }

      success
    }

    "replaceOne without upsert" in {
      checkMongoIsRunning

      val obj = FieldTypeTestRecord.createRecord

      FieldTypeTestRecord.replaceOneAsync(obj, false) must beEqualTo[FieldTypeTestRecord](obj).awaitFor(5.seconds)
      FieldTypeTestRecord.find(obj.id.get).isDefined must_== false
    }
  }
}
