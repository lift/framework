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

package net.liftweb {
package couchdb {

import _root_.scala.collection.{Map => MapTrait}
import _root_.scala.collection.immutable.Map
import _root_.scala.collection.mutable.ArrayBuffer
import _root_.scala.reflect.Manifest
import _root_.dispatch.{:/, Handler, Http, Request, StatusCode}
import _root_.net.liftweb.common.{Box, Empty, Failure, Full}
import _root_.net.liftweb.json.{DefaultFormats, Formats}
import _root_.net.liftweb.json.Extraction.{decompose, extract}
import _root_.net.liftweb.json.Implicits.string2jvalue
import _root_.net.liftweb.json.JsonAST.{JArray, JBool, JField, JInt, JObject, JString, JValue, render}
import _root_.net.liftweb.json.JsonDSL.pair2jvalue
import _root_.net.liftweb.json.Printer.compact
import _root_.net.liftweb.util.ControlHelpers.tryo
import DocumentHelpers.{jobjectToJObjectExtension, updateIdAndRev}
import DispatchJSON.requestToJSONRequest

/** Helper functions */
private[couchdb] object DatabaseHelpers {
  /** Handles the JSON result of an update action by parsing out id and rev, updating the given original object with the values and returning it */
  def handleUpdateResult(original: JObject)(json: JValue): Box[JObject] =
    for {
      obj <- Full(json).asA[JObject] ?~ ("update result is not a JObject: " + json)
      ok  <- Full(json \ "ok" ).asA[JField].map(_.value).asA[JBool].filter(_.value) ?~ ("ok not present in reply or not true: "+json)
      id  <- Full(json \ "id" ).asA[JField].map(_.value).asA[JString].map(_.s)    ?~ ("id not present or not a string: " + json)
      rev <- Full(json \ "rev").asA[JField].map(_.value).asA[JString].map(_.s)    ?~ ("rev not present or not a string: " + json)
    } yield updateIdAndRev(original, id, rev)
}

import DatabaseHelpers._

/** Single element Map implementation */
object SingleElementMap {
  /** Implicitly convert a pair to a single element map */
  implicit def pairToSingleElementMap[A, B](pair: (A, B)): MapTrait[A, B] = Map(pair)
}

import SingleElementMap.pairToSingleElementMap

/** Trait that adds a "fetch" method for getting a JObject from CouchDB */
trait FetchableAsJObject {
  self: Request =>

  /** Fetch the document as a JObject */
  def fetch: Handler[JObject] = this ># (_.asInstanceOf[JObject])
}  

/** Trait of requests that represent a document in a Couch database */
trait Document extends Request with FetchableAsJObject {
  /** Refine to a particular revision of the document. Only GET-style requests should be used with the resulting path */
  def at(rev: String): DocumentRevision = new Request(this <<? ("rev" -> rev)) with DocumentRevision { }

  /** Alias for at */
  def @@ (rev: String): DocumentRevision = at(rev)

  /** Refine to a particular revision of the document by getting _rev from a given JObject. */
  def at(doc: JObject): DocumentRevision = at(doc._rev.open_!)

  /** Alias for at */
  def @@ (doc: JObject): DocumentRevision = at(doc)

  /** Store a new version of the document, returning the document with _id and _rev updated */
  def put(doc: JObject): Handler[Box[JObject]] = JSONRequest(this) <<<# doc ># handleUpdateResult(doc) _
  
  /** Alias for put */
  def <<<# (doc: JObject): Handler[Box[JObject]] = put(doc)
}

/** Trait of requests that represent a particular document revision in a Couch database */
trait DocumentRevision extends Request with FetchableAsJObject {
  /** Destroy the document. The document's current revision must be the revision represented by this request */
  def delete: Handler[Unit] = DELETE >|
}

/** Trait of requests that represent a particular design document */
trait Design extends Document {
  /** Access a particular view by name that can be queried */
  def view(name: String): View = new Request(this / "_view" / name) with View { }
}

/** Trait of requests that represent a view that can be queried */
trait View extends Request with Queryable[View] {
  protected def newQueryable(req: Request): View = new Request(req) with View { }
}

/** Trait of requests representing all documents in a Couch database */
trait AllDocs extends Request with Queryable[AllDocs] {
  protected def newQueryable(req: Request): AllDocs = new Request(req) with AllDocs { }
}

/** Trait of requests that support CouchDB querying. That is, _all_docs and views */
trait Queryable[SelfType <: Queryable[SelfType]] {
  self: Request =>

  /** Create a new self-typed instance */
  protected def newQueryable(req: Request): SelfType

  /** Add parameters to the query */
  def withParams(params: MapTrait[String, Any]): SelfType = newQueryable(this <<? params)

  /** Fetch results of the query */
  def query: Handler[Box[QueryResults]] = this ># (QueryResult.read _)

  /** Query for the given key only */
  def key(keyValue: JValue): SelfType = withParams("key" -> compact(render(keyValue)))

  /** Query for the given set of keys */
  def keys(keyValues: JValue*): SelfType = newQueryable(this <<# ("keys" -> JArray(keyValues.toList)))

  /** Restrict the query to only keys greater than or equal to the given key */
  def from(lowValue: JValue): SelfType = withParams("startkey" -> compact(render(lowValue)))

  /** Restrict the query to only keys greater than or equal to the given key, not including any documents that are earlier in the view than the given docid */
  def from(lowValue: JValue, docid: String): SelfType = withParams(Map("startkey" -> compact(render(lowValue)), "startkey_docid" -> docid))

  /** Restrict the query to only keys less than or equal to the given key */
  def to(highValue: JValue): SelfType = withParams("endkey" -> compact(render(highValue)))

  /** Restrict the query to only keys less than or equal to the given key, not including any documents that are later in the view than the given docid */
  def to(highValue: JValue, docid: String): SelfType = withParams(Map("endkey" -> compact(render(highValue)), "endkey_docid" -> docid))

  /** Limit the query to the given number of results */
  def limit(i: Int): SelfType = withParams("limit" -> i)

  /** Specify that stale view data is okay. Used for optimization -- some other query must keep the view fresh. */
  def staleOk: SelfType = withParams("stale" -> "ok")

  /** Specify a descending sort. Note that the reversal is applied before key filtering, so you must reverse your from(...) and to(...) values. */
  def descending: SelfType = withParams("descending" -> "true")

  /** Group results (see http://wiki.apache.org/couchdb/Introduction_to_CouchDB_views) */
  def group: SelfType = withParams("group" -> "true")

  /** Group results at the given level (see http://wiki.apache.org/couchdb/Introduction_to_CouchDB_views) */
  def group(level: Int): SelfType = withParams(Map("group" -> "true") + ("group_level" -> level))

  /** Specify that reduction should not occur */
  def dontReduce: SelfType = withParams("reduce" -> "false")

  /** Include the associated document with each result */
  def includeDocs: SelfType = withParams("include_docs" -> "true")

  /**
   * Query a range matching the given key prefix. Equivalent to composing from(prefix) and to(prefix with {} appended),
   * e.g. from=["foobar"]&to=["foobar",{}]
   */
  def arrayRange(prefix: List[JValue]): SelfType = from(JArray(prefix)) to(JArray(prefix ::: (JObject(Nil)::Nil)))
}  

/** Specialization of dispatch's Request that provides Couch specific functionality */
class Database(couch: Request, database: String) extends Request(couch / database) {
  /** Construct a Database request using host and port */
  def this(hostname: String, port: Int, database: String) = this(:/(hostname, port), database)

  /** Construct a Database request to a default installation of CouchDB on localhost (port 5984) */
  def this(database: String) = this("127.0.0.1", 5984, database)

  /** Create the database iff it doesn't already exist */
  def createIfNotCreated(http: Http): Unit =
    try {
      http(info)
      ()
    } catch {
      case StatusCode(404, _) => http(create)
    }

  /** Attempt to create the database (PUT) */
  def create: Handler[Unit] = this <<< "" >|

  /** Retrieve information about the database (GET) */
  def info: Handler[DatabaseInfo] = {
    implicit val f: Formats = DefaultFormats
    this ># (extract[DatabaseInfo] _)
  }

  /** Destroy the database (DELETE) */
  def delete: Handler[Unit] = DELETE >|

  /** Access all documents in the database with a queryable interface */
  def all: AllDocs = new Request(this / "_all_docs") with AllDocs { }

  /** Access a particular document in the database by ID. */
  def apply(id: String): Document = new Request(this / id) with Document { }
  
  /** Access a particular document in the database with _id from a given JObject */
  def apply(doc: JObject): Document = this(doc._id.open_!.s)

  /** Access a series of documents by ID. */
  def apply(ids: Seq[String]): AllDocs = all.includeDocs.keys(ids.map(JString): _*)

  /** Access a particular design document in the database by name */
  def design(name: String): Design = new Request(this / "_design" / name) with Design { }

  /** Store a document in the database, generating a new unique ID for it and returning the document with _id and _rev updated */
  def post(doc: JObject): Handler[Box[JObject]] = JSONRequest(this) <<# doc ># handleUpdateResult(doc) _

  /** Alias for post */
  def <<# (doc: JObject): Handler[Box[JObject]] = post(doc)

  /** Inserts or updates a document in the database, using the standard _id field to do so. Returns the updated document. */
  def store(doc: JObject): Handler[Box[JObject]] =
    doc._id match {
      case Full(id) => this(id.s) <<<# doc
      case _    => this     <<#  doc
    }
}

/** Case class that holds information about a couch database, as retrieved using GET /database */
case class DatabaseInfo(db_name: String, doc_count: Int, doc_del_count: Int, update_seq: BigInt, compact_running: Boolean, disk_size: BigInt)

/** Result of a CouchDB query, possibly containing some summary information (if Couch provided it) such as total rows, and the results themselves */
case class QueryResults(totalRows: Box[BigInt], offset: Box[BigInt], rows: Seq[QueryRow])

/** Single result of a CouchDB query */
case class QueryRow(id: Box[String], key: JValue, value: Box[JValue], doc: Box[JObject], error: Box[JString])

object QueryResult {
  /** Read JSON into a QueryResults instance that holds the rows along with metadata about the query */
  def read(json: JValue): Box[QueryResults] =
    for {
      obj <- Full(json).asA[JObject] ?~ ("query JSON is not a JObject: " + json)
      jsonRows <- obj.get[JArray]("rows").map(_.arr) ?~ ("rows not found or wrong type in " + json)
      rows <- ((Full(new ArrayBuffer): Box[ArrayBuffer[QueryRow]]) /: jsonRows)((prev, cur) => {
        prev flatMap {
          buf => readRow(cur).flatMap { res => buf += res; prev }
        }
      })
    } yield {
      QueryResults(obj.get[JInt]("total_rows").map(_.num), obj.get[JInt]("offset").map(_.num), rows)
    }

  /** Read JSON into a QueryRow */
  private def readRow(json: JValue): Box[QueryRow] =
    for {
      obj <- Full(json).asA[JObject] ?~ ("row not a JObject: " + json)
      key <- obj.get[JValue]("key") ?~ ("key not found or wrong type in " + json)
    } yield QueryRow(obj.get[JString]("id").map(_.s), key, obj.get[JValue]("value"), obj.get[JObject]("doc"), obj.get[JString]("error"))
}

}
}
