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

import _root_.scala.collection.immutable.Map
import _root_.dispatch.{Handler, Http}
import _root_.net.liftweb.common.{Box, Empty, Failure, Full}
import Box.{box2Iterable, option2Box}
import _root_.net.liftweb.json.JsonAST.{JField, JString, JValue}
import _root_.net.liftweb.record.field.OptionalStringField
import _root_.net.liftweb.util.ControlHelpers.tryo
import DocumentHelpers.jobjectToJObjectExtension

object CouchDB {
  /** Default database instance of the application */
  var defaultDatabase: Database = new Database("default")
}

/** Base trait of records that can be stored in CouchDB */
trait CouchRecord[MyType <: CouchRecord[MyType]] extends JSONRecord[MyType] {
  self: MyType =>

  /** Refine meta to require a CouchMetaRecord */
  def meta: CouchMetaRecord[MyType]

  /** The mandatory _id field */
  object id extends OptionalStringField(this, 100) with JSONField {
    override def jsonName = Full("_id")
  }

  /** The mandatory _rev field */
  object rev extends OptionalStringField(this, 100) with JSONField {
    override def jsonName = Full("_rev")
  }

  /** By default include a "type" field with value calculated by the typeName method */
  override def fixedAdditionalJFields: List[JField] = JField("type", JString(typeName))::Nil

  /**
   * The value of the "type" field for this class of record. Only used with default implementation of extraEncodedFields.
   * The default definition is the unqualified class name (e.g. the fully-qualified class name with the package name stripped)
   */
  def typeName: String = {
    val className = getClass.getName

    className.lastIndexOf('.') match {
      case -1 => className
      case n  => className.substring(n+1)
    }
  }

  /** Save the record instance and return it */
  def save: Box[MyType] = for (ok <- meta.save(this)) yield this

  /** Return whether this instance was saved into the backing store or not */
  def saved_? : Boolean = meta.saved_?(this)

  /** Delete the record instance from the backing store */
  def delete_! : Box[Unit] = canDelete_? match {
    case true => runSafe(meta.delete_!(this))
    case false => Failure("can't delete")
  }

  /** Can this record instance be deleted (e.g. is it in the backing store in the first place?) */
  def canDelete_? : Boolean = meta.saved_?(this)

  /** The database this record is associated with */
  private var _database: Box[Database] = Empty

  /** Return the database this record is associated with, determining it if necessary */
  def database: Database = _database match {
    case Full(database) => database
    case _ =>
      _database = Full(_calcDatabase)
      _database.open_!
  }

  /** Set the current database (if any) for this record */
  def database_= (database: Database): Unit = _database = Full(database)

  /** Override to calculate which database to use on a per-record basis */
  def calculateDatabase: PartialFunction[MyType, Database] = Map.empty

  /**
   * Calculate the database for this record, falling back to the CouchMetaRecord's default database if
   * calculateDatabase does not apply to this instance
   */
  def _calcDatabase: Database = if (calculateDatabase.isDefinedAt(this)) calculateDatabase(this) else meta.defaultDatabase
}

/** Base trait of meta records for records that can be stored in CouchDB */
trait CouchMetaRecord[BaseRecord <: CouchRecord[BaseRecord]] extends JSONMetaRecord[BaseRecord] {
  self: BaseRecord =>

  /** Get the default Database to use, if the record's calculateDatabase function does not provide one. Defaults to CouchDB.defaultDatabase */
  def defaultDatabase: Database = CouchDB.defaultDatabase

  /** Get an Http instance to use when accessing CouchDB */
  def http: Http = Http

  /** Save the record instance in the backing store */
  def save(inst: BaseRecord): Box[Unit] = {
    foreachCallback(inst, _.beforeSave)
    try {
      for (newDoc <- http(inst.database store inst.asJValue)) yield {
        inst.id.setBox(newDoc._id)
        inst.rev.setBox(newDoc._rev)
        ()
      }
    } finally {
      foreachCallback(inst, _.afterSave)
    }
  }

  /** Was the record instance saved in the backing store? */
  def saved_? (inst: BaseRecord): Boolean = inst.id.valueBox.isDefined && inst.rev.valueBox.isDefined

  /** Delete the instance from the backing store. Only works if the instance is the current revision in the database. */
  def delete_! (inst: BaseRecord): Box[Unit] = {
    foreachCallback(inst, _.beforeDelete)
    try {
      for {
        id <- inst.id.valueBox
        rev <- inst.rev.valueBox
        deletedOk <- tryo(http(inst.database(id) @@ rev delete))
      } yield {
        inst.id.setBox(Empty)
        inst.rev.setBox(Empty)
        ()
      }
    } finally {
      foreachCallback(inst, _.afterDelete)
    }
  }

  /** Query a single document by _id from the default database */
  def fetch(id: String): Box[BaseRecord] = fetchFrom(defaultDatabase, id)

  /** Query a single document by _id from the given database */
  def fetchFrom(database: Database, id: String): Box[BaseRecord] = tryo(http(database(id) fetch)).flatMap(fromJValue)

  /** Query a series of documents by _id from the default database */
  def fetchMany(ids: String*): Box[Seq[BaseRecord]] = fetchManyFrom(defaultDatabase, ids: _*)

  /** Query a series of documents by _id from the given database */
  def fetchManyFrom(database: Database, ids: String*): Box[Seq[BaseRecord]] =
    for (resultBox <- tryo(http(database(ids) query)); result <- resultBox)
      yield result.rows.flatMap(_.doc.flatMap(fromJValue))

  /**
   * Query records from the default database by document id. includeDocs is always on for this type of query.
   * Filter refines the query (e.g. by key), see Queryable.
   * Note that this is probably not very useful, as there is no way to constrain the documents retrieved by type
   */
  def all(filter: AllDocs => AllDocs): Box[Seq[BaseRecord]] = allIn(defaultDatabase, filter)

  /**
   * Query records from the given database by document id. includeDocs is always on for this type of query.
   * Filter refines the query (e.g. by key), see Queryable.
   * Note that this is probably not very useful, as there is no way to constrain the documents retrieved by type
   */
  def allIn(database: Database, filter: AllDocs => AllDocs): Box[Seq[BaseRecord]] =
    for (resultBox <- tryo(http(filter(database.all.includeDocs) query)); result <- resultBox)
      yield result.rows.flatMap { row => row.doc.flatMap(fromJValue) }

  /** Query using a view in the default database. */
  def queryView(design: String, view: String): Box[Seq[BaseRecord]] =
    queryViewFrom(defaultDatabase, design, view, identity)

  /** Query using a view in the default database. Filter refines the query (e.g. by key), see Queryable. */
  def queryView(design: String, view: String, filter: View => View): Box[Seq[BaseRecord]] =
    queryViewFrom(defaultDatabase, design, view, filter)

  /** Query using a view in the given database. Filter refines the query (e.g. by key), see Queryable. */
  def queryViewFrom(database: Database, design: String, view: String, filter: View => View): Box[Seq[BaseRecord]] =
    queryViewProjectionFrom(database, design, view, filter, _.value)

  /**
   * Query using a view in the default database, returning records created from the documents returned with the view.
   * If used against a reduce view, make sure to use dontReduce in the filter, otherwise CouchDB will signal an error.
   * includeDocs are always on for this type of query.
   */
  def queryViewDocs(design: String, view: String): Box[Seq[BaseRecord]] =
    queryViewDocsFrom(defaultDatabase, design, view, identity)

  /**
   * Query using a view in the default database, returning records created from the documents returned with the view.
   * If used against a reduce view, make sure to use dontReduce in the filter, otherwise CouchDB will signal an error.
   * Filter refines the query (e.g. by key), see Queryable.
   * includeDocs are always on for this type of query.
   */
  def queryViewDocs(design: String, view: String, filter: View => View): Box[Seq[BaseRecord]] =
    queryViewDocsFrom(defaultDatabase, design, view, filter)

  /**
   * Query using a view in the given database, returning records created from the documents returned with the view.
   * If used against a reduce view, make sure to use dontReduce in the filter, otherwise CouchDB will signal an error.
   * Filter refines the query (e.g. by key), see Queryable.
   * includeDocs are always on for this type of query.
   */
  def queryViewDocsFrom(database: Database, design: String, view: String, filter: View => View): Box[Seq[BaseRecord]] =
    queryViewProjectionFrom(database, design, view, v => filter(v.includeDocs), _.doc)

  /**
   * Query using a view in the default database, using some projection function that converts each QueryRow into a JSON document to read
   * as the record.
   */
  def queryViewProjection(design: String, view: String, project: QueryRow => Box[JValue]): Box[Seq[BaseRecord]] =
    queryViewProjectionFrom(defaultDatabase, design, view, identity, project)

  /**
   * Query using a view in the default database, using some projection function that converts each QueryRow into a JSON document to read
   * as the record. Filter refines the query (e.g. by key), see Queryable.
   */
  def queryViewProjection(design: String, view: String, filter: View => View, project: QueryRow => Box[JValue]): Box[Seq[BaseRecord]] =
    queryViewProjectionFrom(defaultDatabase, design, view, filter, project)

  /**
   * Query using a view in the given database, using some projection function that converts each QueryRow into a JSON document to read
   * as the record.
   */
  def queryViewProjectionFrom(database: Database, design: String, view: String, 
                              filter: View => View, project: QueryRow => Box[JValue]): Box[Seq[BaseRecord]] =
    for (resultBox <- tryo(http(filter(database.design(design).view(view)) query)); result <- resultBox)
      yield result.rows.flatMap(row => project(row).flatMap(fromJValue))

  /** Perform the given action with loose parsing turned on */
  def looseParsing[A](f: => A): A = 
    JSONMetaRecord.overrideIgnoreExtraJSONFields.doWith(true) {
      JSONMetaRecord.overrideNeedAllJSONFields.doWith(false) {
        f
      }
    }
}

}
}
