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
package mapper {
package view {

import scala.xml.{NodeSeq, Text, Elem}
import net.liftweb.common.Loggable
import net.liftweb.http.{S,DispatchSnippet,Paginator,PaginatorSnippet,
  SortedPaginator,SortedPaginatorSnippet}
import net.liftweb.http.S.?
import net.liftweb.util.Helpers._
import net.liftweb.mapper.{Mapper, MetaMapper, MappedField,
         QueryParam, OrderBy, StartAt, MaxRows, Ascending, Descending}
 
/**
 * Helper for when using paginators with a ModelSnippet.
 * Adds a dispatch that delegates the "paginate" snippet to the paginator member.
 * @author nafg and Timothy Perrett
 */ 
trait PaginatedModelSnippet[T <: Mapper[T]] extends ModelSnippet[T] {
  abstract override def dispatch: DispatchIt = super.dispatch orElse Map("paginate" -> paginator.paginate _ )
  /**
   * The paginator to delegate to
   */
  val paginator: PaginatorSnippet[T]
}

/** 
 * Paginate mapper instances by supplying the model you 
 * wish to paginate and Paginator will run your query for you etc.
 *
 * @param meta The singleton of the Mapper class you're paginating
 * @author nafg and Timothy Perrett
 */
class MapperPaginator[T <: Mapper[T]](val meta: MetaMapper[T]) extends Paginator[T] {
  /**
   * QueryParams to use always
   */
  var constantParams: Seq[QueryParam[T]] = Nil
  
  def count = meta.count(constantParams: _*)
  def page = meta.findAll(constantParams ++ Seq[QueryParam[T]](MaxRows(itemsPerPage), StartAt(first)): _*)
}

/**
 * Convenience class that combines MapperPaginator with PaginatorSnippet
 * @param meta The singleton of the Mapper class you're paginating
 */
class MapperPaginatorSnippet[T <: Mapper[T]](meta: MetaMapper[T])
  extends MapperPaginator[T](meta) with PaginatorSnippet[T]

/**
 * Implements MapperPaginator and SortedPaginator.
 * @param meta The singleton of the Mapper class you're paginating
 * @param initialSort The field to sort by initially
 * @param _headers Pairs of column labels and MappedFields.
 */
class SortedMapperPaginator[T <: Mapper[T]](meta: MetaMapper[T],
                                initialSort: net.liftweb.mapper.MappedField[_, T],
                                _headers: (String, MappedField[_, T])*)
    extends MapperPaginator[T](meta) with SortedPaginator[T, MappedField[_, T]] {
    
    val headers = _headers.toList
    sort = (headers.findIndexOf{case (_,`initialSort`)=>true; case _ => false}, true)
    
    override def page = meta.findAll(constantParams ++ Seq[QueryParam[T]](mapperSort, MaxRows(itemsPerPage), StartAt(first)): _*)
    private def mapperSort = sort match {
      case (fieldIndex, ascending) =>
        OrderBy(
          headers(fieldIndex) match {case (_,f)=>f},
          if(ascending) Ascending else Descending
        )
    }
}

/**
 * Convenience class that combines SortedMapperPaginator and SortedPaginatorSnippet.
 * @param meta The singleton of the Mapper class you're paginating
 * @param initialSort The field to sort by initially
 * @param headers Pairs of column labels and MappedFields.
 */
class SortedMapperPaginatorSnippet[T <: Mapper[T]](
  meta: MetaMapper[T],
  initialSort: net.liftweb.mapper.MappedField[_, T],
  headers: (String, MappedField[_, T])*
) extends SortedMapperPaginator[T](meta, initialSort, headers: _*)
  with SortedPaginatorSnippet[T, MappedField[_, T]]

}
}
}