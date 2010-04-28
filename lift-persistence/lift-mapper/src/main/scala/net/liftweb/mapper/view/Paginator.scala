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

/** FIXME 280 fix paginator code
import scala.xml.{NodeSeq, Text, Elem}
import net.liftweb.common.Loggable
import net.liftweb.http.{S,DispatchSnippet,Paginator,PaginatorSnippet,
  SortedPaginator,SortedPaginatorSnippet}
import net.liftweb.http.S.?
import net.liftweb.util.Helpers._
import net.liftweb.mapper.{Mapper, MetaMapper, MappedField,
         QueryParam, OrderBy, StartAt, MaxRows, Ascending, Descending}
 
/**
 * Snippet helper for people implementing 
 * pagination - this ensures they implement the paginator
 * 
 * @author nafg and Timothy Perrett
 */ 
trait PaginatedModelSnippet[T <: Mapper[T]] extends ModelSnippet[T] {
  abstract override def dispatch: DispatchIt = super.dispatch orElse Map("paginate" -> paginator.paginate _ )
  val paginator: PaginatorSnippet[T]
}

/** 
 * Paginate mapper instances by supplying the model you 
 * wish to paginate and Paginator will run your query for you etc.
 * 
 * @author nafg and Timothy Perrett
 */
class MapperPaginator[T <: Mapper[T]](val meta: MetaMapper[T]) extends Paginator[T] {
  var constantParams: Seq[QueryParam[T]] = Nil
  def count = meta.count(constantParams: _*)
<<<<<<< HEAD:framework/lift-persistence/lift-mapper/src/main/scala/net/liftweb/mapper/view/Paginator.scala
  /**
   * The number of items on a page
   */
  var num = 20
  /**
   * The query offset of the current page
   */
  var first = 0L
  /**
   * 
   */
  var sort: OrderBy[T, _] = OrderBy(initialSort, Ascending)
  
  /**
   * Returns the items on the current page. Use this in
   * the list snippet to get the items to display
   */
  def page = meta.findAll(constantParams ++ List(sort, MaxRows[T](num), StartAt[T](first)): _*)
  /**
   * Sets the sort-by field. If the specified field is already
   * the sort field, it is sorted descending; otherwise ascending.
   * Called when a column header is clicked.
   */
  def sortBy(field: MappedField[_, T]) = sort = sort match {
    case OrderBy(f, Ascending) if f eq field =>
      OrderBy(field, Descending)
    case _ | null =>
      OrderBy(field, Ascending)
  }
  def numPages =
    (count/num).toInt +
      (if(count % num > 0) 1 else 0)
  
  /**
   * This is the paginate snippet. It provides page
   * navigation and column sorting links.
   * View XHTML is as follows.
   * sort prefix
   *  - (a header name passed to the constructor) - a link that sorts by the field specified in the constructor
   * nav prefix
   *  - first - a link to the first page
   *  - prev - a link to the previous page
   *  - allpages - links to individual pages. The contents of this node are used to separate page links.
   *  - next - a link to the next page
   *  - last - a link to the last page
   *  - records - a description of which records are currently being displayed
   */
  def paginate(xhtml: NodeSeq) = {
    val prevXml = Text(?("<"))
    val nextXml = Text(?(">"))
    def linkIfOther(newFirst: Long, ns: NodeSeq) = {
      if(first==newFirst)
        ns
      else
        snippet.link(S.uri, ()=>first=newFirst, ns)
      }
    def pageLinks(pages: Seq[Int], sep: NodeSeq) = NodeSeq.fromSeq(
      pages.toList map {n =>
        linkIfOther(n*num, Text(n+1 toString))
      } match {
        case one :: Nil => one
        case first :: rest => rest.foldLeft(first) {
          case (a,b) => a ++ sep ++ b
        }
        case Nil => Nil
      }
    )
    bind("nav",
      bind("sort", xhtml,
           headers.map {
             case (binding, field) =>
               FuncBindParam(binding, (n:NodeSeq)=>snippet.link(S.uri, ()=>sortBy(field),n))
           }.toSeq : _*
      ),
      "first" -> linkIfOther(0, Text(?("<<"))),
      "prev" -> linkIfOther(first-num max 0, prevXml),
      "allpages" -> {(n:NodeSeq) => pageLinks(0 until numPages, n)},
      "zoomedpages" -> {(ns: NodeSeq) =>
        val curPage = (first / num).toInt
=======
  def page = meta.findAll(constantParams ++ Seq(MaxRows(itemsPerPage), StartAt(first)): _*)
}
>>>>>>> edcb456c75d27ac5748faebe0b9002042a4442d3:framework/lift-persistence/lift-mapper/src/main/scala/net/liftweb/mapper/view/Paginator.scala

class MapperPaginatorSnippet[T <: Mapper[T]](meta: MetaMapper[T])
  extends MapperPaginator[T](meta) with PaginatorSnippet[T]

class SortedMapperPaginator[T <: Mapper[T]](meta: MetaMapper[T],
                                initialSort: net.liftweb.mapper.MappedField[_, T],
                                _headers: (String, MappedField[_, T])*)
    extends MapperPaginator[T](meta) with SortedPaginator[T, MappedField[_, T]] {
    
    val headers = _headers.toList
    sort = (headers.findIndexOf{case (_,`initialSort`)=>true; case _ => false}, true)
    
    override def page = meta.findAll(constantParams ++ Seq(mapperSort, MaxRows(itemsPerPage), StartAt(first)): _*)
    private def mapperSort = sort match {
      case (fieldIndex, ascending) =>
        OrderBy(
          headers(fieldIndex) match {case (_,f)=>f},
          if(ascending) Ascending else Descending
        )
    }
}

class SortedMapperPaginatorSnippet[T <: Mapper[T]](
  meta: MetaMapper[T],
  initialSort: net.liftweb.mapper.MappedField[_, T],
  headers: (String, MappedField[_, T])*
) extends SortedMapperPaginator[T](meta, initialSort, headers: _*)
  with SortedPaginatorSnippet[T, MappedField[_, T]]

*/
}
}
}
