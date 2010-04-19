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
  import net.liftweb.http.{S,DispatchSnippet}
  import net.liftweb.http.S.?
  import net.liftweb.util.Helpers._
  import net.liftweb.mapper.{Mapper, MetaMapper, MappedField,
           QueryParam, OrderBy, StartAt, MaxRows, Ascending, Descending}
  import net.liftweb.mapper.view.ModelSnippet
  
  /**
   * Snippet helper for people implementing 
   * pagination - this ensures they implement the paginator
   * 
   * @author Timothy Perrett and nafg
   */ 
  trait PaginatedSnippet extends DispatchSnippet {
    def dispatch = dispatch orElse Map("paginate" -> paginator.paginate _ )
    val paginator: Paginator
  }
  
  /**
   * Base trait for anything that should exhibit paginating abilities.
   * The key fields here are:
   * - "num" that holds the number of items per page
   * - "count" holds the overall item count
   * 
   * Then we have a bunch of methods you can override to change things:
   * - firstXml is the Elem for the first page
   * - lastXml is the Elem for the last page
   * - prevXml is the Elem for the previous page
   * - nextXml is the Elem for the next page
   * - recordsXml is the Elem a handy message notifiying the users which records are showing
   * - calcActiveLink is the Elem for active links
   * - calcPassiveLink is the Elem for unclickable (current) links
   * 
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
   *
   * @author Timothy Perrett and nafg
   */ 
  trait Paginator extends Loggable {
    
    /**
     * The query offset of the current page
     */
    var _first: Long = 0L
    def first(): Long = _first
    def first(x: Long): Long = {  
      _first = x 
      first() 
    }
    
    /**
     * The number of items on a page
     */
    def num = 20
    
    /**
     * Returns the total number of items to list
     */
    protected def count: Long
    
    /**
     * Configuration points for the XML components
     */ 
    def firstXml = Text(?("<<"))
    def lastXml = Text(?(">>"))
    def prevXml = Text(?("<"))
    def nextXml = Text(?(">"))
    def recordsXml = Text(
      "Displaying records "+(first()+1)+"-"+(first()+num min count)+" of "+count
    )

    def numPages = (count/num).toInt + (if(count % num > 0) 1 else 0)

    def pageLinks(pages: Seq[Int], sep: NodeSeq) = NodeSeq.fromSeq(
      pages.toList map {n =>
        linkIfOther(n*num, Text(n+1 toString))
      } match {
        case one :: Nil => one
        case start :: rest => rest.foldLeft(start) {
          case (a,b) => a ++ sep ++ b
        }
        case Nil => Nil
      }
    )

    def linkIfOther(start: Long, ns: NodeSeq): NodeSeq = 
      if(first==start) calcPassiveLink(start,ns)
      else {
        // first(start)
        calcActiveLink(start,ns)
      }
    
    
    /**
     * Link calculation methods - override this in a specilization
     */
    def calcActiveLink(start: Long, xml: NodeSeq): NodeSeq 
    def calcPassiveLink(start: Long, xml: NodeSeq): NodeSeq

    def curPage = (first() / num).toInt

    def nextOffset = first()+num min num*(numPages-1)
    def prevOffset = first()-num max 0
    def lastOffset = num*(numPages-1)
    def firstOffset = 0
    
    
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
    def paginate(xhtml: NodeSeq) = 
      bind("nav", xhtml,
        "first" -> linkIfOther(firstOffset, firstXml),
        "last" -> linkIfOther(lastOffset, lastXml),
        "prev" -> linkIfOther(prevOffset, prevXml),
        "next" -> linkIfOther(nextOffset, nextXml),
        "allpages" -> {(n:NodeSeq) => pageLinks(0 until numPages, n)},
        "zoomedpages" -> {(ns: NodeSeq) =>
          val pages = List(curPage - 1020, curPage - 120, curPage - 20) ++
            (curPage-10 to curPage+10) ++
            List(curPage + 20, curPage + 120, curPage + 1020) filter { n=>
              n>=0 && n < numPages
            }
          pageLinks(pages, ns)
        },
        "records" -> recordsXml
      )

  }
  
  /**
   * Specilized paginator that deals with Mapper instances 
   * and knows how to automaticlly find the count from a MetaMapper
   *
   * @param meta The singleton to query for items
   * @param initialSort Selection of MappedField's that will be use to sort the data set
   * @param headers Pairs (Tuple2) of column header names as used in the view and the fields that they sort by
   * @author Timothy Perrett and nafg
   */
  class MapperPaginator[T <: Mapper[T]](val meta: MetaMapper[T],
                           initialSort: MappedField[_, T],
                           val headers: (String, MappedField[_, T])*) extends Paginator {

    var sort: OrderBy[T, _] = OrderBy(initialSort, Ascending)
    
    /**
     * Override this to specify unchanging QueryParams to query the listing
     */
    def constantParams: Seq[QueryParam[T]] = Nil

    override def first(): Long = S.param("offset").map(_.toLong) openOr 0L

    override def calcActiveLink(start: Long, xml: NodeSeq): NodeSeq = 
      <a href={"?offset=" + start}>{xml}</a>
    override def calcPassiveLink(start: Long, xml: NodeSeq): NodeSeq = 
      <span>{xml}</span>
    
    /**
     * Returns the total number of items to list 
     * by calling the MetaMapper
     */
    def count = meta.count(constantParams: _*)
    
    /**
     * The current data set for this page
     */
    def page: List[T] = meta.findAll(constantParams ++ Seq(sort, MaxRows(num), StartAt(first())): _*)
    
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
  }
  
  /**
   * Further specilization for Mapper uses where if you 
   * are using ModelView stuff submitted by nafg then you can 
   * auto-paginate that calling into the StatefulSnippet 
   */
  class MapperViewPaginator[T <: Mapper[T]](meta: MetaMapper[T], 
          snippet: ModelSnippet[T], initialSort: MappedField[_, T], headers: (String, MappedField[_, T])*) 
        extends MapperPaginator[T](meta,initialSort,headers: _*){

    def calcLink(start: Long, xml: NodeSeq) = snippet.link(S.uri, ()=>first(start), xml)                     
    override def calcActiveLink(start: Long, xml: NodeSeq) = calcLink(start,xml)
    override def calcPassiveLink(start: Long, xml: NodeSeq) = calcLink(start,xml)
  }

}
}
}
