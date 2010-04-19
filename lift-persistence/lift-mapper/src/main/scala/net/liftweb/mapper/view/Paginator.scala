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

  trait PaginatedSnippet extends DispatchSnippet {
    val paginator: Paginator
  }

  trait Paginator extends Loggable {

    var _first: Long = 0L
    def first(): Long = _first
    def first(x: Long): Long = {  
      _first = x 
      first() 
    }

    def num = 20
    protected def count: Long

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

    def calcActiveLink(start: Long, xml: NodeSeq): NodeSeq 
    def calcPassiveLink(start: Long, xml: NodeSeq): NodeSeq

    def curPage = (first() / num).toInt

    def nextOffset = first()+num min num*(numPages-1)
    def prevOffset = first()-num max 0
    def lastOffset = num*(numPages-1)
    def firstOffset = 0

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

  class MapperPaginator[T <: Mapper[T]](val meta: MetaMapper[T],
                           initialSort: MappedField[_, T],
                           val headers: (String, MappedField[_, T])*) extends Paginator {

    var sort: OrderBy[T, _] = OrderBy(initialSort, Ascending)
    var constantParams: Seq[QueryParam[T]] = Nil

    override def first(): Long = S.param("offset").map(_.toLong) openOr 0L

    override def calcActiveLink(start: Long, xml: NodeSeq): NodeSeq = {
      <a href={"?offset=" + start}>{xml}</a>
    }

    override def calcPassiveLink(start: Long, xml: NodeSeq): NodeSeq = 
      <span>{xml}</span>

    def count = meta.count(constantParams: _*)
    def page: List[T] = meta.findAll(constantParams ++ Seq(sort, MaxRows(num), StartAt(first())): _*)
    def sortBy(field: MappedField[_, T]) = sort = sort match {
      case OrderBy(f, Ascending) if f eq field =>
        OrderBy(field, Descending)
      case _ | null =>
        OrderBy(field, Ascending)
    }
  }

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
