/*
 * Copyright 2008-2010 WorldWide Conferencing, LLC
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
package util {

  trait Paginator[T] extends Loggable {

    def count: Long

    def itemsPerPage = 20

    def first = 0L

    def page: Seq[T]

    def numPages =
      (count/itemsPerPage).toInt +
        (if(count % itemsPerPage > 0) 1 else 0)

    def curPage = (first / itemsPerPage).toInt

    def zoomedPages = (
      List(curPage - 1020, curPage - 120, curPage - 20) ++
      (curPage-10 to curPage+10) ++
      List(curPage + 20, curPage + 120, curPage + 1020)
    ) filter { n=>
      n >= 0 && n < numPages
    }
  }

  /**
   * T: The type of the elements, accessed via def page within the listing snippet
   * F: The type of the fields, used to specify sorting
  */
  trait SortedPaginator[T, F] extends Paginator[T] {
    def headers: List[(String, F)]

    // current sort state: (field, ascending?)
    var _sort = (0, true)
    def sort: (Int, Boolean) = _sort
    def sort_=(s: (Int, Boolean)) = _sort = s

    def sortedBy(field: Int): (Int, Boolean) = sort match {
      case (`field`, true) =>  // descending is only on second click
        (field, false)
      case _ =>
        (field, true)
    }
  }
  
  trait PaginatorSnippet[T] extends Paginator[T] {
    def prevXml: NodeSeq = Text(?("<"))
    def nextXml: NodeSeq = Text(?(">"))
    def firstXml: NodeSeq = Text(?("<<"))
    def lastXml: NodeSeq = Text(?(">>"))
    def currentXml: NodeSeq = Text("Displaying records "+(first+1)+"-"+(first+itemsPerPage min count)+" of "+count)

    def navPrefix = "nav"
    def offsetParam = "offset"

    var _first = 0L
    override def first = S.param(offsetParam).map(toLong) openOr _first
    def first_=(f: Long) = _first = first

    def pageUrl(offset: Long): String = appendParams(S.uri, List(offsetParam -> offset.toString))

    def pageXml(newFirst: Long, ns: NodeSeq): NodeSeq =
      if(first==newFirst)
        ns
      else
        <a href={pageUrl(newFirst)}>{ns}</a>


    def pagesXml(pages: Seq[Int], sep: NodeSeq): NodeSeq =
      pages.toList map {n =>
        pageXml(n*itemsPerPage, Text(n+1 toString))
      } match {
        case one :: Nil => one
        case first :: rest => rest.foldLeft(first) {
          case (a,b) => a ++ sep ++ b
        }
        case Nil => Nil
      }



    // differences:
    //  - Instead of snippet.link, use registerSnippetFn(). For StatefulSnippets that would be registerThisSnippet.
    // For others, it could be an S.mapSnippet invocation etc.
    def paginate(xhtml: NodeSeq) = {
      bind(navPrefix, xhtml,
        "first" -> pageXml(0, firstXml),
        "prev" -> pageXml(first-itemsPerPage max 0, prevXml),
        "allpages" -> {(n:NodeSeq) => pagesXml(0 until numPages, n)},
        "zoomedpages" -> {(ns: NodeSeq) => pagesXml(zoomedPages, ns)},
        "next" -> pageXml(first+itemsPerPage min itemsPerPage*(numPages-1), nextXml),
        "last" -> pageXml(itemsPerPage*(numPages-1), lastXml),
        "records" -> currentXml
      )
    }
  }

  trait SortedPaginatorSnippet[T, F] extends SortedPaginator[T, F] with PaginatorSnippet[T] {
    def sortPrefix = "sort"
    def sortParam = "sort"
    def ascendingParam = "asc"

    def sortedPageUrl(offset: Long, sort: (Int, Boolean)) = sort match {
      case (field, ascending) =>
        appendParams(super.pageUrl(offset), List(sortParam->field.toString, ascendingParam->ascending.toString))
    }
    override def pageUrl(offset: Long) = sortedPageUrl(offset, sort)

    override def sort = super.sort match {
      case (field, ascending) => (
        S.param("sort").map(toInt) openOr field,
        S.param("asc").map(toBoolean) openOr ascending
      )
    }

    override def paginate(xhtml: NodeSeq): NodeSeq =
      bind(sortPrefix, super.paginate(xhtml),
        headers.zipWithIndex.map {
          case ((binding, _), fieldIndex) =>
            FuncBindParam(binding, (ns:NodeSeq) => <a href={sortedPageUrl(first, sortedBy(fieldIndex))}>{ns}</a> )
        }.toSeq : _*
      )
  }

  trait StatefulSortedPaginatorSnippet[T, F] extends SortedPaginatorSnippet[T, F] {
    def registerThisSnippet: Unit
    override def sortedPageUrl(offset: Long, sort: (Int, Boolean)) =
      S.fmapFunc(S.NFuncHolder(() => registerThisSnippet)){ name =>
        appendParams(super.sortedPageUrl(offset,sort), List(name -> "_"))
    }
  }
  
}
}