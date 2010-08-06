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
package http {
  
  import scala.xml.{NodeSeq,Text,Elem}
  import net.liftweb.common.Loggable
  import net.liftweb.util.Helpers._
  import S.?
  
  
  /**
   * Base class for things that require pagination. Implements a contract 
   * for supplying the correct number of browsable pages etc
   *
   * @tparam T the type of item being paginated
   * @author nafg and Timothy Perrett
   */
  trait Paginator[T] extends Loggable {
    /**
     * The total number of items
     */
    def count: Long
    /**
     * How many items to put on each page
     */
    def itemsPerPage = 20
    /**
     * The record number this page starts at. Zero-based.
     */
    def first = 0L
    /**
     * The items displayed on the current page
     */
    def page: Seq[T]
    /**
      * Calculates the number of pages the items will be spread across
      */
    def numPages =
      (count/itemsPerPage).toInt +
        (if(count % itemsPerPage > 0) 1 else 0)
    /**
      * Calculates the current page number, based on the value of 'first.'
      */
    def curPage = (first / itemsPerPage).toInt
    /**
     * Returns a list of page numbers to be displayed in 'zoomed' mode, i.e.,
     * as the page numbers get further from the current page, they are more sparse.
     */
    def zoomedPages = (
      List(curPage - 1020, curPage - 120, curPage - 20) ++
      (curPage-10 to curPage+10) ++
      List(curPage + 20, curPage + 120, curPage + 1020)
    ) filter { n=>
      n >= 0 && n < numPages
    }
  }

  /**
   * In many situations you'll want to sort things in your paginated view. 
   * <code>SortedPaginator</code> is a specialized paginator for doing such tasks.
   * 
   * T: The type of the elements, accessed via def page within the listing snippet
   * C: The type of the columns, used to specify sorting
   *
   * @author nafg and Timothy Perrett
   */
  trait SortedPaginator[T, C] extends Paginator[T] {
    /**
     * Pair of (column index, ascending)
     */
    type SortState = (Int, Boolean)
    /**
     * The sort headers: pairs of column labels, and column identifier objects of type C.
     */
    def headers: List[(String, C)]

    protected var _sort = (0, true)
    /**
     * Get the current sort state: Pair of (column index, ascending?)
     */
    def sort: SortState = _sort
    /**
     * Set the current sort state: Pair of (column index, ascending?)
     */
    def sort_=(s: SortState) = _sort = s

    /**
     * Returns a new SortState based on a column index.
     * If the paginator is already sorted by that column, it
     * toggles the direction; otherwise the direction is ascending.
     * Note that this method does not alter the sort state in the
     * paginator; it only calculates the direction toggle.
     * Example usage:
     * sortedPaginator.sort = sortedPaginator.sortedBy(columns.indexOf(clickedColumn))
     */
    def sortedBy(column: Int): SortState = sort match {
      case (`column`, true) =>  // descending is only if it was already sorted ascending
        (column, false)
      case _ =>
        (column, true)
    }
  }
  
  /**
   * This is the paginator snippet. It provides page
   * navigation and column sorting links.
   * View XHTML is as follows: 
   * nav prefix (prefix is configurable by overriding def navPrefix)
   *  - &lt;nav:first/&gt; - a link to the first page
   *  - &lt;nav:prev/&gt; - a link to the previous page
   *  - &lt;nav:allpages/&gt; - individual links to all pages. The contents of this node are used to separate page links.
   *  - &lt;nav:next/&gt; - a link to the next page
   *  - &lt;nav:last/&gt; - a link to the last page
   *  - &lt;nav:records/&gt; - a description of which records are currently being displayed
   *  - &lt;nav:recordsFrom/&gt; - the first record number being displayed
   *  - &lt;nav:recordsTo/&gt; - the last record number being displayed
   *  - &lt;nav:recordsCount/&gt; - the total number of records on all pages
   *
   * @author nafg and Timothy Perrett
   */
  trait PaginatorSnippet[T] extends Paginator[T] {
    /**
     * The "previous page" link text
     */
    def prevXml: NodeSeq = Text(?("<"))
    /**
     * The "next page" link text
     */
    def nextXml: NodeSeq = Text(?(">"))
    /**
     * The "first page" link text
     */
    def firstXml: NodeSeq = Text(?("<<"))
    /**
     * The "last page" link text
     */
    def lastXml: NodeSeq = Text(?(">>"))
		
    /**
     * How to display the page's starting record
     */
    def recordsFrom: String = (first+1 min count) toString
    /**
     * How to display the page's ending record
     */
    def recordsTo: String = ((first+itemsPerPage) min count) toString
    /**
     * The status displayed when using &lt;nav:records/&gt; in the template.
     */
    def currentXml: NodeSeq = if(count==0)
			Text("There are no records to display")
		else
			Text("Displaying records "+recordsFrom+"-"+recordsTo+" of "+count)
    /**
     * The template prefix for general navigation components
     */
    def navPrefix = "nav"
    /**
     * The URL query parameter to propagate the record the page should start at
     */
    def offsetParam = "offset"

    protected var _first = 0L
    /**
     * Overrides the super's implementation so the first record can be overridden by a URL query parameter.
     */
    override def first = S.param(offsetParam).map(toLong) openOr _first max 0
    /**
     * Sets the default starting record of the page (URL query parameters take precedence over this)
     */
    def first_=(f: Long) = _first = f max 0 min (count-1)
    /**
     * Returns a URL used to link to a page starting at the given record offset.
     */
    def pageUrl(offset: Long): String = appendParams(S.uri, List(offsetParam -> offset.toString))
    /**
     * Returns XML that links to a page starting at the given record offset, if the offset is valid and not the current one.
     * @param ns The link text, if the offset is valid and not the current offset; or, if that is not the case, the static unlinked text to display
     */
    def pageXml(newFirst: Long, ns: NodeSeq): NodeSeq =
      if(first==newFirst || newFirst < 0 || newFirst >= count)
        ns
      else
        <a href={pageUrl(newFirst)}>{ns}</a>

    /**
     * Generates links to multiple pages with arbitrary XML delimiting them.
     */
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
    
    /**
     * This is the method that binds template XML according according to the specified configuration.
     * You can reference this as a snippet method directly in your template; or you can call it directly
     * as part of your binding code.
     */
    def paginate(xhtml: NodeSeq) = {
      bind(navPrefix, xhtml,
        "first" -> pageXml(0, firstXml),
        "prev" -> pageXml(first-itemsPerPage max 0, prevXml),
        "allpages" -> {(n:NodeSeq) => pagesXml(0 until numPages, n)},
        "zoomedpages" -> {(ns: NodeSeq) => pagesXml(zoomedPages, ns)},
        "next" -> pageXml(first+itemsPerPage min itemsPerPage*(numPages-1) max 0, nextXml),
        "last" -> pageXml(itemsPerPage*(numPages-1), lastXml),
        "records" -> currentXml,
        "recordsFrom" -> Text(recordsFrom),
        "recordsTo" -> Text(recordsTo),
        "recordsCount" -> Text(count.toString)
      )
    }
  }

  /**
   * This trait adds snippet functionality for sorted paginators.
   * You can place bind points in your template for column headers, and it turns them into links
   * That you can click to sort by that column. Simply write, e.g.,
	 * &lt;th&gt;&lt;sort:name/&gt;&lt;/th&gt;&lt;th&gt;&lt;sort:email/&gt;&lt;/th&gt; etc.
   */
  trait SortedPaginatorSnippet[T, C] extends SortedPaginator[T, C] with PaginatorSnippet[T] {
    /**
     * The prefix to bind the sorting column headers
     */
    def sortPrefix = "sort"
    /**
     * The URL query parameter to specify the sort column
     */
    def sortParam = "sort"
    /**
     * The URL query parameter to specify the sort direction
     */
    def ascendingParam = "asc"
    /**
     * Calculates the page url taking sorting into account.
     */
    def sortedPageUrl(offset: Long, sort: (Int, Boolean)) = sort match {
      case (col, ascending) =>
        appendParams(super.pageUrl(offset), List(sortParam->col.toString, ascendingParam->ascending.toString))
    }
    /**
     * Overrides pageUrl and delegates to sortedPageUrl using the current sort
     */
    override def pageUrl(offset: Long) = sortedPageUrl(offset, sort)
    /**
     * Overrides sort, giving the URL query parameters precedence
     */
    override def sort = super.sort match {
      case (col, ascending) => (
        S.param("sort").map(toInt) openOr col,
        S.param("asc").map(toBoolean) openOr ascending
      )
    }
    /**
     * This is the snippet method, which you can reference in your template or call directly.
     */
    override def paginate(xhtml: NodeSeq): NodeSeq =
      bind(sortPrefix, super.paginate(xhtml),
        headers.zipWithIndex.map {
          case ((binding, _), colIndex) =>
            FuncBindParam(binding, (ns:NodeSeq) => <a href={sortedPageUrl(first, sortedBy(colIndex))}>{ns}</a> )
        }.toSeq : _*
      )
  }
  
  /**
   * Sort your paginated views by using lifts functions mapping. 
   * The only down side with this style is that your links are session 
   * specific and non-bookmarkable.
   * If you mix this trait in to a StatefulSnippet, it should work out the box.
   * Otherwise, implement 'registerThisSnippet.'
   * @author nafg and Timothy Perrett
   */
  trait StatefulSortedPaginatorSnippet[T, C] extends SortedPaginatorSnippet[T, C] {
    /**
     * This method is called before the new page is served, to set up the state in advance.
     * It is implemented by StatefulSnippet so you can just mix in StatefulSortedPaginatorSnippet to one;
     * or you can implement it yourself, using things like S.mapSnippet.
     */
    def registerThisSnippet: Unit
    /**
     * Overrides to use Lift state rather than URL query parameters.
     */
    override def sortedPageUrl(offset: Long, sort: (Int, Boolean)) =
      S.fmapFunc(S.NFuncHolder(() => registerThisSnippet)){ name =>
        appendParams(super.sortedPageUrl(offset,sort), List(name -> "_"))
    }
  }
  
}
}