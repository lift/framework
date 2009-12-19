package net.liftweb.mapper.view

import net.liftweb.http.S
import S.?
import net.liftweb.util.Helpers._

import net.liftweb.mapper.{Mapper,
                           MetaMapper,
                           MappedField,
                           QueryParam,
                           OrderBy,
                           StartAt,
                           MaxRows,
                           Ascending,
                           Descending}

import scala.xml.{NodeSeq, Text}


/**
 * Use this trait instead of ModelSnippet if you want
 * the list view to be paginated.
 * You must instantiate paginator to a Paginator.
 * @author nafg
 */
trait PaginatedSnippet[T <: Mapper[T]] extends ModelSnippet[T] {
  override def dispatch = super.dispatch orElse Map("paginate" -> paginator.paginate _ )
  
  val paginator: Paginator[T]
}


/**
 * This class contains the logic for sortable pagination.
 * @param meta The singleton to query for items
 * @param snippet The ModelSnippet which this Paginator is for
 * @param headers Pairs (Tuple2) of column header names as used in the view and the fields that they sort by
 * @author nafg
 */
class Paginator[T <: Mapper[T]](val meta: MetaMapper[T], val snippet: ModelSnippet[T],
                                initialSort: MappedField[_, T],
                                val headers: (String, MappedField[_, T])*) {
  @deprecated def this(meta: MetaMapper[T],
		snippet: ModelSnippet[T],
		headers: (String,MappedField[_, T])) = {
    this(meta, snippet, null, headers)
  }
  /**
   * Override this to specify unchanging QueryParams to query the listing
   */
  var constantParams: Seq[QueryParam[T]] = Nil
  /**
   * Returns the total number of items to list
   */
  def count = meta.count(constantParams: _*)
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
  def page = meta.findAll(constantParams ++ Seq(sort, MaxRows(num), StartAt(first)): _*)
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

        val pages = List(curPage - 1020, curPage - 120, curPage - 20) ++
          (curPage-10 to curPage+10) ++
          List(curPage + 20, curPage + 120, curPage + 1020) filter { n=>
            n>=0 && n < numPages
          }
        pageLinks(pages, ns)
      },
      "next" -> linkIfOther(first+num min num*(numPages-1), nextXml),
      "last" -> linkIfOther(num*(numPages-1), Text(?(">>"))),
      "records" -> Text(
        "Displaying records "+(first+1)+"-"+(first+num min count)+" of "+count
      )
    )
  }
}
