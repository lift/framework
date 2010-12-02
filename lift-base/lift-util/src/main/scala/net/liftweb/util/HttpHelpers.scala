/*
 * Copyright 2006-2010 WorldWide Conferencing, LLC
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

import _root_.java.net.{URLDecoder, URLEncoder}
import _root_.scala.collection.mutable.ListBuffer
import _root_.scala.xml._
import _root_.scala.collection.{Map}
import _root_.scala.collection.mutable.HashMap
import _root_.java.util.concurrent.atomic.AtomicLong
import common._

object HttpHelpers extends ListHelpers with StringHelpers

trait HttpHelpers { self: ListHelpers with StringHelpers  =>

  /**
   * The list of known suffixes used to split the URI into path parts and suffixes.
   */
  val knownSuffixes: Set[String] = Set("json","rss","atom","do","3dm",
    "3dmf","a","aab","aam","aas","abc","acgi","afl","ai","aif","aifc","aiff",
    "aim","aip","ani","aos","aps","arc","arj","art","asf","asm","asp","asx","au","avi","avs",
    "bcpio","bin","bm","bmp","boo","book","boz","bsh","bz","bz2","c","c++","cat","cc","ccad",
    "cco","cdf","cer","cha","chat","class","com","conf","cpio","cpp","cpt","crl","crt","csh",
    "css","cxx","dcr","deepv","def","der","dif","dir","dl","doc","dot","dp","drw","dump","dv",
    "dvi","dwf","dwg","dxf","dxr","el","elc","env","eps","es","etx","evy","exe","f","f77",
    "f90","fdf","fif","fli","flo","flx","fmf","for","fpx","frl","funk","g","g3","gif","gl","gsd",
    "gsm","gsp","gss","gtar","gz","gzip","h","hdf","help","hgl","hh","hlb","hlp","hpg","hpgl",
    "hqx","hta","htc","htm","html","htmls","htt","htx","ice","ico","idc","ief","iefs","iges","igs",
    "ima","imap","inf","ins","ip","isu","it","iv","ivr","ivy","jam","jav","java","jcm","jfif",
    "jfif-tbnl","jpe","jpeg","jpg","jps","js","jut","kar","ksh","la","lam","latex","lha","lhx",
    "list","lma","log","lsp","lst","lsx","ltx","lzh","lzx","m","m1v","m2a","m2v","m3u","man","map",
    "mar","mbd","mc$","mcd","mcf","mcp","me","mht","mhtml","mid","midi","mif","mime","mjf","mjpg",
    "mm","mme","mod","moov","mov","movie","mp2","mp3","mpa","mpc","mpe","mpeg","mpg","mpga","mpp",
    "mpt","mpv","mpx","mrc","ms","mv","my","mzz","nap","naplps","nc","ncm","nif","niff","nix",
    "nsc","nvd","o","oda","omc","omcd","omcr","p","p10","p12","p7a","p7c","p7m","p7r","p7s","part",
    "pas","pbm","pcl","pct","pcx","pdb","pdf","pfunk","pgm","pic","pict","pkg","pko","pl","plx","pm",
    "pm4","pm5","png","pnm","pot","pov","ppa","ppm","pps","ppt","ppz","pre","prt","ps","psd",
    "pvu","pwz","py","pyc","qcp","qd3","qd3d","qif","qt","qtc","qti","qtif","ra","ram","ras",
    "rast","rexx","rf","rgb","rm","rmi","rmm","rmp","rng","rnx","roff","rp","rpm","rt","rtf","rtx",
    "rv","s","s3m","saveme","sbk","scm","sdml","sdp","sdr","sea","set","sgm","sgml","sh","shar",
    "shtml","sid","sit","skd","skm","skp","skt","sl","smi","smil","snd","sol","spc","spl","spr",
    "sprite","src","ssi","ssm","sst","step","stl","stp","sv4cpio","sv4crc","svf","svr","swf","t",
    "talk","tar","tbk","tcl","tcsh","tex","texi","texinfo","text","tgz","tif","tiff","tr","tsi",
    "tsp","tsv","turbot","txt","uil","uni","unis","unv","uri","uris","ustar","uu","uue","vcd","vcs",
    "vda","vdo","vew","viv","vivo","vmd","vmf","voc","vos","vox","vqe","vqf","vql","vrml","vrt",
    "vsd","vst","vsw","w60","w61","w6w","wav","wb1","wbmp","web","wiz","wk1","wmf","wml","wmlc",
    "wmls","wmlsc","word","wp","wp5","wp6","wpd","wq1","wri","wrl","wrz","wsc","wsrc","wtk","x-png",
    "xbm","xdr","xgz","xif","xl","xla","xlb","xlc","xld","xlk","xll","xlm","xls","xlt","xlv","xlw",
    "xm","xml","xmz","xpix","xpm","xsr","xwd","xyz","z","zip","zoo","zsh")

  /**
   * URL decode the string.
   *
   * This is a pass-through to Java's URL decode with UTF-8
   */
  def urlDecode(in : String) = URLDecoder.decode(in, "UTF-8")

  /**
   * URL encode the string.
   *
   * This is a pass-through to Java's URL encode with UTF-8
   */
  def urlEncode(in : String) = URLEncoder.encode(in, "UTF-8")

  /**
   * Take a list of name/value parse and turn them into a URL query string
   *
   * @param params the name/value pairs
   * @return a valid query string
   */
  def paramsToUrlParams(params: List[(String, String)]): String = params.map {
    case (n, v) => urlEncode(n) + "=" + urlEncode(v)
  }.mkString("&")


  /**
   * Append parameters to a URL
   *
   * @param url the url to append the params to
   * @param params the parameters (name/value) to append to the URL
   *
   * @return the url with the parameters appended
   */
  def appendParams(url: String, params: Seq[(String, String)]): String = params.toList match {
    case Nil => url
    case xs if !url.contains("?") => url + "?" + paramsToUrlParams(xs)
    case xs => url + "&" + paramsToUrlParams(xs)
  }


  /**
   * Given a map of HTTP properties, return true if the "Content-type"
   * value in the map is either "text/html" or "application/xhtml+xml"
   * @param in Map which may contain a key named Content-Type
   * @return true if there is a pair ("Content-Type", "text/html") or
   *                                 ("Content-Type", "application/xhtml+xml")
   */
  def couldBeHtml(in: Map[String, String]): Boolean =
  in match {
    case null => true
    case n => {
        n.get("Content-Type") match {
          case Some(s) => { (s.toLowerCase == "text/html") ||
                           (s.toLowerCase == "application/xhtml+xml") }
          case None => true
        }
      }
  }

  /**
   * Return true if the xml doesn't contain an &lt;html&gt; tag
   */
  def noHtmlTag(in: NodeSeq): Boolean = findElems(in)(_.label == "html").length != 1

  /**
   * Transform a general Map to a nutable HashMap
   */
  def toHashMap[A,B](in : Map[A,B]) : HashMap[A,B] = {
    val ret = new HashMap[A,B];
    in.keysIterator.foreach { k => ret += Pair(k, in(k)) }
    ret
  }

  /**
   * Ensure that all the appropriate fields are in the header.
   */
  def insureField(toInsure: List[(String, String)], headers: List[(String, String)]): List[(String, String)] = {
    def insureField_inner(toInsure : List[(String, String)], field : (String, String)): List[(String, String)] =
    toInsure.ciGet(field._1) match {
      case Full(_) => toInsure
      case _ => field :: toInsure
    }

    headers match {
      case Nil => toInsure
      case x :: xs => insureField(insureField_inner(toInsure, x), xs)
    }
  }

  /**
   * Transform a pair (name: String, value: Any) to an unprefixed XML attribute name="value"
   */
  implicit def pairToUnprefixed(in: (String, Any)): MetaData = {
    val value: Option[NodeSeq] = in._2 match {
      case null => None
      case js: ToJsCmd => Some(Text(js.toJsCmd))
      case n: Node => Some(n)
      case n: NodeSeq => Some(n)
      case None => None
      case Some(n: Node) => Some(n)
      case Some(n: NodeSeq) => Some(n)
      case Empty => None
      case Full(n: Node) => Some(n)
      case Full(n: NodeSeq) => Some(n)
      case s => Some(Text(s.toString))
    }

    value.map(v => new UnprefixedAttribute(in._1, v, Null)) getOrElse Null
  }


  /**
   * If the specified Elem has an attribute named 'id', return it, otherwise
   * construct a new Elem with a randomly generated id attribute and return the pair
   *
   * @param in the element to test &amp; add 'id' to
   * @return the new element and the id
   */
  def findOrAddId(in: Elem): (Elem, String) = (in \ "@id").toList match {
    case Nil => {
        val id = nextFuncName
        (in % ("id" -> id), id)
      }
    case x :: xs => (in, x.text)
  }

  /**
   * Within a NodeSeq, find the first elem and run it through
   * the function.  Return the resulting NodeSeq
   */
  def evalElemWithId(f: (String, Elem) => NodeSeq)(ns: NodeSeq): NodeSeq = {
    var found = false
    ns.flatMap {
      case e: Elem if !found => {
        found = true
        val (ne, id) = findOrAddId(e)
        f(id, ne)
      }
      case x => x
    }
  }
    


  private val serial = new AtomicLong(Math.abs(Helpers.randomLong(Helpers.millis)) + 1000000L)

  /**
   * Get a monotonically increasing number that's guaranteed to be unique for the
   * current session
   */
  def nextNum = serial.incrementAndGet

  /**
   * Find the elements of the specified NodeSeq that match
   * the specified predicate and concatenate them into
   * a resulting NodeSeq.
   *
   * @param nodes - the NodeSeq to search for elements matching the predicate
   * @param f - the predicate to match elements with
   * @return the NodeSeq resulting from concatenation of the matched elements.
   */
  def findElems(nodes: NodeSeq)(f: Elem => Boolean): NodeSeq = {
    val ret = new ListBuffer[Elem]
    def find(what: NodeSeq) {
      what.foreach {
        case Group(g) => find(g)
        case e: Elem =>
          if (f(e)) ret += e
          find(e.child)

        case n => find(n.child)
      }
    }
    find(nodes)

    ret.toList
  }

  /**
   * Map the specified function over the elements of the
   * specified NodeSeq and return the concatenated result.
   * This is essentially a container-type-transforming flatMap operation.
   */
  def findInElems[T](nodes: NodeSeq)(f: Elem => Iterable[T]): List[T] = {
    val ret = new ListBuffer[T]

    def find(what: NodeSeq) {
      what.foreach {
        case Group(g) => find(g)
        case e: Elem =>
          ret ++= f(e)
          find(e.child)

        case n => find(n.child)
      }
    }

    find(nodes)

    ret.toList
  }

  /**
   * Get a guaranteed unique field name
   * (16 or 17 letters and numbers, starting with a letter)
   */
  def nextFuncName: String = nextFuncName(0)

    /**
   * Get a guaranteed unique field name
   * (16 or 17 letters and numbers, starting with a letter)
   */
  def nextFuncName(seed: Long): String = {
    val sb = new StringBuilder(20)
    sb.append('F')
    sb.append(nextNum + seed)
    // sb.append('_')
    sb.append(randomString(3))
    sb.toString
  }

  def findKids(in: NodeSeq, prefix: String, label: String): NodeSeq =
  in.filter(n => n.label == label && n.prefix == prefix).flatMap(_.child)

  def deepFindKids(in: NodeSeq, prefix: String, label: String): NodeSeq = {
    val ret: ListBuffer[Node] = new ListBuffer

    def doIt(in: NodeSeq) {
      in.foreach {
        case e: Elem if e.prefix == prefix && e.label == label =>
          e.child.foreach(ret.+=)
        case g: Group => doIt(g.nodes)
        case n => doIt(n.child)
      }
    }

    doIt(in)
    ret.toList
  }
}

/**
 * TODO: Is this something that can be converted to a JavaScript Command
 */
trait ToJsCmd {
  def toJsCmd: String
}

object CheckNodeSeq {
  def unapply(in: Any): Option[NodeSeq] = in match {
    case Some(ns: NodeSeq) => Some(ns)
    case Full(ns: NodeSeq) => Some(ns)
    case Some(sq: Seq[_]) if sq.forall(_.isInstanceOf[Node])=> val ns: NodeSeq = sq.asInstanceOf[Seq[Node]]
      Some(ns)
    case Full(sq: Seq[_]) if sq.forall(_.isInstanceOf[Node])=> val ns: NodeSeq = sq.asInstanceOf[Seq[Node]]
      Some(ns)
    case ns: NodeSeq => Some(ns)
    case sq: Seq[_] if sq.forall(_.isInstanceOf[Node])=> val ns: NodeSeq = sq.asInstanceOf[Seq[Node]]
      Some(ns)
    case _ => None
  }
}

}
}
