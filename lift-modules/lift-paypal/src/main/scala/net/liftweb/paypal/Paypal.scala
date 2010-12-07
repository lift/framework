/*
 * Copyright 2007-2010 WorldWide Conferencing, LLC
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
package paypal {

import _root_.net.liftweb.util.Helpers
import Helpers._
import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
import _root_.net.liftweb.actor._
import _root_.net.liftweb.http._
import _root_.org.apache.commons.httpclient._
import _root_.org.apache.commons.httpclient.methods._
import _root_.java.io._
import _root_.scala.collection.mutable.ListBuffer

import _root_.scala.xml.{NodeSeq}

/**
 * sealed abstract type PaypalMode so we can cast to the super
 * class in our method declerations. Cannot be subclasses outside
 * of this source file.
 */
sealed trait PaypalMode {
  def domain: String
  override def toString = "PaypalMode: "+domain
}

object PaypalSandbox extends PaypalMode {
  def domain = "www.sandbox.paypal.com"
}

object PaypalLive extends PaypalMode {
  def domain = "www.paypal.com"
}

/**
 * Represents the type of connection that can be made
 * to paypal, irrespecitve of the mode of connection
 */
sealed trait PaypalConnection {
  def protocol: String
  def port: Int = 80

  override def toString = "PaypalConnection: "+protocol+":"+port
}

object PaypalHTTP extends PaypalConnection {
  def protocol = "http"
}

object PaypalSSL extends PaypalConnection {
  def protocol = "https"
  override def port: Int = 443
}

/**
 * Contatins all the papyal status abstractions as enumberable vals
 */
object PaypalTransactionStatus extends Enumeration {
  val CancelledReversalPayment = Value(1, "Cancelled_Reversal")
  val ClearedPayment = Value(2, "Cleared")
  val CompletedPayment = Value(3, "Completed")
  val DeniedPayment = Value(4, "Denied")
  val ExpiredPayment = Value(5, "Expired")
  val FailedPayment = Value(6, "Failed")
  val PendingPayment = Value(7, "Pending")
  val RefundedPayment = Value(8, "Refunded")
  val ReturnedPayment = Value(9, "Returned")
  val ReversedPayment = Value(10, "Reversed")
  val UnclaimedPayment = Value(11, "Unclaimed")
  val UnclearedPayment = Value(12, "Uncleared")
  val VoidedPayment = Value(13, "Voided")
  val InProgressPayment = Value(14, "In-Progress")
  val PartiallyRefundedPayment = Value(15, "Partially-Refunded")
  val ProcessedPayment = Value(16, "Processed")

  def find(name: String): Box[Value] = {
    val n = name.trim.toLowerCase
    this.iterator.filter(v => v.toString.toLowerCase == n).toList.firstOption
  }
}

/**
 * A paramater set that takes request paramaters (from Req) and assigns them
 * to properties of this class
 *
 * @param params The paramaters from the incomming request
 */
class PayPalInfo(params: HasParams) {
  private val r = params
  val itemName = r.param("item_name")
  val business = r.param("business")
  val itemNumber = r.param("item_number")
  val paymentStatus: Box[PaypalTransactionStatus.Value] = r.param("payment_status").flatMap(PaypalTransactionStatus.find)
  val mcGross = r.param("mc_gross")
  val paymentCurrency = r.param("mc_currency")
  val txnId = r.param("txn_id")
  val receiverEmail = r.param("receiver_email")
  val receiverId = r.param("receiver_id")
  val quantity = r.param("quantity")
  val numCartItems = r.param("num_cart_items")
  val paymentDate = r.param("payment_date")
  val firstName = r.param("first_name")
  val lastName = r.param("last_name")
  val paymentType = r.param("payment_type")
  val paymentGross = r.param("payment_gross")
  val paymentFee = r.param("payment_fee")
  val settleAmount = r.param("settle_amount")
  val memo = r.param("memo")
  val payerEmail = r.param("payer_email")
  val txnType = r.param("txn_type")
  val payerStatus = r.param("payer_status")
  val addressStreet = r.param("address_street")
  val addressCity = r.param("address_city")
  val addressState = r.param("address_state")
  val addressZip = r.param("address_zip")
  val addressCountry = r.param("address_country")
  val addressStatus = r.param("address_status")
  val tax = r.param("tax")
  val optionName1 = r.param("option_name1")
  val optionSelection1 = r.param("option_selection1")
  val optionName2 = r.param("option_name2")
  val optionSelection2 = r.param("option_selection2")
  val forAuction = r.param("for_auction")
  val invoice = r.param("invoice")
  val custom = r.param("custom")
  val notifyVersion = r.param("notify_version")
  val verifySign = r.param("verify_sign")
  val payerBusinessName = r.param("payer_business_name")
  val payerId =r.param("payer_id")
  val mcCurrency = r.param("mc_currency")
  val mcFee = r.param("mc_fee")
  val exchangeRate = r.param("exchange_rate")
  val settleCurrency  = r.param("settle_currency")
  val parentTxnId  = r.param("parent_txn_id")
  val pendingReason = r.param("pending_reason")
  val reasonCode = r.param("reason_code")
  val subscrId = r.param("subscr_id")
  val subscrDate = r.param("subscr_date")
  val subscrEffective  = r.param("subscr_effective")
  val period1 = r.param("period1")
  val period2 = r.param("period2")
  val period3 = r.param("period3")
  val amount = r.param("amt")
  val amount1 = r.param("amount1")
  val amount2 = r.param("amount2")
  val amount3 = r.param("amount3")
  val mcAmount1 = r.param("mc_amount1")
  val mcAmount2 = r.param("mc_amount2")
  val mcAmount3 = r.param("mcamount3")
  val recurring = r.param("recurring")
  val reattempt = r.param("reattempt")
  val retryAt = r.param("retry_at")
  val recurTimes = r.param("recur_times")
  val username = r.param("username")
  val password = r.param("password")

  val auctionClosingDate  = r.param("auction_closing_date")
  val auctionMultiItem  = r.param("auction_multi_item")
  val auctionBuyerId  = r.param("auction_buyer_id")
  override def toString: String = {
  val s1 = "itemName={"+ itemName +"}, business={"+ business +"}, itemNumber={"+ itemNumber +"}, paymentStatus={"+ paymentStatus +"}, mcGross={"+ mcGross +"}, paymentCurrency={"+ paymentCurrency +"}, txnId={"+ txnId +"}, receiverEmail={"+ receiverEmail
  val s2 = "}, receiverId={"+ receiverId +"}, quantity={"+ quantity +"}, numCartItems={"+ numCartItems +"}, paymentDate={"+ paymentDate +"}, firstName={"+ firstName +"}, lastName={"+ lastName +"}, paymentType={"+ paymentType +"}, paymentGross={"+ paymentGross +"}, paymentFee={"+ paymentFee +"}, settleAmount={"+ settleAmount +"}, memo={"+ memo +"}, payerEmail={"+ payerEmail 
  val s3 = "}, txnType={"+ txnType +"}, payerStatus={"+ payerStatus +"}, addressStreet={"+ addressStreet +"}, addressCity={"+ addressCity +"}, addressState={"+ addressState +"}, addressZip={"+ addressZip +"}, addressCountry={"+ addressCountry +"}, addressStatus={"+ addressStatus +"}, tax={"+ tax +"}, optionName1={"+ optionName1 +"}, optionSelection1={"+ optionSelection1 +"}, optionName2={"+ optionName2 +"}, optionSelection2={"+ optionSelection2 
  val s4 = "}, forAuction={"+ forAuction +"}, invoice={"+ invoice +"}, custom={"+ custom +"}, notifyVersion={"+ notifyVersion +"}, verifySign={"+ verifySign +"}, payerBusinessName={"+ payerBusinessName +"}, payerId={"+ payerId +"}, mcCurrency={"+ mcCurrency +"}, mcFee={"+ mcFee +"}, exchangeRate={"+ exchangeRate +"}, settleCurrency={"+ settleCurrency 
  val s5 = "}, parentTxnId={"+ parentTxnId +"}, pendingReason={"+ pendingReason +"}, reasonCode={"+ reasonCode +"}, subscrId={"+ subscrId +"}, subscrDate={"+ subscrDate +"}, subscrEffective={"+ subscrEffective +"}, period1={"+period1+"}, period2={"+period2+"}, period3={"+period3+"}, amount={"+ amount +"}, amount={"+amount1+"}, amount2={"+amount2+"}, amount3={"+amount3
  val s6 = "}, mcAmount1={"+mcAmount1+"}, mcAmount2={"+mcAmount2+"}, mcAmount3={"+mcAmount3+"},recurring={"+ recurring +"}, reattempt,retryAt={"+ retryAt +"}, recurTimes,username={"+ username +"},password={"+ password +"}, auctionClosingDate={"+ auctionClosingDate +"}, auctionMultiItem={"+ auctionMultiItem +"}, auctionBuyerId={"+auctionBuyerId+"}"
  s1 + s2 + s3 + s4 + s5 + s6
  }
}



/**
 * As the HTTP Commons HttpClient class is by definition very mutable, we
 * provide this factory method to produce an instance we can assign to a val
 *
 * @param url The domain url your sending to
 * @param port The TCP port the message will be sent over
 * @param connection The protocal to use: http, or https
 */
private object HttpClientFactory {
  def apply(url: String, port: Int, connection: String): HttpClient = {
    val c: HttpClient = new HttpClient()
    c.getParams().setParameter("http.protocol.content-charset", "UTF-8")
    c.getHostConfiguration().setHost(url, port, connection)
    c
  }
}

/**
 * Creates a new PostMethod and applys the passed paramaters
 *
 * @param url The string representation of the endpoing (e.g. www.paypal.com)
 * @paypal paramaters A Seq[(String,String)] of paramaters that will become the paypload of the request
 */
private object PostMethodFactory {
  def apply(url: String, paramaters: Seq[(String, String)]): PostMethod = {
    val p: PostMethod = new PostMethod(url)
    p.setRequestBody(paramaters)
    p
  }

  implicit def tonvp(in: Seq[(String, String)]): Array[NameValuePair] =
  in.map(p => new NameValuePair(p._1, p._2)).toArray
}

/**
 * Common functionality for paypal PDT and IPN
 */
trait PaypalBase {
  /**
   * Create a new HTTP client
   *
   * @param mode The PaypalMode type that your targeting. Options are PaypalLive or PaypalSandbox
   * @param connection The protocol the invocation is made over. Options are PaypalHTTP or PaypalSSL
   */
  protected def client(mode: PaypalMode, connection: PaypalConnection): HttpClient = HttpClientFactory(mode.domain, connection.port, connection.protocol)
}


/**
 * A simple abstraction for all HTTP operations. By definition they will return a HTTP error
 * code. We are invaribly only concerned with if it was a good one or not.
 */
trait PaypalUtilities {
  def wasSuccessful(code: Int): Boolean = code match {
    case 200 => true
    case _ => false
  }
}

/**
 * All HTTP requests to the paypal servers must subclass PaypalRequest.
 *
 * @param client Must be a HTTP client; the simplest way to create this is by using HttpClientFactory
 * @param post Specify the payload of the HTTP request. Must be an instance of PostMethod from HTTP commons
 */
private object PaypalRequest extends PaypalUtilities {
  def apply(client: HttpClient, post: PostMethod): List[String] = wasSuccessful(tryo(client.executeMethod(post)).openOr(500)) match {
    case true => StreamResponseProcessor(post)
    case _ => List("Failure")
  }
}

/**
 * As InputStream is a mutable I/O, we need to use a singleton to access
 * it / process it and return us a immutable result we can work with. If
 * we didnt do this then we get into a whole world of hurt and null pointers.
 */
private object StreamResponseProcessor {
  /**
   * @param p PostMethod Takes the raw HTTP commons PostMethod and processes its stream response
   */
  def apply(p: PostMethod): List[String] = {
    val stream: InputStream = p.getResponseBodyAsStream()
    val reader: BufferedReader = new BufferedReader(new InputStreamReader(stream))
    val ret: ListBuffer[String] = new ListBuffer

    try {
      def doRead {
        reader.readLine() match {
          case null => ()
          case line =>
            ret += line
            doRead
        }
      }

      doRead
      ret.toList
    } catch {
      case _ => Nil
    }
  }
}


/**
 * All paypal service classes need to subclass PaypalResponse explicitally.
 */

trait PaypalResponse extends PaypalUtilities with HasParams {
  def response: List[String]
  def isVerified: Boolean

  private lazy val info: Map[String, String] =
  Map((for (v <- response; s <- split(v)) yield s) :_*)

  def param(name: String): Box[String] = Box(info.get(name))

  lazy val paypalInfo: Box[PayPalInfo] =
  if (this.isVerified) Full(new PayPalInfo(this))
  else Empty

  def rawHead: Box[String] = Box(response.headOption)

  private def split(in: String): Box[(String, String)] = {
    val pos = in.indexOf("=")
    if (pos < 0) Empty
    else Full((urlDecode(in.substring(0, pos)),
               urlDecode(in.substring(pos + 1))))
  }
}


object PaypalDataTransfer extends PaypalBase {
  /**
   * payloadArray is the array of the post body we'll be sending.
   * As the payload body is different in PDT vs IPN
   *
   * @retrn List[(String,String)]
   */
  private def payloadArray(authToken: String, transactionToken: String) =
  List("cmd" -> "_notify-synch",
       "tx" -> transactionToken,
       "at" -> authToken)

  /**
   * Execute the PDT call
   *
   * @param authToken The token you obtain from the paypal merchant console
   * @param transactionToken The token that is passed back to your application as the "tx" part of the query string
   * @param mode The PaypalMode type that your targeting. Options are PaypalLive or PaypalSandbox
   * @param connection The protocol the invocation is made over. Options are PaypalHTTP or PaypalSSL
   * @return PaypalDataTransferResponse
   */
  def apply(authToken: String, transactionToken: String, mode: PaypalMode, connection: PaypalConnection): PaypalResponse =
  PaypalDataTransferResponse(
    PaypalRequest(client(mode, connection),
                  PostMethodFactory("/cgi-bin/webscr",payloadArray(authToken, transactionToken))))

}

/**
 * Wrapper instance for handling the response from a paypal data transfer.
 *
 * @param response The processed response List[String]. The response
 * input should be created with StreamResponseProcessor
 */
case class PaypalDataTransferResponse(response: List[String]) extends PaypalResponse {
  def isVerified = paymentSuccessful
  /**
   * Quick utility method for letting you know if the payment data is returning a sucsessfull message
   *
   * @return Boolean
   */
  def paymentSuccessful: Boolean = rawHead match {
    case Full("SUCCESS") => true
    case _ => false
  }
}

//
// PAYPAL IPN
//

/**
 * Users would generally invoke this case class in a DispatchPF call in
 * Boot.scala as it handles the incomming request and dispatches the IPN
 * callback, and handles the subsequent response.
 */
private object PaypalIPN extends PaypalUtilities {
  /**
   * @todo Really need to make sure that multiple custom paramaters can be mapped through.
   * The current solution is not good!
   */
  private def paramsAsPayloadList(request: Req): Seq[(String, String)] =
  (for(p <- request.params; mp <- p._2.map(v => (p._1, v))) yield (mp._1, mp._2)).toList

  def apply(request: Req, mode: PaypalMode, connection: PaypalConnection) = {
    //create request, get response and pass response object to the specified event handlers
    val ipnResponse: PaypalIPNPostbackReponse = PaypalIPNPostback(mode, connection, paramsAsPayloadList(request))
    ipnResponse
  }
}

/**
 * In response to the IPN postback from paypal, its nessicary to then call paypal and pass back
 * the exact set of paramaters that you were given by paypal - this stops spoofing. Use the
 * PaypalInstantPaymentTransferPostback exactly as you would PaypalDataTransferResponse.
 */
private[paypal] object PaypalIPNPostback extends PaypalBase {

  def payloadArray(paramaters: Seq[(String, String)]) = List("cmd" -> "_notify-validate") ++ paramaters

  /**
   * @return PaypalIPNPostbackReponse
   */
  def apply(mode: PaypalMode, connection: PaypalConnection, paramaters: Seq[(String, String)]): PaypalIPNPostbackReponse =
  new PaypalIPNPostbackReponse(
    PaypalRequest(client(mode, connection),PostMethodFactory("/cgi-bin/webscr",payloadArray(paramaters)))
  )
}

/**
 * An abstration for the response from Paypal during the to and frow of IPN validation
 *
 * @param response The processed List[String] from the paypal IPN request response cycle
 */
private[paypal] class PaypalIPNPostbackReponse(val response: List[String]) extends PaypalResponse {
  def isVerified: Boolean = rawHead match {
    case Full("VERIFIED") => true
    case _ => false
  }
}

object SimplePaypal extends PaypalIPN with PaypalPDT with Loggable {
  val paypalAuthToken = "123"
  def actions = {
    case (status, info, resp) =>
      logger.info("Got a verified PayPal IPN: "+status)
  }

  def pdtResponse = {
    case (info, resp) =>
      logger.info("Got a verified PayPal PDT: "+resp)
      DoRedirectResponse.apply("/")
  }
}

trait BasePaypalTrait extends LiftRules.DispatchPF {
  lazy val RootPath = rootPath

  def functionName = "Paypal"

  def rootPath = "paypal"

  def dispatch: List[LiftRules.DispatchPF] = Nil

  lazy val mode = PaypalRules.mode.vend()

  def connection = PaypalRules.connection.vend()

  def isDefinedAt(r: Req) = NamedPF.isDefinedAt(r, dispatch)

  def apply(r: Req) = NamedPF(r, dispatch)
}

trait PaypalPDT extends BasePaypalTrait {
  def paypalAuthToken: String

  lazy val PDTPath = pdtPath
  def pdtPath = "pdt"

  override def dispatch: List[LiftRules.DispatchPF] = {
    val nf: LiftRules.DispatchPF = NamedPF("Default PDT") {
      case r @ Req(RootPath :: PDTPath :: Nil, "", _) =>
      r.params // force the lazy value to be evaluated
      processPDT(r) _
    }

    super.dispatch ::: List(nf)
  }

  def pdtResponse:  PartialFunction[(PayPalInfo, Req), LiftResponse]

  def processPDT(r: Req)(): Box[LiftResponse] = {
    for (tx <- r.param("tx");
         val resp = PaypalDataTransfer(paypalAuthToken, tx, mode, connection);
         info <- resp.paypalInfo;
         redir <- tryo(pdtResponse(info, r))) yield {
      redir
    }
  }
}

/**
 * To handle IPN transactions you need to do the following:
 *
 * <code>
 *  // in Whatever.scala
 *  object MyPayPalHandler extends PayPal {
 *    import PaypalTransactionStatus._
 *    def actions = {
 *       case (ClearedPayment, info, _) => // write the payment to the database
 *       case (RefundedPayment, info, _) => // process refund
 *    }
 *  }
 *
 * // in Boot.scala
 *
 * LiftRules.statelessDispatchTable = MyPayPalHandler orElse
 *    LiftRules.statelessDispatchTable
 * </code>
 *
 * In this way you then get all the DispatchPF processing stuff for free.
 *
 */
trait PaypalIPN extends BasePaypalTrait {
  lazy val IPNPath = ipnPath
  def ipnPath = "ipn"

  def defaultResponse(): Box[LiftResponse] = Full(PlainTextResponse("ok"))

  override def dispatch: List[LiftRules.DispatchPF] = {
    val nf: LiftRules.DispatchPF = NamedPF("Default PaypalIPN") {
      case r @ Req(RootPath :: IPNPath :: Nil, "", PostRequest) =>
      r.params // force the lazy value to be evaluated
      requestQueue ! IPNRequest(r, 0, millis)
      defaultResponse _
    }

    super.dispatch ::: List(nf)
  }

  def actions: PartialFunction[(Box[PaypalTransactionStatus.Value], PayPalInfo, Req), Unit]

  protected case class IPNRequest(r: Req, cnt: Int, when: Long)
  protected case object PingMe


  protected def buildInfo(resp: PaypalResponse, req: Req): Box[PayPalInfo] = {
    if (resp.isVerified) Full(new PayPalInfo(req))
    else Empty
  }

  /**
   * How many times do we try to verify the request
   */
  val MaxRetry = 6

  protected object requestQueue extends LiftActor {
    protected def messageHandler =
      {
        case PingMe => ActorPing.schedule(this, PingMe, 10 seconds)

        case IPNRequest(r, cnt, _) if cnt > MaxRetry => // discard the transaction

          case IPNRequest(r, cnt, when) if when <= millis =>
            tryo {
              val resp = PaypalIPN(r, mode, connection)

              for (info <-  buildInfo(resp, r)) yield {
                actions((info.paymentStatus, info, r))
//              for (info <-  buildInfo(resp, r);
//                   stat <- info.paymentStatus) yield {
//                actions((stat, info, r))
                true
              }
            } match {
              case Full(Full(true)) => // it succeeded
		case _ => // retry
                  this ! IPNRequest(r, cnt + 1, millis + (1000 * 8 << (cnt + 2)))
            }
      }
  }
  requestQueue ! PingMe
}

}
}
