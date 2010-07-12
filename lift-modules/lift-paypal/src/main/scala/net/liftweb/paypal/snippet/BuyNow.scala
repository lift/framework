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

package net.liftweb.paypal {
package snippet {
  
  import scala.xml.NodeSeq
  import net.liftweb.paypal.PaypalRules
  import net.liftweb.http.{DispatchSnippet}
  
  trait BuyNowSnippet extends DispatchSnippet {
    def dispatch = {
      case _ => buynow _
    }
    
    def amount: Double
    
    def values: Map[String,String] = Map()
    
    def buynow(xhtml: NodeSeq): NodeSeq = 
      <form name="_xclick" 
            action={PaypalRules.connection.vend().protocol+"://"+PaypalRules.mode.vend().domain+"/cgi-bin/webscr"} 
            method="post">
        <input type="hidden" name="cmd" value="_xclick" />
        <input type="hidden" name="amount" value={amount.toString} />
        <input type="hidden" name="currency_code" value={PaypalRules.currency.vend()} />
        { values.-("amount","currency_code","cmd","submit")
            .map(x => <input type="hidden" name={x._1} value={x._2} />) }
        <input type="image" src={PaypalRules.button.vend()} name="submit" alt="" />
      </form>
      
  }
  
}}