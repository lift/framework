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
  
  import net.liftweb.util.Props
  import net.liftweb.http.{Factory,ResourceServer}
  import java.util.{Currency,Locale}
  
  object PaypalRules extends Factory {
    
    def init {
      ResourceServer.allow {
        case "paypal" :: _ :: Nil => true
      }
    }
    
    val mode = new FactoryMaker[() => PaypalMode](() => Props.mode match {
      case Props.RunModes.Production => PaypalLive
      case _ => PaypalSandbox
    }){}
    
    val connection = new FactoryMaker[() => PaypalConnection](() => PaypalSSL){}
    
    val currency = new FactoryMaker[() => String](() => Currency.getInstance(Locale.getDefault).getCurrencyCode){}
    
    val button = new FactoryMaker[() => String](() => "/classpath/paypal/en_buynow_68x23.gif"){}
    
  }
  
}}