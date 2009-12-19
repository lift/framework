package net.liftweb.paypal

// import org.specs.Specification
// import org.specs.mock._
// import org.specs.mock.JMocker._
// import org.specs.runner.JUnit4

// class PaypalIPNSpecTest extends JUnit4(PaypalIPNSpec)
// object ApplicationServicesSpec 
//   extends Specification("Paypal IPN") 
//   with JMocker 
//   with ClassMocker {
//   
//   "IPN responses" should {
//     "have a boxed transaction status" in {
//       
//     }
//   }
//   
// }
// 
// object SimplePaypal extends PaypalIPN {
//   def actions = {
//     case (status, info, resp) =>
//       Log.info("Got a verified PayPal IPN: "+status)
//   }
// }