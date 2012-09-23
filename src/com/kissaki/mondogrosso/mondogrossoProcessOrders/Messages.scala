package com.kissaki.mondogrosso.mondogrossoProcessOrders

object Messages extends Enumeration {
	
	type Messages = Value
	
	//MESSAGES
	val MESSAGE_START,
	MESSAGE_STARTED,
	MESSAGE_PROCESSING,
	MESSAGE_ERROR,
	MESSAGE_DONE
	= Value
	
//	def MyValue(name: String): Value with Matching = 
//         new Val(nextId, name) with Matching
//
//   // enables matching against all Role.Values
//   def unapply(s: String): Option[Value] = 
//      values.find(s == _.toString)
//
//   trait Matching {
//      // enables matching against a particular Role.Value
//      def unapply(s: String): Boolean = 
//            (s == toString)
//   }
	
	def get(in:String):Value = {
		println("Value	is	"+Value)
		MESSAGE_START
	} 
}