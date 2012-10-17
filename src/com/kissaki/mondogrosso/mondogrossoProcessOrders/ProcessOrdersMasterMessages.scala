package com.kissaki.mondogrosso.mondogrossoProcessOrders

object ProcessOrdersMasterMessages extends Enumeration {
	
	type ContextMessages = Value
	
	//MESSAGES
	val MESSAGE_EMPTY,
	MESSAGE_START,
	MESSAGE_PROCEEDED,
	MESSAGE_ERROR,
	MESSAGE_TIMEOUTED,
	MESSAGE_DONE,
	MESSAGE_CONTEXT_OVER,
	
	MESSAGE_NOTFOUND
	= Value
	
	def get(in:String):Value = {
		values.find(in == _.toString) match {
			case None => MESSAGE_NOTFOUND
			case Some(v) => v
		}
	} 
}