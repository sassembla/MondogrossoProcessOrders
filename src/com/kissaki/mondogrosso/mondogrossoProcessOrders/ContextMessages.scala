package com.kissaki.mondogrosso.mondogrossoProcessOrders

object ContextMessages extends Enumeration {
	
	type ContextMessages = Value
	
	//MESSAGES
	val MESSAGE_READY,
	MESSAGE_START,
	MESSAGE_PROCEEDED,
	MESSAGE_ERROR,
	MESSAGE_TIMEOUT,
	MESSAGE_DONE,
	
	MESSAGE_NOTFOUND
	= Value
	
	def get(in:String):Value = {
		values.find(in == _.toString) match {
			case None => MESSAGE_NOTFOUND
			case Some(v) => v
		}
	} 
}