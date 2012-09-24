package com.kissaki.mondogrosso.mondogrossoProcessOrders

object Messages extends Enumeration {
	
	type Messages = Value
	
	//MESSAGES
	val MESSAGE_START,
	MESSAGE_STARTED,
	MESSAGE_PROCESSING,
	MESSAGE_ERROR,
	MESSAGE_DONE,
	MESSAGE_NOTFOUND
	= Value
	
	def get(in:String):Value = {
		values.find(in == _.toString) match {
			case None => {
				MESSAGE_NOTFOUND
			}
			case Some(v) => {
				v
			}
		}
	} 
}