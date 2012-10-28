package com.kissaki.mondogrosso.mondogrossoProcessOrders

object WorkerMessages extends Enumeration {
	
	type WorkerMessages = Value
	
	//MESSAGES
	val MESSAGE_SETUP,
	MESSAGE_START,
	MESSAGE_SETUP_AND_START,
	
	MESSAGE_SYNCRONOUSLY_STARTED,
	MESSAGE_ASYNCRONOUSLY_STARTED,
	
	MESSAGE_REQUEST,
	MESSAGE_REQUEST_SPECIFY,
	
	MESSAGE_WAITING,
	MESSAGE_FINISHEDORDER_NOTIFY,
	MESSAGE_ERROR,
	
	
	MESSAGE_TIMEOUT,
	MESSAGE_DONE,
	
	MESSAGE_OVER,
	MESSAGE_OVERED,
	
	MESSAGE_FINALLY,
	
	MESSAGE_NOTFOUND
	= Value
	
	def get(in:String):Value = {
		values.find(in == _.toString) match {
			case None => MESSAGE_NOTFOUND
			case Some(v) => v
		}
	} 
}