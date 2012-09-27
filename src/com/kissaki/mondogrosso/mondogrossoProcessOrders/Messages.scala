package com.kissaki.mondogrosso.mondogrossoProcessOrders

object Messages extends Enumeration {
	
	type Messages = Value
	
	//MESSAGES
	val MESSAGE_START,
	MESSAGE_SYNCRONOUSLY_STARTED,
	MESSAGE_ASYNCRONOUSLY_STARTED,
	MESSAGE_REQUEST,
	
	MESSAGE_WAITING,
	MESSAGE_FINISHEDORDER_NOTIFY,
	MESSAGE_ERROR,
	
	MESSAGE_EXEC_ASYNC,
	MESSAGE_EXEC_TIMEOUT_READY,
	
	MESSAGE_TIMEOUT,
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