package com.kissaki.mondogrosso.mondogrossoProcessOrders

object ContextStatus extends Enumeration {
	
	type ContextStatus = Value
	
	//MESSAGES
	val STATUS_NOTREADY,
	STATUS_READY,
	STATUS_RUNNING,
	
	STATUS_FINALLY,
	STATUS_DONE,
	
	STATUS_TIMEOUT,
	STATUS_TIMEOUTED,
	
	STATUS_ERROR,
	
	STATUS_NOTFOUND
	= Value
	
	def get(in:String):Value = {
		values.find(in == _.toString) match {
			case None => {
				STATUS_NOTFOUND
			}
			case Some(v) => {
				v
			}
		}
	}
}