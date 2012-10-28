package com.kissaki.mondogrosso.mondogrossoProcessOrders

object WorkerStatus extends Enumeration {
	
	type WorkerStatus = Value
	
	//MESSAGES
	val STATUS_EMPTY,
	STATUS_SETUP,
	STATUS_READY,
	
	STATUS_SPLIT_WAIT,
	STATUS_SPLIT_READY,
	
	STATUS_DOING,

	STATUS_AFTER_WAIT,
	
	STATUS_REQUESTING,

	
	STATUS_TIMEOUT,
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