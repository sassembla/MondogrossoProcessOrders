package com.kissaki.mondogrosso.mondogrossoProcessOrders

object WorkerStatus extends Enumeration {
	
	type WorkerStatus = Value
	
	//MESSAGES
	val STATUS_READY, 
	STATUS_DOING,
	STATUS_DONE,
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