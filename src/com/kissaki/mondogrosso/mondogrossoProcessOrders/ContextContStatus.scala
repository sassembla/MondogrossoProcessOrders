package com.kissaki.mondogrosso.mondogrossoProcessOrders

object ContextContStatus extends Enumeration {
	
	type ContextContStatus = Value
	
	//CONTEXT_CONT_STATUS
	val STATUS_EMPTY,
	STATUS_RUNNING,
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