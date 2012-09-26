package com.kissaki.mondogrosso.mondogrossoProcessOrders

object OrderErrors extends Enumeration {
	
	type OrderErrors = Value
	
	//ERRORS
	val ERROR_NO_TIMEOUT_VALUE,
	ERROR_NOTFOUND
	= Value
	
	def get(in:String):Value = {
		values.find(in == _.toString) match {
			case None => ERROR_NOTFOUND
			case Some(v) => v
		}
	} 
}