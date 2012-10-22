package com.kissaki.mondogrosso.mondogrossoProcessOrders

object ProcessOrdersModes extends Enumeration {
	
	type ProcessOrdersModes = Value
	
	//MODES
	val MODE_DEFAULT,
	MODE_ATTACH,
	MODE_RUN,
	
	MODE_NOTFOUND
	= Value
	
	def get(in:String):Value = {
		values.find(in == _.toString) match {
			case None => MODE_NOTFOUND
			case Some(v) => v
		}
	} 
}