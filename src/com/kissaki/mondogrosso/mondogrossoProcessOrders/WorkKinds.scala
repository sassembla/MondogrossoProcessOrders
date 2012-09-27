package com.kissaki.mondogrosso.mondogrossoProcessOrders

object WorkKinds extends Enumeration {
	
	type WorkKinds = Value
	
	//WORK_KINDS
	val jar,
	sh,
	KIND_NOT_FOUND
	= Value
	
	def get(in:String):Value = {
		values.find(in == _.toString) match {
			case None => {
				KIND_NOT_FOUND
			}
			case Some(v) => {
				v
			}
		}
	} 
}