package com.kissaki.mondogrosso.mondogrossoProcessOrders

object ContextExecs extends Enumeration {
	
	type Execs = Value
	
	//EXECS
	val EXEC_TIMEOUT_RUN,
	EXEC_NOTFOUND
	= Value
	
	def get(in:String):Value = {
		values.find(in == _.toString) match {
			case None => EXEC_NOTFOUND
			case Some(v) => v
		}
	} 
}