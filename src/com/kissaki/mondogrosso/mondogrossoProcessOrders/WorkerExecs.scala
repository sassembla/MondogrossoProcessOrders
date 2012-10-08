package com.kissaki.mondogrosso.mondogrossoProcessOrders

object WorkerExecs extends Enumeration {
	
	type Exec = Value
	
	//EXECS
	val EXEC_READY_TIMEOUT,
	EXEC_READY_RUN,
	
	EXEC_ASYNC,
	
	EXEC_TIMEOUT_READY,
	EXEC_TIMEOUT_RUN,
	
	EXEC_UNLOCK_AFTERWAIT,
	
	EXEC_DONE,
	EXEC_NOTFOUND
	= Value
	
	def get(in:String):Value = {
		values.find(in == _.toString) match {
			case None => EXEC_NOTFOUND
			case Some(v) => v
		}
	} 
}