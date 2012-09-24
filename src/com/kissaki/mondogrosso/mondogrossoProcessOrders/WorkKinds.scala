package com.kissaki.mondogrosso.mondogrossoProcessOrders

object WorkKinds extends Enumeration {
	
	val kindJar = "jar"
	val kindProcess = "sh"
	
	type WorkKinds = Value
	
	//WORK_KINDS
	val KIND_PROCESS,
	KIND_JAR,
	KIND_NONE
	= Value
	
	def get(in:String):Value = {
		values.find(in == _.toString) match {
			case None => {
				KIND_NONE
			}
			case Some(v) => {
				v
			}
		}
	} 
}