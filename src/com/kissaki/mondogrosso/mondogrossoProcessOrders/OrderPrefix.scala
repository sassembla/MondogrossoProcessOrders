package com.kissaki.mondogrosso.mondogrossoProcessOrders

object OrderPrefix extends Enumeration {
	
	val RUN_PREFIX_JAVA = "java"
	val RUN_PREFIX_JAR = "-jar"
	val RUN_PREFIX_DOTJAR = ".jar"
	val RUN_TIMEOUT_MESSAGE = "timeout"
	val RUN_PREFIX_CURRENTDIR = "./"
	val RUN_PREFIX_WHITESPACE = " "
		
	type OrderPrefix = Value
	
	//PREFIX
	val
	/*must*/
	_kind,
	_main,
	_result,
	
	/*optional*/
	__delay,
	__timeout,
	
	/*finally*/
	__finallyTimeout,
	
	PREFIX_NOTFOUND
	= Value
	
	def getXORWithKeyword(keySet:Set[String]) = {
		//この入力集合と、valuesをtoStringした物との差を取る
		val defKeys = for (v <- values) yield v.toString
		
		//x o r
		keySet.filter(v => !defKeys.contains(v))
	}
	
	//一致するValueを返す
	def get(in:String):Value = {
		values.find(in == _.toString) match {
			case None => {
				PREFIX_NOTFOUND
			}
			case Some(v) => {
				v
			}
		}
	} 
}