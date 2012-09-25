package com.kissaki.mondogrosso.mondogrossoProcessOrders

object OrderPrefix extends Enumeration {
	type OrderPrefix = Value
	
	//PREFIX
	val
	//must
	_kind,
	_main,
	_result,
	
	//optional
	__trigger,
	__timeout,
	
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