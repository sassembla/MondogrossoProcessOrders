package com.kissaki.mondogrosso.mondogrossoProcessOrders

object OrderPrefix extends Enumeration {
	
	val prefixKind = "_kind"
	val prefixMain = "_main"
	
		
	type OrderPrefix = Value
	
	//PREFIX
	val PREFIX_KIND,
	PREFIX_MAIN,
	PREFIX_NOTFOUND
	= Value
	
	def get(in:String):Value = {
		if (in.equals(prefixKind)) return PREFIX_KIND
		if (in.equals(prefixMain)) return PREFIX_MAIN
		return PREFIX_NOTFOUND
	}
}