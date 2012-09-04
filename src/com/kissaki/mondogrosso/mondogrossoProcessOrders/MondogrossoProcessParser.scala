package com.kissaki.mondogrosso.mondogrossoProcessOrders

import scala.collection.mutable._

/*
 * AST
 */
case class Process(orders:Array[Orders], finallyOrder:Order)
case class Orders(ordersId:String, orders:Array[Order])
case class Order(orderIdentity:String,keyValues:OrderKeyValues,waitForOrder:Order)
case class OrderKeyValues(keyValues:Array[OrderKeyValue])
case class OrderKeyValue(promisses:OrderInputPromisses)
case class OrderInputPromisses(promissesArray:Array[OrderInputPromiss])
case class OrderInputPromiss(sourceIdentity:String,sourceKey:String,destinationKey:String)

/**
 * プロセスのパーサ
 * 文字列入力を受けて、内容をパースする。
 */
class MondogrossoProcessParser(originalProcessesSource : String, json : String) {
	println("processesSource is " + originalProcessesSource + "	/json	" + json)

	val PREFIX_FINALLY = "!"
	val PREFIX_PROCESS_DELIM = "./"
	val PREFIX_ORDERS_DELIM = ">"

	

	//
	val contextId = "sample"
	var classDescription = "testasas"

	def getContextId : String = {
		contextId
	}

	/**
	 * 吐き出すクラス記述の文字列。ここからコンパイル開始
	 */
	def getContextClassDesctiption : String = {
		classDescription
	}

	def generateClassDescription() : String = {
		/*
		 * ジェネレート処理を行う。
		 * 上記までの情報から、importやstateをくみ上げる。
		 */

		classDescription
	}
}