package com.kissaki.mondogrosso.mondogrossoProcessOrders

import scala.collection.mutable._


/**
 * プロセスのパーサ
 * 文字列入力を受けて、内容をパースする。
 */
class MondogrossoProcessParser(processesSource : String, json : String) {
	println("processesSource is "+processesSource + "	/json	"+ json)
	
	val PREFIX_FINALLY = "!"
	
		
		
	
	//parse "!"
	val finallySeq = processesSource.split(PREFIX_FINALLY)(1)
	
	val processNum = 1
	val totalSequenceNum = 1
	val eachSequenceNum = Seq(1, 1, 1, 1, 1)

	val processes : ListMap[String, Process] = ListMap()

	
	//params for run
	lazy val context = new Context
	
	class Context {
		val contextId = ""
		val classDescription = ""
			
		println("contextId is "+contextId)
		
	
	}
	
	
	
	/**
	 * finally処理のidentityを取得する
	 */
	def getFinally : String = {
		finallySeq
	}

	/**
	 * プロセス数を取得する
	 */
	def getProcessNum : Int = {
		processNum
	}

	/**
	 * 各プロセスの長さを取得する
	 */
	def getEachProcessLength : Seq[Int] = {
		eachSequenceNum
	}

	/**
	 * 名称から特定のプロセスを取得する
	 */
	def getProcess(processName : String) : Option[Process] = {
		processes.get(processName)
	}

	/**
	 * ダミーのプロセスを作成する
	 */
	def createProcess(processName : String) {
		new Process(processName)
	}

	
	def getContextId :String = {
		context.contextId
	}
	
	/**
	 * 吐き出すクラス記述の文字列。ここからコンパイル開始
	 */
	def getContextClassDesctiption :String = {
		context.classDescription
	}
	
	
	/**
	 * プロセス
	 */
	class Process(processName : String) {
		val orders : IndexedSeq[Orders] = IndexedSeq()
		val namedOrders : ListMap[String, Orders] = ListMap()
		val eachOrdersLength = Seq(1);

		def getEachOrdersLength : Seq[Int] = {
			eachOrdersLength
		}

		def getOrdersAt(index : Int) : Orders = {
			orders(index)
		}
		
		def getOrders(targetOrdersName:String) : Option[Orders] = {
			namedOrders.get(targetOrdersName)
		}

		/**
		 * オーダーの集合体
		 */
		class Orders(odersName : String) {
			val waits:Seq[String] = Seq()
			val indexArray:ListMap[Int, Order] = ListMap()
			val nameArray:ListMap[String, Order] = ListMap()
			
			def getWaits :Seq[String] = {
				waits
			}
			
			def getArraySize :Int = {
				indexArray.size
			}
			
			def getOrderAt(index : Int) :Option[Order] = {
				indexArray.get(index)
			}
			
			def getOrder(identifier : String) :Option[Order] = {
				nameArray.get(identifier)
			}
			
			
			/**
			 * オーダー
			 */
			class Order(orderIdentity : String, paramInformationJSON : String) {
				
				/*
				 * JSONのパースを行い、値のセッティングを行う。
				 */
				val keysAndValues:ListMap[String, String] = ListMap("dummyKey" -> "dummyValue")
				
				
				
				def identity:String = {
					orderIdentity
				}
				
				def getAllParamKeysAndValues :ListMap[String, String] = {
					keysAndValues
				}
				
				
				
			}
		}

	}

}