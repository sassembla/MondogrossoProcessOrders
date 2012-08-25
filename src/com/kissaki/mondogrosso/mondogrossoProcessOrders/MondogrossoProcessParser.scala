package com.kissaki.mondogrosso.mondogrossoProcessOrders

import scala.collection.mutable._

/**
 * プロセスのパーサ
 * 文字列入力を受けて、内容をパースする。
 */
class MondogrossoProcessParser(input : String) {

	val finallySeq = ""
	val processNum = 1
	val totalSequenceNum = 1
	val eachSequenceNum = Seq(1, 1, 1, 1, 1)

	val processes : ListMap[String, Process] = ListMap()

	
	
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

	/**
	 * プロセス
	 */
	class Process(processName:String) {
		val orders:Seq[Order] = Seq()
		val eachOrdersLength = Seq(1);

		def getEachOrdersLength :Seq[Int] = {
			eachOrdersLength
		}
		
		/**
		 * オーダー
		 */
		class Order(orderIdentity:String) {
			
		
		}
	}

}