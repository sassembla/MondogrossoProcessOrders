package com.kissaki.mondogrosso.mondogrossoProcessOrders.option
import com.kissaki.Messenger
import com.kissaki.MessengerProtocol
import com.kissaki.TagValue
import com.kissaki.mondogrosso.mondogrossoProcessOrders.ProcessOrdersMasterMessages

class FileWriteReceiver (id : String) extends MessengerProtocol {
	val messenger = new Messenger(this, id)
	
	def receiver(exec: String, tagValues: Array[TagValue]) = {
		ProcessOrdersMasterMessages.get(exec) match {
			case ProcessOrdersMasterMessages.MESSAGE_EMPTY => addLog(tagValues)
			case ProcessOrdersMasterMessages.MESSAGE_START => addLog(tagValues)
			case ProcessOrdersMasterMessages.MESSAGE_PROCEEDED =>  addLog(tagValues)
			case ProcessOrdersMasterMessages.MESSAGE_ERROR => addLog(tagValues)
			case ProcessOrdersMasterMessages.MESSAGE_TIMEOUTED => addLog(tagValues)
			case ProcessOrdersMasterMessages.MESSAGE_DONE => addLog(tagValues)
		}
	}
	
	/**
	 * ログ追記
	 */
	def addLog (tagValues : Array[TagValue]) = {
		println("addLog完了")	
	}

	/**
	 * ログ書き出し
	 */
	def writeoutLog = {
		println("writeoutLog完了")
	}

}