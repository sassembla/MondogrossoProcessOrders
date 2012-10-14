package com.kissaki.mondogrosso.mondogrossoProcessOrders
import com.kissaki.Messenger
import java.util.UUID
import com.kissaki.TagValue
import com.kissaki.MessengerProtocol

/**
 * Contextの親のダミー
 * 実際にはContextControllerが負う。
 */
class DummyContextParent (masterName:String) extends MessengerProtocol {
	val messenger = new Messenger(this, masterName)
	def name = messenger.getName

	def receiver(exec:String, tagValues:Array[TagValue]) = {
		val messagesExec = ContextMessages.get(exec)
		messagesExec match {
			case ContextMessages.MESSAGE_READY => {
				println("MESSAGE_READY")
				tagValues.foreach{ println }
			}
			case ContextMessages.MESSAGE_START => {
				println("MESSAGE_START")
				tagValues.foreach{ println }
			}
			case ContextMessages.MESSAGE_PROCEEDED => {
				println("MESSAGE_PROCEEDED")
				tagValues.foreach{ println }
			}
			case ContextMessages.MESSAGE_DONE => {
				println("MESSAGE_DONE")
				tagValues.foreach{ println }
			}
			case ContextMessages.MESSAGE_ERROR => {
				println("MESSAGE_ERROR")
				tagValues.foreach{ println }
			}
			case ContextMessages.MESSAGE_TIMEOUT => {
				println("MESSAGE_TIMEOUT")
				tagValues.foreach{ println }
			}
		}
	}
}