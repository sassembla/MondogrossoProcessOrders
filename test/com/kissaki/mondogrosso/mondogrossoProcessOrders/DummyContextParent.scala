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
		
	}
}