package com.kissaki.mondogrosso.mondogrossoProcessOrders
import com.kissaki.Messenger
import java.util.UUID
import com.kissaki.TagValue
import com.kissaki.MessengerProtocol

/**
 * ContextControllerの親(ProcessOrder)のダミー
 * 実際にはProcessOrderが負う。
 */
class DummyProcessOrder (masterName:String) extends MessengerProtocol {
	val messenger = new Messenger(this, masterName)
	def name = messenger.getName

	def receiver(exec:String, tagValues:Array[TagValue]) = {
		//特に何も解釈しないつもり
	}
}