package com.kissaki.mondogrosso.mondogrossoProcessOrders
import com.kissaki.MessengerProtocol
import com.kissaki.Messenger
import com.kissaki.TagValue

/**
 * Worker挙動のテストのためのMessenger
 */
class DummyParent extends MessengerProtocol {
	val messenger = new Messenger(this, "DUMMY_PARENT")
	def name = messenger.getName
	
	def receiver(exec:String, tagValues:Array[TagValue]) = {
		println("DummyParent exec	"+exec)
	}
}

/**
 * Context挙動のテストのためのMessenger
 */
class DummyChild extends MessengerProtocol {
	val messenger = new Messenger(this, "DUMMY_CHILD")
	def setParent(parentName:String) = messenger.inputParent(parentName)
	
	
	def receiver(exec:String, tagValues:Array[TagValue]) = {
		println("DummyChild	exec	"+exec)
	}
}