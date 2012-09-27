package com.kissaki.mondogrosso.mondogrossoProcessOrders
import com.kissaki.MessengerProtocol
import com.kissaki.Messenger
import com.kissaki.TagValue
import java.util.UUID

/**
 * Worker挙動のテストのためのMessenger
 */
class DummyParent extends MessengerProtocol {
	
	val uuid = UUID.randomUUID().toString
	
	val messenger = new Messenger(this, uuid)
	def name = messenger.getName
	
	def waitTime (delay:Int) = {
		println("delay	"+delay)
		messenger.callMyself("wait", messenger.tagValues(new TagValue("delay", delay)))
	}
	
	def receiver(exec:String, tagValues:Array[TagValue]) = {
		println("DummyParent exec	"+exec)
		exec match {
			case "wait" => {
				println("dummyParent start to sleep")
				
				val delay = messenger.get("delay", tagValues).asInstanceOf[Int]
				Thread.sleep(delay)
				println("dummyParent wake from sleep")
			}
			case other => {
				val messagesExec = Messages.get(other)
				messagesExec match {
					case Messages.MESSAGE_SYNCRONOUSLY_STARTED =>
					case Messages.MESSAGE_ASYNCRONOUSLY_STARTED =>
						
					case Messages.MESSAGE_TIMEOUT => {
						val timeoutedWorkerIdentity = messenger.get("timeoutedWorkerIdentity", tagValues).asInstanceOf[String]
						val timeoutedOrderIdentity = messenger.get("timeoutedOrderIdentity", tagValues).asInstanceOf[String]
						
						//messengerのログに追加
						messenger.addLog(timeoutedWorkerIdentity+timeoutedOrderIdentity)
					}
					case Messages.MESSAGE_DONE => {
						val currentFinishedWorkerIdentity = messenger.get("identity", tagValues).asInstanceOf[String]
						val currentFinishedOrderIdentity = messenger.get("orderIdentity", tagValues).asInstanceOf[String]

						//messengerのログに追加
						messenger.addLog(currentFinishedWorkerIdentity+currentFinishedOrderIdentity)
					}
					case Messages.MESSAGE_REQUEST => {
						val processIdentity = messenger.get("workerIdentity", tagValues).asInstanceOf[String]
						val finishedOrderIdentity = messenger.get("finishedOrderIdentity", tagValues).asInstanceOf[String]
					
						//messengerのログに追加
						messenger.addLog(processIdentity+finishedOrderIdentity)
					}
					case other => println("dummyParent, 想定外のMessageが来た	"+other)
				}
			}
		}
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