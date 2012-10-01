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
	
	def waitTime (delay:Int, delayId:String) = {
		println("delay	"+delay+",	withId	"+delayId)
		messenger.callMyself("wait", 
				messenger.tagValues(new TagValue("delay", delay),
						new TagValue("delayId",delayId)))
	}
	
	def receiver(exec:String, tagValues:Array[TagValue]) = {
		println("DummyParent exec	"+exec)
		exec match {
			case "wait" => {
				val id = messenger.get("delayId", tagValues).asInstanceOf[String]
				println("dummyParent start to sleep	"+id)
				
				val delay = messenger.get("delay", tagValues).asInstanceOf[Int]
				Thread.sleep(delay)
				println("dummyParent wake from sleep	"+id)
			}
			case other => {
				val messagesExec = Messages.get(other)
				messagesExec match {
					case Messages.MESSAGE_SYNCRONOUSLY_STARTED =>
					case Messages.MESSAGE_ASYNCRONOUSLY_STARTED =>
						
					case Messages.MESSAGE_ERROR => {
						val erroredWorkerIdentity = messenger.get("erroredWorkerIdentity", tagValues).asInstanceOf[String]
						val erroredOrderIdentity = messenger.get("erroredOrderIdentity", tagValues).asInstanceOf[String]
						
						//messengerのログに追加
						messenger.addLog(erroredWorkerIdentity+erroredOrderIdentity)
					}
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