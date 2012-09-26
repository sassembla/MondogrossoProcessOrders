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
					case Messages.MESSAGE_TIMEOUT => {
						println("子どもからのTimeoutが届いた")
						messenger.addLog(Messages.MESSAGE_TIMEOUT.toString)
					}
					case _ => {
						
					}
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