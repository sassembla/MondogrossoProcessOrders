package com.kissaki.mondogrosso.mondogrossoProcessOrders
import com.kissaki.MessengerProtocol
import com.kissaki.Messenger
import com.kissaki.TagValue
import java.util.UUID
import com.kissaki.mondogrosso.mondogrossoProcessOrders.option.WriteOnceFileWriter
import java.io.File

/**
 * Worker挙動のテストのためのMessenger
 */
class DummyWorkerParent(testName:String) extends MessengerProtocol {
	val uuid = UUID.randomUUID().toString
	val messenger = new Messenger(this, uuid)
	def name = messenger.getName

	val fileWriteReceiver = new WriteOnceFileWriter(uuid) //最終一発書き出し
	
	def outputLog = {
		val path = "workerTestLogs/"+testName+".text"
		val file : File = new File(path)
		fileWriteReceiver.writeoutLog(file)
	}


	def receiver(exec:String, tagValues:Array[TagValue]) = {
		
		println("DummyParent exec	"+exec)
		
		fileWriteReceiver.addLog(testName+"@"+exec, tagValues)

		exec match {
			case "wait" => {
				val id = messenger.get("delayId", tagValues).asInstanceOf[String]
				println("dummyParent start to sleep	"+id)
				
				val delay = messenger.get("delay", tagValues).asInstanceOf[Int]
				Thread.sleep(delay)
				println("dummyParent wake from sleep	"+id)
			}

			case other => {
				val messagesExec = WorkerMessages.get(other)
				messagesExec match {
					case WorkerMessages.MESSAGE_SYNCRONOUSLY_STARTED =>
					case WorkerMessages.MESSAGE_ASYNCRONOUSLY_STARTED =>
						
					case WorkerMessages.MESSAGE_ERROR => {
						val erroredWorkerIdentity = messenger.get("erroredWorkerIdentity", tagValues).asInstanceOf[String]
						val erroredOrderIdentity = messenger.get("erroredOrderIdentity", tagValues).asInstanceOf[String]
						
						println("MESSAGE_ERROR	finishedOrderIdentity	"+erroredOrderIdentity	+"	/processIdentity	"+erroredWorkerIdentity)
						
						
						//messengerのログに追加
						messenger.addLog(erroredWorkerIdentity+erroredOrderIdentity)
					}
					case WorkerMessages.MESSAGE_TIMEOUT => {
						val timeoutedWorkerIdentity = messenger.get("timeoutedWorkerIdentity", tagValues).asInstanceOf[String]
						val timeoutedOrderIdentity = messenger.get("timeoutedOrderIdentity", tagValues).asInstanceOf[String]
						
						println("MESSAGE_TIMEOUT	finishedOrderIdentity	"+timeoutedOrderIdentity	+"	/processIdentity	"+timeoutedWorkerIdentity)
						
						
						//messengerのログに追加
						messenger.addLog(timeoutedWorkerIdentity+timeoutedOrderIdentity)
					}
					case WorkerMessages.MESSAGE_DONE => {
						val currentFinishedWorkerIdentity = messenger.get("identity", tagValues).asInstanceOf[String]
						val currentFinishedOrderIdentity = messenger.get("orderIdentity", tagValues).asInstanceOf[String]
						
						println("MESSAGE_DONE	finishedOrderIdentity	"+currentFinishedOrderIdentity	+"	/processIdentity	"+currentFinishedWorkerIdentity)
						
						//messengerのログに追加
						messenger.addLog(currentFinishedWorkerIdentity+currentFinishedOrderIdentity)
					}
					case WorkerMessages.MESSAGE_REQUEST => {
						val processIdentity = messenger.get("workerIdentity", tagValues).asInstanceOf[String]
						val finishedOrderIdentity = messenger.get("finishedOrderIdentity", tagValues).asInstanceOf[String]
					
						println("MESSAGE_REQUEST	finishedOrderIdentity	"+finishedOrderIdentity	+"	/processIdentity	"+processIdentity)
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