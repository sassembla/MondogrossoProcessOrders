package com.kissaki.mondogrosso.mondogrossoProcessOrders
import scala.collection.mutable.ListBuffer
import com.kissaki.Messenger
import com.kissaki.TagValue
import com.kissaki.MessengerProtocol
import com.kissaki.Messenger
import java.util.UUID

/**
 * ランナー
 *
 * プロセスごとのコンテキストを持つ。
 */
class MondogrossoProcessOrdersController {

	//それぞれのProcessの実行がされるコンテキスト
	val contexts : ListBuffer[ProcessContext] = ListBuffer()

	/**
	 * 新しいプロセスをアタッチする
	 */
	def attachProcess(identity : String, processSource : Any) = {
		println("attach	" + processSource)
		contexts += new ProcessContext(identity, processSource)
	}

	/**
	 * 準備済みのコンテキストを実行する
	 */
	def runAllContext = {
		contexts.withFilter(_.currentCondition == Condition.CONDITION_PREPARED).foreach { context =>
			context.run
		}
	}

	/**
	 * コンテキストを空にする
	 */
	def clearContext = {
		contexts.clear
	}
}

/**
 * コンテキスト
 * メッセージングのコアになる。
 */
class ProcessContext(processIdentity : String, processSource : Any) extends MessengerProtocol {
	val messenger = new Messenger(this, processIdentity)
	
	val MESSAGE_START = "MESSAGE_START"
	val MESSAGE_ERROR = "MESSAGE_ERROR"
	val MESSAGE_DONE = "MESSAGE_DONE"
	
	//ワーカーの名簿
	val workersNameList : ListBuffer[String] = ListBuffer()

	
	var orderIndex = -1
	var condition = Condition.CONDITION_PREPARED

	//このコンテキストが所持するプロセスのID一覧
	val processList:ListBuffer[String] = ListBuffer()
	
	
	/**
	 * 現在のコンテキストのIdを返す
	 */
	def identity = processIdentity

	def processNum = processList.size

	def totalOrderNum = 1

	def currentCondition = {
		condition match {
			case Condition.CONDITION_PREPARED => {

			}
		}
	}

	/**
	 * このコンテキストの現在のindexからの実行開始
	 */
	def run = {
		//for 必要な数のworkerを動かす
		val newWorkerName = UUID.randomUUID().toString()
		workersNameList += newWorkerName
		new ProcessWorker(newWorkerName, processIdentity)
	}
	
	
	/**
	 * レシーバ
	 */
	def receiver(exec : String, tagValues : Array[TagValue]) = {
//		exec match {
//			case MESSAGE_START => {
//				val mode = messenger.get("mode", tagValues).asInstanceOf[Int]
//				start(mode)
//			}
//		}
	}
	
}


/**
 * ワーカー
 * Messagingで親(Context)との通信を行う
 */
class ProcessWorker (identity:String, masterName:String) extends MessengerProtocol {
	val messenger = new Messenger(this, identity)
	messenger.inputParent(masterName)

	val MESSAGE_START = "MESSAGE_START"
	val MESSAGE_ERROR = "MESSAGE_ERROR"
	val MESSAGE_DONE = "MESSAGE_DONE"
	
	/**
	 * レシーバ
	 */
	def receiver(exec : String, tagValues : Array[TagValue]) = {
		exec match {
			case MESSAGE_START => {
				val mode = messenger.get("mode", tagValues).asInstanceOf[Int]
				start(mode)
			}
		}
	}
	
	
	def start(mode:Int) = {
		
	}
	
}

