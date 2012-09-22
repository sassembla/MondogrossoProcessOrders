package com.kissaki.mondogrosso.mondogrossoProcessOrders
import scala.collection.mutable.ListBuffer
import com.kissaki.Messenger
import com.kissaki.TagValue
import com.kissaki.MessengerProtocol
import com.kissaki.Messenger
import java.util.UUID
import scala.collection.mutable.MapBuilder

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
	def attachProcess(identity : String, contextSrc : ContextSource) = {
		println("attach	Context" + contextSrc)
		contexts += new ProcessContext(identity, contextSrc)
	}

	/**
	 * 準備済みのコンテキストを実行する
	 */
	def runAllContext = {
		contexts.foreach { context =>
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
 * 
 * 各Orderを実行する、Workerメッセージングのコアになる。
 */
class ProcessContext(processIdentity : String, contextSrc : ContextSource) extends MessengerProtocol {
	val messenger = new Messenger(this, processIdentity)

	//MESSAGES
	val MESSAGE_START = "MESSAGE_START"
	val MESSAGE_STARTED = "MESSAGE_STARTED"
	val MESSAGE_ERROR = "MESSAGE_ERROR"
	val MESSAGE_DONE = "MESSAGE_DONE"

	
	val DEFAULT_INDEX = 0
	
	//コンテキストソース
	val currentContext = contextSrc
	
	//ワーカーの名簿
	val workersNameList : ListBuffer[String] = ListBuffer()

	//現在実行しているIndex(全体を並べたときをもとにした順、実行順とは異なり、個数)
	var currentOrderIndex = DEFAULT_INDEX

	//実行中のOrderのList
	val currentExecutingOrders:ListBuffer[String] = ListBuffer()
	
	//このコンテキストが所持するプロセスのID一覧
	val processList : ListBuffer[String] = ListBuffer()

	/**
	 * 現在のコンテキストのIdを返す
	 */
	def identity = processIdentity

	def processNum = processList.size

	def totalOrderNum = currentContext.totalOrderCount
	

	/**
	 * このコンテキストの現在のindexからの実行開始
	 */
	def run = {
		println("コンテキスト	" + processIdentity)
		
		println("currentOrderIndex	"+currentOrderIndex)
		
//		currentContext.finallyOrderをfinally節に予約
		
		//ここから先は、Workerに送りこみ、待つだけ。
		val currentOrderIdentity = currentContext.current.processList(0).orderIdentityList(currentOrderIndex)
		
		println("currentOrderIdentity	"+currentOrderIdentity)
		println("here is	"+currentContext)
		
		//入力するkey-valueを用意する
		
		
		val initial = currentContext.initialParam(currentOrderIdentity)
		println("current initial	"+initial)
		
		
		//新しいWorkerを生成する
		val newWorkerName = UUID.randomUUID().toString()
		workersNameList += newWorkerName

		new ProcessWorker(newWorkerName, processIdentity)
		messenger.call(newWorkerName, MESSAGE_START,messenger.tagValues(
				new TagValue("orderIdentity", currentOrderIdentity)
		))
		
		//実行中のOrderをセット
		currentExecutingOrders += currentOrderIdentity
	}

	/**
	 * レシーバ
	 */
	def receiver(exec : String, tagValues : Array[TagValue]) = {
		exec match {
			case MESSAGE_STARTED => {
				currentOrderIndex = currentOrderIndex+1
				println("currentOrderIndex	"+currentOrderIndex)
			}
		}
	}

	/**
	 * ワーカー
	 * Messagingで親(Context)との通信を行う
	 */
	class ProcessWorker(identity : String, masterName : String) extends MessengerProtocol {
		val messenger = new Messenger(this, identity)
		messenger.inputParent(masterName)

		/**
		 * レシーバ
		 */
		def receiver(exec : String, tagValues : Array[TagValue]) = {
			exec match {
				case MESSAGE_START => {
					//ここで、あらゆる情報を受け取る
					messenger.callParent(MESSAGE_STARTED, Array())
				}
			}
		}
	}
	
}
