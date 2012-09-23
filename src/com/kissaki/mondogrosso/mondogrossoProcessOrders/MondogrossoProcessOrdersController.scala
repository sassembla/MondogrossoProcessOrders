package com.kissaki.mondogrosso.mondogrossoProcessOrders
import scala.collection.mutable.ListBuffer
import com.kissaki.Messenger
import com.kissaki.TagValue
import com.kissaki.MessengerProtocol
import com.kissaki.Messenger
import java.util.UUID
import scala.collection.mutable.MapBuilder

/**
 * プロセスのコントローラ
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

	val DEFAULT_INDEX = 0

	//コンテキストソース
	val currentContext = contextSrc

	//ワーカーの名簿
	val workersNameList : ListBuffer[String] = ListBuffer()

	//現在実行しているIndex(全体を並べたときをもとにした順、実行順とは異なり、個数)
	var currentOrderIndex = DEFAULT_INDEX

	//実行中のOrderのList
	val currentExecutingOrders : ListBuffer[String] = ListBuffer()

	/**
	 * 現在のコンテキストのIdを返す
	 */
	def identity = processIdentity

	def processNum = contextSrc.totalProcessNum

	def totalOrderNum = currentContext.totalOrderCount

	/**
	 * このコンテキストの現在のindexからの実行開始
	 */
	def run = {
		println("コンテキスト	" + processIdentity)

		println("currentOrderIndex	" + currentOrderIndex)

		//		currentContext.finallyOrderをfinally節に予約

		//ここから先は、Workerに送りこみ、待つだけ。
		val currentOrderIdentity = currentContext.current.processList(0).orderIdentityList(currentOrderIndex)

		//入力するkey-valueを用意する

		val initial = currentContext.initialParam(currentOrderIdentity)
		println("current initial	" + initial)

		//新しいWorkerを生成する
		val newWorkerName = UUID.randomUUID().toString()
		workersNameList += newWorkerName

		new ProcessWorker(newWorkerName, processIdentity)
		messenger.call(newWorkerName, "adada", messenger.tagValues(
			new TagValue("orderIdentity", currentOrderIdentity)))

		//実行中のOrderをセット
		currentExecutingOrders += currentOrderIdentity
	}

	/**
	 * レシーバ
	 */
	def receiver(execSrc : String, tagValues : Array[TagValue]) = {

		val exec = Messages.get(execSrc)
		println("なんか来た	execSrc	" + execSrc + "	/exec	" + exec)
		//		exec match {
		//			case MESSAGE_STARTED => {
		//				println("currentOrderIndex	started	" + currentOrderIndex + "	/tagValues	" + tagValues)
		//			}
		//
		//			case MESSAGE_DONE => {
		//				println("MESSAGE_DONE	tagValues	" + tagValues)
		//				//				currentOrderIndex++
		//			}
		//		}
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
	def receiver(execSrc : String, tagValues : Array[TagValue]) = {
		val exec = Messages.get(execSrc)
		println("なんか来た2	execSrc" + execSrc + "	/exec	" + exec)
		//		exec match {
		//			case MESSAGE_START => {
		//				//ここで、あらゆる情報を受け取る
		//				println("identity	" + identity + "	/tagValues	" + tagValues)
		//
		//				//動作開始の通知
		//				messenger.callParent(MESSAGE_STARTED,
		//					messenger.tagValues(new TagValue("identity", identity)))
		//
		//				run
		//			}
		//		}
	}

	def run = {

		//挙動完了
		//		messenger.callParent(MESSAGE_DONE,
		//			messenger.tagValues(new TagValue("identity", identity)))
	}
}
	
