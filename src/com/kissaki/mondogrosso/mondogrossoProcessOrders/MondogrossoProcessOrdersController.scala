package com.kissaki.mondogrosso.mondogrossoProcessOrders
import scala.collection.mutable.ListBuffer
import com.kissaki.Messenger
import com.kissaki.TagValue
import com.kissaki.MessengerProtocol
import com.kissaki.Messenger
import java.util.UUID
import scala.collection.mutable.MapBuilder
import scala.sys.process.{ Process ⇒ scalaProcess }
import java.util.Date

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
			context.runContext
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

	//Processの名簿
	val processList : ListBuffer[String] = ListBuffer()

	//現在実行しているIndex(全体を並べたときをもとにした順、実行順とは異なり、個数)
	var currentOrderIndex = DEFAULT_INDEX

	//実行中のOrderのList
	val currentExecutingOrders : ListBuffer[String] = ListBuffer()

	//初期コンテキスト
	val initialContext = contextSrc.initialParam

	//コンテキスト Map(identity:String -> Map(key:String -> value:String)) 実行後の結果入力で上書きされていく。
	val currentContext = contextSrc.initialParam

	/**
	 * 現在のコンテキストのIdを返す
	 */
	def identity = processIdentity

	def processNum = contextSrc.totalProcessNum

	def totalOrderNum = contextSrc.totalOrderCount

	val finallyOrder = contextSrc.finallyOrder

	/**
	 * このコンテキストの現在のindexからの実行開始
	 */
	def runContext = {
		println("コンテキスト	" + processIdentity)

		println("currentOrderIndex	" + currentOrderIndex)

		if (currentOrderIndex == DEFAULT_INDEX) { //新しいプロセスが生まれるタイミング

			//新しいProcessを登録する
			val newProcessName = UUID.randomUUID().toString()
			processList += newProcessName

			//Workerを作成
			new ProcessWorker(newProcessName, processIdentity)

			//次に開始すべきIdentityを取得する
			val currentOrderIdentity = contextSrc.current.processList(0).orderIdentityList(currentOrderIndex)

			//入力するkey-valueを用意する
			val initial = contextSrc.initialParam.get(currentOrderIdentity) match {
				case Some(v) => v
				case None => Map("" -> "") //エラー
			}

			val kind = initial(OrderPrefix._kind.toString)
			val main = initial(OrderPrefix._main.toString)

			//initialに対して、currentContextに同名の値があれば上書きする
			val currentEventual = currentContext.get(currentOrderIdentity) match {
				case Some(v) => v
				case None => Map()
			}

			val actual = initial ++ currentEventual

			messenger.call(newProcessName, Messages.MESSAGE_START.toString, messenger.tagValues(
				new TagValue("identity", currentOrderIdentity),
				new TagValue("context", actual)))

			//実行中のOrderをセット
			currentExecutingOrders += currentOrderIdentity
		}

	}

	/**
	 * レシーバ
	 */
	def receiver(execSrc : String, tagValues : Array[TagValue]) = {

		Messages.get(execSrc) match {
			case Messages.MESSAGE_SYNCRONOUSLY_STARTED => {
				println("currentOrderIndex	started	" + currentOrderIndex + "	/tagValues	" + tagValues)
			}

			case Messages.MESSAGE_ASYNCRONOUSLY_STARTED => {
				println("currentOrderIndex	async started	" + currentOrderIndex + "	/tagValues	" + tagValues)
			}

			case Messages.MESSAGE_DONE => {
				val currentFinishedWorkerIdentity = messenger.get("identity", tagValues).asInstanceOf[String]
				val currentFinishedOrderIdentity = messenger.get("orderIdentity", tagValues).asInstanceOf[String]
				val currentFinishedEventualContext = messenger.get("eventualContext", tagValues).asInstanceOf[scala.collection.mutable.Map[String, String]]

				println("currentFinishedWorkerIdentity	" + currentFinishedWorkerIdentity)
				println("currentFinishedOrderIdentity	" + currentFinishedOrderIdentity)
				println("currentFinishedEventualContext	" + currentFinishedEventualContext)

				//eventualを、currentContextに上書きする
				val appendedContext = (currentFinishedOrderIdentity -> currentFinishedEventualContext)
				currentContext += appendedContext

				//非同期で、Contextが調整されたことを全Workerに通達する -> Workerからのリクエストを待つ(Wait状態になっていればRequestが来る。)
				notifyFinishedOrderInfoToAllWorker(currentFinishedOrderIdentity)
			}

			/*
			 * WorkerからのOrderリクエストを受ける
			 * 
			 * そのWorkerへの次のOrderが無ければ、対象Workerを停止する
			 * 全Workerが停止されたら、Context自体のfinallyを実行する
			 */
			case Messages.MESSAGE_REQUEST => {
				println("MESSAGE_REQUEST	に到着")
				val processIdentity = messenger.get("workerIdentity", tagValues).asInstanceOf[String]
				val finishedOrderIdentity = messenger.get("finishedOrderIdentity", tagValues).asInstanceOf[String]

				processList.withFilter(_.equals(processIdentity)).foreach { n =>
					println("answered to n = " + n)
					//対象となるプロセスの次のcontextを渡す	/	 停める
				}
			}

			case other => {
				println("未整理のメッセージ	" + other)
			}
		}
	}

	/**
	 * 存在する有効なWorkerに直近のOrderの終了通知を投げる
	 */
	def notifyFinishedOrderInfoToAllWorker(finishedOrderIdentity : String) = {
		for (processName <- processList) {
			println("processName	" + processName)
			messenger.callWithAsync(processName, Messages.MESSAGE_FINISHEDORDER_NOTIFY.toString, messenger.tagValues(
				new TagValue("finishedOrderIdentity", finishedOrderIdentity)))
		}
	}
}