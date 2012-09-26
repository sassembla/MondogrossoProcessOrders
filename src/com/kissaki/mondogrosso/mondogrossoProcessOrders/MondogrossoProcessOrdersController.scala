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
//				println("before	currentContext	" + currentContext)
				val appendedContext = (currentFinishedOrderIdentity -> currentFinishedEventualContext)
				currentContext += appendedContext
//				println("after	currentContext	" + currentContext)
			}

			case other => {
				println("未整理のメッセージ	" + other)
			}
		}
	}
}

case class WorkInformation(orderIdentity : String, localContext : scala.collection.Map[String, String])
/**
 * ワーカー
 * Messagingで親(Context)との通信を行う
 */
class ProcessWorker(identity : String, masterName : String) extends MessengerProtocol {
	val RUN_PREFIX_JAVA = "java"
	val RUN_PREFIX_JAR = "-jar"
	val RUN_PREFIX_DOTJAR = ".jar"
	val RUN_TIMEOUT_MESSAGE = "timeout"
	val RUN_PREFIX_CURRENTDIR = "./"
	val WHITESPACE = " "
	val messenger = new Messenger(this, identity)
	messenger.inputParent(masterName)

	//このworkerのidentity
	val workerIdentity = identity

	//実行中のWork情報履歴
	val currentWorkInformationHistory : ListBuffer[WorkInformation] = ListBuffer()

	def getLatestWorkInformation = {
		currentWorkInformationHistory.head
	}

	//状態
	var currentStatus = WorkerStatus.STATUS_READY

	/**
	 * レシーバ
	 */
	def receiver(execSrc : String, tagValues : Array[TagValue]) = {
		Messages.get(execSrc) match {
			case Messages.MESSAGE_START => {

				val orderIdentity = (messenger.get("identity", tagValues)).asInstanceOf[String]
				val orderContext = (messenger.get("context", tagValues)).asInstanceOf[scala.collection.Map[String, String]]

				val currentAddedOrderInfo = new WorkInformation(orderIdentity, orderContext)

				//currentWorkInformationHistoryのheadに、最新のInfoを加算する
				currentAddedOrderInfo +=: currentWorkInformationHistory

				//タイムアウトの設定があれば、リミットをセットする
				orderContext.contains(OrderPrefix.__timeout.toString) match {
					case true => {
						val limit = orderContext.get(OrderPrefix.__timeout.toString) match {
							case Some(v) => setTimeout(currentAddedOrderInfo)
							case None => messenger.callParent(Messages.MESSAGE_ERROR.toString, messenger.tagValues(new TagValue("reason", OrderErrors.ERROR_NO_TIMEOUT_VALUE.toString)))
						}
					}
					case false =>
				}

				//非同期	/	同期
				orderContext.contains(OrderPrefix.__async.toString) match {
					case true => {
						//動作開始の通知
						messenger.callParent(Messages.MESSAGE_ASYNCRONOUSLY_STARTED.toString,
							messenger.tagValues(new TagValue("workerIdentity", identity),
								new TagValue("orderIdentity", orderIdentity)))

						//自分自身を非同期で実行する
						messenger.callMyselfWithAsync(Messages.MESSAGE_EXEC_ASYNC.toString, messenger.tagValues(new TagValue("currentAddedOrderInfo", currentAddedOrderInfo)))
					}
					case false => {
						//動作開始の通知
						messenger.callParent(Messages.MESSAGE_SYNCRONOUSLY_STARTED.toString,
							messenger.tagValues(new TagValue("workerIdentity", identity),
								new TagValue("orderIdentity", orderIdentity)))
						run(currentAddedOrderInfo)
					}
				}
			}

			//Timeout
			case Messages.MESSAGE_EXEC_TIMEOUT_READY => {
				val timeoutContextWorkInfo = messenger.get("timeoutContext", tagValues).asInstanceOf[WorkInformation]

				val delay = timeoutContextWorkInfo.localContext.get(OrderPrefix.__timeout.toString) match {
					case Some(v) => v.toInt
					case _ => 0 //事前にメソッド呼び出し前にセットしてあるので、到達不可能
				}

				Thread.sleep(delay)
				/*-----------時間切れ-----------*/

				//結果のコンテキストを作成する
				val timeoutContext = timeoutContextWorkInfo.localContext ++ Map(OrderPrefix._result.toString -> (RUN_TIMEOUT_MESSAGE + WHITESPACE + delay + "msec elapsed"))

				//結果を残す
				new WorkInformation(timeoutContextWorkInfo.orderIdentity, timeoutContext) +=: currentWorkInformationHistory

				//timeoutの見なし　既存workの情報送信を行う
				messenger.callParentWithAsync(Messages.MESSAGE_TIMEOUT.toString, messenger.tagValues(
					new TagValue("timeoutOrderIdentity", timeoutContextWorkInfo.orderIdentity),
					new TagValue("timeoutOrderContext", timeoutContextWorkInfo.localContext)))

				currentStatus = WorkerStatus.STATUS_TIMEOUT
			}

			//非同期実行
			case Messages.MESSAGE_EXEC_ASYNC => {
				val currentAddedOrderInfo = messenger.get("currentAddedOrderInfo", tagValues).asInstanceOf[WorkInformation]
				run(currentAddedOrderInfo)
			}

			case other => {
				println("未整理の何か	" + other)
			}
		}
	}

	/**
	 * timeout Contextのセット
	 */
	def setTimeout(timeoutContext : WorkInformation) = {

		messenger.callMyselfWithAsync(Messages.MESSAGE_EXEC_TIMEOUT_READY.toString,
			messenger.tagValues(new TagValue("timeoutContext", timeoutContext)))

	}

	/**
	 * 実行
	 */
	def run(info : WorkInformation) = {

		//開始
		currentStatus = WorkerStatus.STATUS_DOING

		//予約語以外
		val currentKeys = OrderPrefix.getXORWithKeyword(info.localContext.keys.toSet)

		info.localContext(OrderPrefix._kind.toString) match {

			/*
			 * jar実行
			 */
			case WorkKinds.kindJar => {
				val inputKeyValuePair = for (key <- currentKeys) yield {
					val value = info.localContext.get(key) match {
						case Some(v) => v
						case None => ""
					}
					key + WHITESPACE + value
				}

				val reduced = inputKeyValuePair match {
					case v if (v.isEmpty) => ""
					case default => {
						inputKeyValuePair.reduceLeft {
							(total, add) => total + WHITESPACE + add
						}
					}
				}

				val stableCommand = RUN_PREFIX_JAVA + WHITESPACE +
					RUN_PREFIX_JAR + WHITESPACE +
					RUN_PREFIX_CURRENTDIR + info.localContext(OrderPrefix._main.toString) +
					RUN_PREFIX_DOTJAR + WHITESPACE +
					reduced

				//process生成
				val process = scalaProcess(stableCommand)

				//実行
				runProcess(info, process)
			}

			/*
			 * process実行
			 */
			case WorkKinds.kindProcess => {
				val inputKeyValuePair = for (key <- currentKeys) yield {
					val value = info.localContext.get(key) match {
						case Some(v) => v
						case None => ""
					}
					key + WHITESPACE + value
				}

				val reduced = inputKeyValuePair match {
					case v if (v.isEmpty) => ""

					case default => {
						inputKeyValuePair.reduceLeft {
							(total, add) => total + WHITESPACE + add
						}
					}
				}

				val stableCommand = info.localContext(OrderPrefix._main.toString) + WHITESPACE + reduced

				println("shell stableCommand	" + stableCommand)

				//プロセス生成
				val process = scalaProcess(stableCommand)

				//実行
				runProcess(info, process)
			}
			case other => {
				println("type not found	:" + other)
			}
		}
	}

	/**
	 * プロセスの実行
	 *
	 * 成功するか、実行時エラーか
	 */
	def runProcess(info : WorkInformation, process : scala.sys.process.ProcessBuilder) = {
		try {
			val result = process.!!

			currentStatus match {
				case WorkerStatus.STATUS_DOING => {
					//結果のコンテキストを作成する
					val eventualContext = info.localContext ++ Map(OrderPrefix._result.toString -> result)

					//結果を残す
					new WorkInformation(info.orderIdentity, eventualContext) +=: currentWorkInformationHistory

					//親に終了を通知
					messenger.callParent(Messages.MESSAGE_DONE.toString,
						messenger.tagValues(
							new TagValue("identity", identity),
							new TagValue("orderIdentity", info.orderIdentity),
							new TagValue("eventualContext", eventualContext)))

					currentStatus = WorkerStatus.STATUS_DONE
				}
				case _ =>
			}
		} catch {
			case e => {
				//error結果のコンテキストを作成する
				val errorContext = info.localContext ++ Map(OrderPrefix._result.toString -> e.toString)
				println("errorContext	" + errorContext)
				//結果を残す
				new WorkInformation(info.orderIdentity, errorContext) +=: currentWorkInformationHistory

				//親に終了を通知
				messenger.callParent(Messages.MESSAGE_ERROR.toString,
					messenger.tagValues(
						new TagValue("identity", identity),
						new TagValue("orderIdentity", info.orderIdentity),
						new TagValue("eventualContext", errorContext)))
				currentStatus = WorkerStatus.STATUS_ERROR
			}
		}
	}
}
	
