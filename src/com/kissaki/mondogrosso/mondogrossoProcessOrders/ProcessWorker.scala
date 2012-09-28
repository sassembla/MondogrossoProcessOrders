package com.kissaki.mondogrosso.mondogrossoProcessOrders

import scala.collection.mutable.ListBuffer
import scala.sys.process.{ Process ⇒ scalaProcess }
import com.kissaki.Messenger
import com.kissaki.TagValue
import com.kissaki.MessengerProtocol
import com.kissaki.Messenger
import java.util.UUID

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
	var currentStatus = WorkerStatus.STATUS_NONE
	

	/**
	 * レシーバ
	 */
	def receiver(execSrc : String, tagValues : Array[TagValue]) : Unit = {
		Messages.get(execSrc) match {
			case Messages.MESSAGE_FINISHEDORDER_NOTIFY => {
				val finishedOrderIdentity = messenger.get("finishedOrderIdentity", tagValues).asInstanceOf[String]

				println("finishedOrderIdentity	" + finishedOrderIdentity)
			}

			case Messages.MESSAGE_START => {

				currentStatus = WorkerStatus.STATUS_READY

				val orderIdentity = (messenger.get("identity", tagValues)).asInstanceOf[String]
				val orderContext = (messenger.get("context", tagValues)).asInstanceOf[scala.collection.Map[String, String]]

				val currentAddedOrderInfo = new WorkInformation(orderIdentity, orderContext)

				//currentWorkInformationHistoryのheadに、最新のInfoを加算する
				currentAddedOrderInfo +=: currentWorkInformationHistory

				/*----------実行----------*/

				//validation
				validateKeys(currentAddedOrderInfo) match {
					case true => {
						//タイムアウトの設定があれば、リミットをセットする
						orderContext.get(OrderPrefix.__timeout.toString) match {
							case Some(v) => setTimeout(currentAddedOrderInfo)

							//値がセットされてない
							case None =>
						}

						//非同期	/	同期
						orderContext.contains(OrderPrefix.__delay.toString) match {
							case true => {
								//非同期動作開始の通知
								messenger.callParent(Messages.MESSAGE_ASYNCRONOUSLY_STARTED.toString,
									messenger.tagValues(new TagValue("workerIdentity", identity),
										new TagValue("orderIdentity", orderIdentity)))

								//自分自身を非同期で実行する
								messenger.callMyselfWithAsync(Messages.MESSAGE_EXEC_ASYNC.toString, messenger.tagValues(new TagValue("asyncContextWorkInfo", currentAddedOrderInfo)))
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
					
					case false => //エラーは出力済みなので何も起こらない
				}

			}

			//Timeout
			case Messages.MESSAGE_EXEC_TIMEOUT_READY => {
				val timeoutContextWorkInfo = messenger.get("timeoutContext", tagValues).asInstanceOf[WorkInformation]
				val delay = messenger.get("delay", tagValues).asInstanceOf[Int]

				Thread.sleep(delay)
				/*-----------時間切れ-----------*/

				//結果のコンテキストを作成する
				val timeoutContext = timeoutContextWorkInfo.localContext ++ Map(OrderPrefix._result.toString -> (RUN_TIMEOUT_MESSAGE + WHITESPACE + delay + "msec elapsed"))

				//結果を残す
				new WorkInformation(timeoutContextWorkInfo.orderIdentity, timeoutContext) +=: currentWorkInformationHistory

				//timeoutの見なし　既存workの情報送信を行う
				messenger.callParentWithAsync(Messages.MESSAGE_TIMEOUT.toString, messenger.tagValues(
					new TagValue("timeoutedWorkerIdentity", identity),
					new TagValue("timeoutedOrderIdentity", timeoutContextWorkInfo.orderIdentity),
					new TagValue("timeoutedOrderContext", timeoutContextWorkInfo.localContext)))

				currentStatus = WorkerStatus.STATUS_TIMEOUT
			}

			//非同期実行
			case Messages.MESSAGE_EXEC_ASYNC => {
				val asyncContextWorkInfo = messenger.get("asyncContextWorkInfo", tagValues).asInstanceOf[WorkInformation]
				asyncContextWorkInfo.localContext.get(OrderPrefix.__delay.toString) match {
					case Some(v) => {
						try {
							val delay = v.toInt

							try {
								Thread.sleep(delay)
							} catch { //sleepに対するException -など
								case e : Exception => errorProc(asyncContextWorkInfo, e.toString)
							}

							/*-----------一定時間後実行-----------*/

							run(asyncContextWorkInfo)
						} catch {
							case e : Exception => errorProc(asyncContextWorkInfo, e.toString)

							case _ =>
						}
					}
					case _ =>
				}
			}

			//終了命令
			case Messages.MESSAGE_OVER => {
				//messengerを閉じる
				messenger.close
			}

			case other => {
				println("未整理の何か	" + other)
			}
		}
	}

	/**
	 * must条件のキーの存在確認
	 */
	def validateKeys(context : WorkInformation) = {
		(context.localContext.isDefinedAt(OrderPrefix._kind.toString),
			context.localContext.isDefinedAt(OrderPrefix._main.toString)) match {
				case (true, true) => true //validated.
				case (false, true) => {
					errorProc(context, "no" + WHITESPACE + OrderPrefix._kind + WHITESPACE + "key found in " + context)
					false
				}
				case (true, false) => {
					errorProc(context, "no" + WHITESPACE + OrderPrefix._main + WHITESPACE + "key found in " + context)
					false
				}
				case _ => {
					errorProc(context, "no" + WHITESPACE + OrderPrefix._kind + WHITESPACE + OrderPrefix._main + WHITESPACE + "keys found in " + context)
					false
				}
			}
	}

	/**
	 * timeout Contextのセット
	 */
	def setTimeout(timeoutContext : WorkInformation) = {
		timeoutContext.localContext.get(OrderPrefix.__timeout.toString) match {
			case Some(v) => {
				try {
					val delay = v.toInt

					messenger.callMyselfWithAsync(Messages.MESSAGE_EXEC_TIMEOUT_READY.toString,
						messenger.tagValues(
							new TagValue("timeoutContext", timeoutContext),
							new TagValue("delay", delay)))
				} catch {
					case e : Exception => errorProc(timeoutContext, e.toString)

					case _ =>
				}
			}
			case _ =>
		}

	}

	/**
	 * 実行
	 */
	def run(info : WorkInformation) = {
		//実行前エラーの切り分け
		currentStatus match {
			case WorkerStatus.STATUS_READY => {
				//開始
				currentStatus = WorkerStatus.STATUS_DOING

				//予約語以外
				val currentKeys = OrderPrefix.getXORWithKeyword(info.localContext.keys.toSet)
				val kindKey = info.localContext(OrderPrefix._kind.toString)
				WorkKinds.get(kindKey) match {

					/*
					 * jar実行
					 */
					case WorkKinds.jar => {
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
					case WorkKinds.sh => {
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

					//未知の_kind
					case _ => {
						errorProc(info, OrderPrefix._kind.toString + WHITESPACE + ":" + WHITESPACE + kindKey + WHITESPACE + "is not defined yet. please use " + WorkKinds.values)
					}
				}
			}
			case _ =>
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

			//実行後
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

					//親に、次のOrderをリクエスト
					messenger.callParentWithAsync(Messages.MESSAGE_REQUEST.toString,
						messenger.tagValues(
							new TagValue("workerIdentity", identity),
							new TagValue("finishedOrderIdentity", info.orderIdentity)))

					currentStatus = WorkerStatus.STATUS_DONE
				}
				case _ => println("エラーなど、DOING以外")
			}
		} catch {
			//実行後エラー
			case e : Exception => errorProc(info, e.toString)
			case other => println("unknown error on " + this)
		}
	}

	/**
	 * Error時の統一的な処理
	 */
	def errorProc(info : WorkInformation, eStr : String) = {
		//error結果のコンテキストを作成する
		val errorContext = info.localContext ++ Map(OrderPrefix._result.toString -> eStr)

		//結果を残す
		new WorkInformation(info.orderIdentity, errorContext) +=: currentWorkInformationHistory

		//親に終了を通知
		messenger.callParent(Messages.MESSAGE_ERROR.toString,
			messenger.tagValues(
				new TagValue("erroredWorkerIdentity", identity),
				new TagValue("erroredOrderIdentity", info.orderIdentity),
				new TagValue("eventualContext", errorContext)))

		currentStatus = WorkerStatus.STATUS_ERROR
	}
}