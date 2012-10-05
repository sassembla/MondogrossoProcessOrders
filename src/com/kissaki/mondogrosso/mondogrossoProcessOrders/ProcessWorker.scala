package com.kissaki.mondogrosso.mondogrossoProcessOrders

import scala.collection.mutable.ListBuffer
import scala.sys.process.{ Process ⇒ scalaProcess }
import com.kissaki.Messenger
import com.kissaki.TagValue
import com.kissaki.MessengerProtocol
import com.kissaki.Messenger
import java.util.UUID
import java.util.Timer
import java.util.TimerTask
import java.util.concurrent.TimeUnit

case class WorkInformation(orderIdentity : String, localContext : scala.collection.Map[String, String], currentAfterWaitIds:List[String])

/**
 * ワーカー
 * Messagingで親(Context)との通信を行う
 */
class ProcessWorker(identity : String, masterName : String) extends MessengerProtocol {
	val messenger = new Messenger(this, identity)
	messenger.inputParent(masterName)

	//このworkerのidentity
	val workerIdentity = identity

	//実行中のWork情報履歴
	val currentWorkInformationHistory : ListBuffer[WorkInformation] = ListBuffer()

	//process自体の待機
	val processSplitWaitOrderIdentitiesList : ListBuffer[String] = ListBuffer()
	
	//最新の記録
	def getLatestWorkInformation = currentWorkInformationHistory.head

	//状態
	val currentStatus : ListBuffer[WorkerStatus.Value] = ListBuffer(WorkerStatus.STATUS_EMPTY)

	/**
	 * レシーバ
	 */
	def receiver(execSrc : String, tagValues : Array[TagValue]) : Unit = {

		currentStatus.head match {

			/**
			 * 開始が来るのを待っている、初期化済み状態
			 */
			case WorkerStatus.STATUS_EMPTY => {
				Messages.get(execSrc) match {
					case Messages.MESSAGE_START => procStart(tagValues)
					
					case other => print("STATUS_EMPTY	other	"+other)
				}
			}
			
			/**
			 * 準備状態、同期/非同期実行　or 待ち　を行う。
			 */
			case WorkerStatus.STATUS_READY => {
				Messages.get(execSrc) match {
					case Messages.MESSAGE_EXEC_READY_TIMEOUT => procReadyTimeout(tagValues)
					case Messages.MESSAGE_EXEC_READY_RUN => procReadyRun(tagValues)
					case other => print("STATUS_READY	other	"+other)
				}
			}
			
			case WorkerStatus.STATUS_SPLIT_WAIT => {
				Messages.get(execSrc) match {
					//通知が来た
					case Messages.MESSAGE_FINISHEDORDER_NOTIFY => procRestart(tagValues)
					case Messages.MESSAGE_EXEC_READY_RUN => procReadyRun(tagValues)
					case other => print("STATUS_READY	other	"+other)
				}
			

			}

			case WorkerStatus.STATUS_AFTER_WAIT => {
				
			}
			
			case WorkerStatus.STATUS_DOING => {
				Messages.get(execSrc) match {
					case Messages.MESSAGE_EXEC_DONE => procDone(tagValues)
					
					case other => print("STATUS_DOING	other	"+other)
				}
			}

			case WorkerStatus.STATUS_DONE => {
				print("STATUS_DONE	exec	"+execSrc)
			}
			case WorkerStatus.STATUS_ERROR => {
				println("エラーが起こったようです")
			}
			
			case other => println("予想外のstate	other	"+other)
		}

		/*
		 * いつでも発生する可能性のあるイベント
		 */
		Messages.get(execSrc) match {
			
			//非同期動作の設定
			case Messages.MESSAGE_EXEC_ASYNC => procStartAsync(tagValues)
			
			
			//タイムアウトの設定
			case Messages.MESSAGE_EXEC_TIMEOUT_READY => procTimeoutReady(tagValues)

			//終了命令
			case Messages.MESSAGE_OVER => {
				//messengerを閉じる
				messenger.close
			}
			
			case _ => 
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
					errorNotice(context, "no" + OrderPrefix.RUN_PREFIX_WHITESPACE + OrderPrefix._kind + OrderPrefix.RUN_PREFIX_WHITESPACE + "key found in " + context)
					false
				}
				case (true, false) => {
					errorNotice(context, "no" + OrderPrefix.RUN_PREFIX_WHITESPACE + OrderPrefix._main + OrderPrefix.RUN_PREFIX_WHITESPACE + "key found in " + context)
					false
				}
				case _ => {
					errorNotice(context, "no" + OrderPrefix.RUN_PREFIX_WHITESPACE + OrderPrefix._kind + OrderPrefix.RUN_PREFIX_WHITESPACE + OrderPrefix._main + OrderPrefix.RUN_PREFIX_WHITESPACE + "keys found in " + context)
					false
				}
			}
	}

	/**
	 *  EMPTY/READYからの開始
	 */
	def procStart(tagValues : Array[TagValue]) = {
		val orderIdentity = (messenger.get("identity", tagValues)).asInstanceOf[String]
		val processSplitIds = (messenger.get("processSplitIds", tagValues)).asInstanceOf[List[String]]
		val afterWaitIds = (messenger.get("afterWaitIds", tagValues)).asInstanceOf[List[String]]
		val orderContext = (messenger.get("context", tagValues)).asInstanceOf[scala.collection.Map[String, String]]

		Option(messenger.get("identity", tagValues)) match {
			case Some(v) => v.asInstanceOf[String]
			case None =>
		}

		val currentAddedOrderInfo = new WorkInformation(orderIdentity, orderContext)

		//currentWorkInformationHistoryのheadに、最新のInfoを加算する
		currentAddedOrderInfo +=: currentWorkInformationHistory

		//準備状態に入る
		WorkerStatus.STATUS_READY +=: currentStatus
		
		//バリデーション
		validateKeys(currentAddedOrderInfo)
		
		//afterWaitIdsが有る場合、マークする
		currentAfterWaitIds += afterWaitIds
		
		//splitでの分岐
		processSplitIds.size match {
			case 0 => {
				messenger.callMyself(Messages.MESSAGE_EXEC_READY_TIMEOUT.toString, messenger.tagValues(
						new TagValue("orderIdentity", orderIdentity),
						new TagValue("orderContext", orderContext),
						new TagValue("currentAddedOrderInfo", currentAddedOrderInfo)))
			}
			case _ => { //processSplitの待ちIdentityが存在する
				WorkerStatus.STATUS_SPLIT_WAIT +=: currentStatus
				
				//idを追加する
				processSplitIds.foreach {id => processSplitWaitOrderIdentitiesList += id}
			}
		}
	}
	
	/**
	 * WAITで行う動作
	 */
	def procRestart (tagValues : Array[TagValue]) = {
		val finishedOrderIdentity = messenger.get("finishedOrderIdentity", tagValues).asInstanceOf[String]
		processSplitWaitOrderIdentitiesList -= finishedOrderIdentity
		
		//ProcessSplitが空になった
		if (processSplitWaitOrderIdentitiesList.isEmpty) {
			
			val currentAddedOrderInfo = currentWorkInformationHistory.head
			val orderIdentity = currentAddedOrderInfo.orderIdentity
			val orderContext = currentAddedOrderInfo.localContext
			
			//WAITからのRUNを行う
			messenger.callMyself(Messages.MESSAGE_EXEC_READY_RUN.toString, messenger.tagValues(
						new TagValue("orderIdentity", orderIdentity),
						new TagValue("orderContext", orderContext),
						new TagValue("currentAddedOrderInfo", currentAddedOrderInfo)))
		}
	}
	
	/**
	 * READYで行う動作１
	 */
	def procReadyTimeout (tagValues : Array[TagValue]) = {
		val orderIdentity = messenger.get("orderIdentity", tagValues).asInstanceOf[String]
		val orderContext = (messenger.get("orderContext", tagValues)).asInstanceOf[scala.collection.Map[String, String]]
		val currentAddedOrderInfo = messenger.get("currentAddedOrderInfo", tagValues).asInstanceOf[WorkInformation]
		
		//タイムアウトの設定があれば、リミットをセットする
		orderContext.get(OrderPrefix.__timeout.toString) match {
			case Some(v) => setTimeout(currentAddedOrderInfo)

			//値がセットされてない
			case None => println("タイムリミットのセットが無い")
		}
		
		//セットが終わったのでRUNを実行
		messenger.callMyself(Messages.MESSAGE_EXEC_READY_RUN.toString, tagValues)
	}
	
	/**
	 * READYで行う動作2
	 * TIMEOUT後
	 */
	def procReadyRun (tagValues : Array[TagValue]) = {
		val orderIdentity = messenger.get("orderIdentity", tagValues).asInstanceOf[String]
		val orderContext = (messenger.get("orderContext", tagValues)).asInstanceOf[scala.collection.Map[String, String]]
		val currentAddedOrderInfo = messenger.get("currentAddedOrderInfo", tagValues).asInstanceOf[WorkInformation]
		
		
		/*----------実行----------*/
		
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
				doWork(currentAddedOrderInfo)
			}
		}
	}

	/**
	 * READYからの非同期の開始
	 */
	def procStartAsync(tagValues : Array[TagValue]) = {
		val asyncContextWorkInfo = messenger.get("asyncContextWorkInfo", tagValues).asInstanceOf[WorkInformation]
		asyncContextWorkInfo.localContext.get(OrderPrefix.__delay.toString) match {
			case Some(v) => {
				try {
					val delay = v.toInt

					try {
						val timer = new Timer("work	" + this);
						timer.schedule(new TimerTask {
							def run = {
								doWork(asyncContextWorkInfo)
							}
						}, TimeUnit.MILLISECONDS.toMillis(delay));

					} catch { //timerに対するException -など
						case e : Exception => errorNotice(asyncContextWorkInfo, e.toString)
					}
				} catch {
					case e : Exception => errorNotice(asyncContextWorkInfo, e.toString)
					case _ =>
				}
			}
			case _ =>
		}
	}

	/**
	 * READYでのTimeoutのセット
	 */
	def procTimeoutReady(tagValues : Array[TagValue]) = {
		val timeoutContextWorkInfo = messenger.get("timeoutContext", tagValues).asInstanceOf[WorkInformation]
		val delay = messenger.get("delay", tagValues).asInstanceOf[Int]

		val timer = new Timer("testing" + this);
		timer.schedule(new TimerTask {
			def run = {
				/*-----------時間切れ-----------*/

				//結果のコンテキストを作成する
				val timeoutContext = timeoutContextWorkInfo.localContext ++ Map(OrderPrefix._result.toString -> (OrderPrefix.RUN_TIMEOUT_MESSAGE + OrderPrefix.RUN_PREFIX_WHITESPACE + delay + "msec elapsed"))

				//結果を残す
				new WorkInformation(timeoutContextWorkInfo.orderIdentity, timeoutContext) +=: currentWorkInformationHistory

				//timeoutの見なし　既存workの情報送信を行う
				messenger.callParentWithAsync(Messages.MESSAGE_TIMEOUT.toString, messenger.tagValues(
					new TagValue("timeoutedWorkerIdentity", identity),
					new TagValue("timeoutedOrderIdentity", timeoutContextWorkInfo.orderIdentity),
					new TagValue("timeoutedOrderContext", timeoutContextWorkInfo.localContext)))

				WorkerStatus.STATUS_TIMEOUT +=: currentStatus
			}
		}, TimeUnit.MILLISECONDS.toMillis(delay));
	}

	
	
	/**
	 * DOING中の完了
	 */
	def procDone(tagValues : Array[TagValue]) = {
		val result = messenger.get("result", tagValues).asInstanceOf[String]
		val info = messenger.get("info", tagValues).asInstanceOf[WorkInformation]
		val process = messenger.get("process", tagValues).asInstanceOf[scala.sys.process.ProcessBuilder]

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
		println("from worker, process is	" + identity + " make request! to parent " + messenger.getParentName)
		//親に、次のOrderをリクエスト
		messenger.callParentWithAsync(Messages.MESSAGE_REQUEST.toString,
			messenger.tagValues(
				new TagValue("workerIdentity", identity),
				new TagValue("finishedOrderIdentity", info.orderIdentity)))

		WorkerStatus.STATUS_DONE +=: currentStatus
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
					case e : Exception => errorNotice(timeoutContext, e.toString)
					case _ =>
				}
			}
			case _ =>
		}
	}

	/**
	 * READYからの実行
	 */
	def doWork(info : WorkInformation) = {
		//開始
		WorkerStatus.STATUS_DOING +=: currentStatus

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
					key + OrderPrefix.RUN_PREFIX_WHITESPACE + value
				}

				val reduced = inputKeyValuePair match {
					case v if (v.isEmpty) => ""
					case default => {
						inputKeyValuePair.reduceLeft {
							(total, add) => total + OrderPrefix.RUN_PREFIX_WHITESPACE + add
						}
					}
				}

				val stableCommand = OrderPrefix.RUN_PREFIX_JAVA + OrderPrefix.RUN_PREFIX_WHITESPACE +
					OrderPrefix.RUN_PREFIX_JAR + OrderPrefix.RUN_PREFIX_WHITESPACE +
					OrderPrefix.RUN_PREFIX_CURRENTDIR + info.localContext(OrderPrefix._main.toString) +
					OrderPrefix.RUN_PREFIX_DOTJAR + OrderPrefix.RUN_PREFIX_WHITESPACE +
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
					key + OrderPrefix.RUN_PREFIX_WHITESPACE + value
				}

				val reduced = inputKeyValuePair match {
					case v if (v.isEmpty) => ""

					case default => {
						inputKeyValuePair.reduceLeft {
							(total, add) => total + OrderPrefix.RUN_PREFIX_WHITESPACE + add
						}
					}
				}

				val stableCommand = info.localContext(OrderPrefix._main.toString) + OrderPrefix.RUN_PREFIX_WHITESPACE + reduced

				//						println("shell stableCommand	" + stableCommand)

				//プロセス生成
				val process = scalaProcess(stableCommand)

				//実行
				runProcess(info, process)
			}

			//未知の_kind
			case _ => {
				errorNotice(info, OrderPrefix._kind.toString + OrderPrefix.RUN_PREFIX_WHITESPACE + ":" + OrderPrefix.RUN_PREFIX_WHITESPACE + kindKey + OrderPrefix.RUN_PREFIX_WHITESPACE + "is not defined yet. please use " + WorkKinds.values)
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
			messenger.callMyself(Messages.MESSAGE_EXEC_DONE.toString,
				messenger.tagValues(
					new TagValue("result", result),
					new TagValue("info", info),
					new TagValue("process", process)))
		} catch {
			//実行後エラー
			case e : Exception => errorNotice(info, e.toString)
			case other => println("unknown error on " + this)
		}
	}

	
	/**
	 * Error時の統一的な処理
	 */
	def errorNotice(info : WorkInformation, eStr : String) = {
		WorkerStatus.STATUS_ERROR +=: currentStatus
		
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

		
	}
}