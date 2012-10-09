package com.kissaki.mondogrosso.mondogrossoProcessOrders

import scala.collection.mutable.ListBuffer
import com.kissaki.Messenger
import com.kissaki.TagValue
import com.kissaki.MessengerProtocol
import com.kissaki.Messenger
import java.util.UUID
import java.util.Timer
import java.util.TimerTask
import java.util.concurrent.TimeUnit
import  scala.sys.process.stringSeqToProcess
import scala.sys.process.ProcessLogger


case class WorkInformation(orderIdentity : String, localContext : scala.collection.Map[String, String], afterWaitIds : List[String])

/**
 * ワーカー
 * Messagingで親(Context)との通信を行う
 */
class ProcessWorker(identity : String, masterName : String) extends MessengerProtocol {
	println("ProcessWorker	identity	"+identity + "	/masterName	"+masterName)
	val messenger = new Messenger(this, identity)
	messenger.inputParent(masterName)

	//このworkerのidentity
	val workerIdentity = identity

	//実行中のWork情報履歴
	val currentWorkInformationHistory : ListBuffer[WorkInformation] = ListBuffer()

	//process自体の待機
	val processSplitWaitOrderIdentitiesList : ListBuffer[String] = ListBuffer()

	//このprocessがMasterから受け取った、既存の終了したOrderIdentity一覧
	val currentFinishedOrdersList : ListBuffer[String] = ListBuffer()

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
			 * 開始が来るのを待っている、初期化待ち状態
			 */
			case WorkerStatus.STATUS_EMPTY => {
				WorkerExecs.get(execSrc) match {
					case WorkerExecs.EXEC_SETUP => procSetUp(tagValues)
					case other =>
				}
			}	
			
			/**
			 * 初期化済みからの開始
			 */
			case WorkerStatus.STATUS_SETUP => {
				Messages.get(execSrc) match {
					case Messages.MESSAGE_START => procStart(tagValues)
					case other =>
				}
			}

			/**
			 * 準備状態、同期/非同期実行　or 待ち　を行う。
			 */
			case WorkerStatus.STATUS_READY => {
				WorkerExecs.get(execSrc) match {
					case WorkerExecs.EXEC_READY_TIMEOUT => procReadyTimeout(tagValues)
					case WorkerExecs.EXEC_READY_RUN => procReadyRun(tagValues)
					case other => 
				}
			}

			case WorkerStatus.STATUS_SPLIT_WAIT => {
				Messages.get(execSrc) match {
					//processSplitを解除する可能性がある、終了ORDERの通知
					case Messages.MESSAGE_FINISHEDORDER_NOTIFY => procRestart(tagValues)
					case other =>
				}
				WorkerExecs.get(execSrc) match {
					case WorkerExecs.EXEC_READY_RUN => procReadyRun(tagValues)
					case other =>
				}
			}

			case WorkerStatus.STATUS_AFTER_WAIT => {
				WorkerExecs.get(execSrc) match {
					case WorkerExecs.EXEC_UNLOCK_AFTERWAIT => procUnlock(tagValues)
					case other => 
				}
			}

			case WorkerStatus.STATUS_DOING => {
				
				WorkerExecs.get(execSrc) match {
					case WorkerExecs.EXEC_DONE => procDone(tagValues)
					case other => 
				}
			}

			case WorkerStatus.STATUS_DONE => {
				WorkerExecs.get(execSrc) match {
					case WorkerExecs.EXEC_SETUP => procSetUp(tagValues)
					case other => 
				}
			}
			
			case WorkerStatus.STATUS_ERROR => {
				println("エラーが起こったようです")
			}

			case other => println("予想外のstate	other	" + other)
		}

		/*
		 * いつでも発生する可能性のある外部からのイベント
		 */
		Messages.get(execSrc) match {
			case Messages.MESSAGE_SETUP => messenger.callMyself(WorkerExecs.EXEC_SETUP.toString, tagValues)
			
//			//processSplitを解除する可能性がある、終了ORDERの通知
//			case Messages.MESSAGE_FINISHEDORDER_NOTIFY => procRestart(tagValues)
//			
			//afterWaitを解除する可能性がある、終了ORDERの通知
			case Messages.MESSAGE_FINISHEDORDER_NOTIFY => unlock_AfterWait(tagValues)

			//終了命令
			case Messages.MESSAGE_OVER => messenger.close

			case _ =>
		}
		
		/*
		 * いつでも発生する可能性のある処理
		 */
		WorkerExecs.get(execSrc) match {
			//非同期動作の設定
			case WorkerExecs.EXEC_ASYNC => procStartAsync(tagValues)

			//タイムアウトの設定
			case WorkerExecs.EXEC_TIMEOUT_READY => procTimeoutReady(tagValues)
			
			case _ =>
		}
	}

	/**
	 * finishedIdsに登録、現在afterWaitしているものがいたら、内容チェックを行い再開させる
	 */
	def unlock_AfterWait(tagValues : Array[TagValue]) = {
		//リストを空に
		currentFinishedOrdersList.clear()

		val allfinishedOrderIdentities = messenger.get("allfinishedOrderIdentities", tagValues).asInstanceOf[List[String]]

		//currentFinishedOrdersListに既存の完了済みOrderIdを移し替える
		allfinishedOrderIdentities.foreach(id => currentFinishedOrdersList += id)

		messenger.callMyself(WorkerExecs.EXEC_UNLOCK_AFTERWAIT.toString, tagValues)
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
	 *  EMPTY/READYからの準備
	 */
	def procSetUp(tagValues : Array[TagValue]) = {
		val orderIdentity = (messenger.get("identity", tagValues)).asInstanceOf[String]
		val processSplitIds = (messenger.get("processSplitIds", tagValues)).asInstanceOf[List[OrderIdentity]]
		val afterWaitIds = (messenger.get("afterWaitIds", tagValues)).asInstanceOf[List[String]]
		val orderContext = (messenger.get("context", tagValues)).asInstanceOf[scala.collection.Map[String, String]]

		Option(messenger.get("identity", tagValues)) match {
			case Some(v) => v.asInstanceOf[String]
			case None =>
		}

		val currentAddedOrderInfo = new WorkInformation(orderIdentity, orderContext, afterWaitIds)

		//currentWorkInformationHistoryのheadに、最新のInfoを加算する
		currentAddedOrderInfo +=: currentWorkInformationHistory

		//バリデーション
		if (validateKeys(currentAddedOrderInfo)) {
	
			//splitでの分岐
			processSplitIds.size match {
				case 0 => {
					WorkerStatus.STATUS_SETUP +=: currentStatus
				}
				case _ => { //processSplitの待ちIdentityが存在する
					WorkerStatus.STATUS_SPLIT_WAIT +=: currentStatus
					
					//idを追加する
					processSplitIds.foreach { id =>
						processSplitWaitOrderIdentitiesList += id.myId 
					}
				}
			}
		}		
		//実行は次の命令を待ってから
	}

	/**
	 * 実行
	 */
	def procStart (tagValues : Array[TagValue]) = {
		val orderIdentity = (messenger.get("identity", tagValues)).asInstanceOf[String]
		val processSplitIds = (messenger.get("processSplitIds", tagValues)).asInstanceOf[List[OrderIdentity]]
		val afterWaitIds = (messenger.get("afterWaitIds", tagValues)).asInstanceOf[List[String]]
		val orderContext = (messenger.get("context", tagValues)).asInstanceOf[scala.collection.Map[String, String]]

		val currentAddedOrderInfo = new WorkInformation(orderIdentity, orderContext, afterWaitIds)
		
		WorkerStatus.STATUS_READY +=: currentStatus
		
		messenger.callMyself(WorkerExecs.EXEC_READY_TIMEOUT.toString, messenger.tagValues(
			new TagValue("orderIdentity", orderIdentity),
			new TagValue("orderContext", orderContext),
			new TagValue("currentAddedOrderInfo", currentAddedOrderInfo)))
	}
	
	/**
	 * SPLIT_WAITで行う動作
	 */
	def procRestart(tagValues : Array[TagValue]) = {
		val finishedOrderIdentity = messenger.get("finishedOrderIdentity", tagValues).asInstanceOf[String]
		processSplitWaitOrderIdentitiesList -= finishedOrderIdentity
		
		//ProcessSplitが空になった
		if (processSplitWaitOrderIdentitiesList.isEmpty) {
			println("のこりzero ラストは	finishedOrderIdentity	"+finishedOrderIdentity)
			val currentAddedOrderInfo = currentWorkInformationHistory.head
			val orderIdentity = currentAddedOrderInfo.orderIdentity
			val orderContext = currentAddedOrderInfo.localContext

			//WAITからのRUNを行う
			messenger.callMyself(WorkerExecs.EXEC_READY_RUN.toString, messenger.tagValues(
				new TagValue("orderIdentity", orderIdentity),
				new TagValue("orderContext", orderContext),
				new TagValue("currentAddedOrderInfo", currentAddedOrderInfo)))
		} else {
			println("まだある	finishedOrderIdentity	"+processSplitWaitOrderIdentitiesList)
		}
	}

	/**
	 * READYで行う動作
	 * タイムアウトの設定
	 */
	def procReadyTimeout(tagValues : Array[TagValue]) = {
		println("procReadyTimeout ide	"+identity)
		val orderIdentity = messenger.get("orderIdentity", tagValues).asInstanceOf[String]
		val orderContext = (messenger.get("orderContext", tagValues)).asInstanceOf[scala.collection.Map[String, String]]
		val currentAddedOrderInfo = messenger.get("currentAddedOrderInfo", tagValues).asInstanceOf[WorkInformation]

		//タイムアウトの設定があれば、リミットをセットする
		orderContext.get(OrderPrefix.__timeout.toString) match {
			case Some(v) => setTimeout(currentAddedOrderInfo)

			//値がセットされてない
			case None => //println("タイムリミットのセットが無い")
		}

		//セットが終わったのでRUNを実行
		messenger.callMyself(WorkerExecs.EXEC_READY_RUN.toString, tagValues)
	}

	/**
	 * READYで行う動作
	 * TIMEOUT後
	 */
	def procReadyRun(tagValues : Array[TagValue]) = {
		println("procReadyRun	identity	"+identity)
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
				messenger.callMyselfWithAsync(WorkerExecs.EXEC_ASYNC.toString, messenger.tagValues(new TagValue("asyncContextWorkInfo", currentAddedOrderInfo)))
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

		val timer = new Timer("Timeout	" + this);
		timer.schedule(new TimerTask {
			def run = {
				/*-----------時間切れ-----------*/

				//結果のコンテキストを作成する
				val timeoutContext = timeoutContextWorkInfo.localContext ++ Map(OrderPrefix._result.toString -> (OrderPrefix.RUN_TIMEOUT_MESSAGE + OrderPrefix.RUN_PREFIX_WHITESPACE + delay + "msec elapsed"))

				//結果を残す
				new WorkInformation(timeoutContextWorkInfo.orderIdentity, timeoutContext, timeoutContextWorkInfo.afterWaitIds) +=: currentWorkInformationHistory

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
	 * AFTERWAIT中の処理
	 */
	def procUnlock(tagValues : Array[TagValue]) = tryUnlockAfterWait(currentWorkInformationHistory.head)

	/**
	 * DOING中の完了
	 */
	def procDone(tagValues : Array[TagValue]) = {
		val result = messenger.get("result", tagValues).asInstanceOf[String]
		val info = messenger.get("info", tagValues).asInstanceOf[WorkInformation]

		//結果のコンテキストを作成する
		val eventualContext = info.localContext ++ Map(OrderPrefix._result.toString -> result)

		//結果を残す
		new WorkInformation(info.orderIdentity, eventualContext, info.afterWaitIds) +=: currentWorkInformationHistory

		//親に終了を通知
		messenger.callParent(Messages.MESSAGE_DONE.toString,
			messenger.tagValues(
				new TagValue("identity", identity),
				new TagValue("orderIdentity", info.orderIdentity),
				new TagValue("eventualContext", eventualContext)))
		
		//この時点で既にafterWaitを突破できる可能性があるので、チェック
		tryUnlockAfterWait(info)
	}

	/**
	 * currentFinishedOrdersListに含まれていれば、afterWaitを解除する
	 */
	def tryUnlockAfterWait(info : WorkInformation) {
		currentWorkInformationHistory.head.afterWaitIds.size match {
			//afterWait無し
			case 0 => {
				messenger.callParentWithAsync(Messages.MESSAGE_REQUEST.toString,
					messenger.tagValues(
						new TagValue("workerIdentity", identity),
						new TagValue("finishedOrderIdentity", info.orderIdentity)))

				WorkerStatus.STATUS_DONE +=: currentStatus
			}
			//afterWaitが存在している
			case many => {
				val currentAfterWaits = currentWorkInformationHistory.head.afterWaitIds

				//包含を調べる
				val containBaseResults = for (waitId <- currentAfterWaits) yield currentFinishedOrdersList.contains(waitId)

				val result = containBaseResults.reduceLeft { (all, toAdd) =>
					(all, toAdd) match {
						case (true, true) => true
						case _ => false
					}
				}
				
				result match {
					//ロックが存在しない
					case true => {
//						println("リクエストする。　currentFinishedOrdersList	" + currentFinishedOrdersList.toList + "	に対して、アンロック条件が既に満たされている	" + currentWorkInformationHistory.head.afterWaitIds)
						//親に、次のOrderをリクエスト
						messenger.callParentWithAsync(Messages.MESSAGE_REQUEST.toString,
							messenger.tagValues(
								new TagValue("workerIdentity", identity),
								new TagValue("finishedOrderIdentity", info.orderIdentity)))

						WorkerStatus.STATUS_DONE +=: currentStatus
					}

					case false => {
//						println("currentFinishedOrdersList	" + currentFinishedOrdersList.toList + "	に対して、ロックがまだある	" + currentWorkInformationHistory.head.afterWaitIds)
						WorkerStatus.STATUS_AFTER_WAIT +=: currentStatus
					}
				}
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
					messenger.callMyselfWithAsync(WorkerExecs.EXEC_TIMEOUT_READY.toString,
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
		
		//対応する種類に関して、実行を行う
		WorkKinds.get(kindKey) match {

			/*
			 * jar実行
			 */
			case WorkKinds.jar => {
				//キーと値をシークエンスに纏める
				val inputKeyValuePairs = for (key <- currentKeys) yield {
					val value = info.localContext.get(key) match {
						case Some(v) => v
						case None => ""
					}
					Seq(key, value)
				}
				val inputKeyValueSeq = if (inputKeyValuePairs.isEmpty) {
					Seq()
				} else {
					inputKeyValuePairs.reduce {
						(total, toAdd) => total ++ toAdd
					}
				}
				
				val stableCommand = Seq(OrderPrefix.RUN_PREFIX_JAVA,
					OrderPrefix.RUN_PREFIX_JAR,
					OrderPrefix.RUN_PREFIX_CURRENTDIR + info.localContext(OrderPrefix._main.toString) + OrderPrefix.RUN_PREFIX_DOTJAR) ++ inputKeyValueSeq
					
				println("stableCommand	"+stableCommand)
				//実行
				runProcess(info, stableCommand)
			}

			/*
			 * process実行
			 */
			case WorkKinds.sh => {
				//キーと値をシークエンスに纏める
				val inputKeyValuePairs = for (key <- currentKeys) yield {
					val value = info.localContext.get(key) match {
						case Some(v) => v
						case None => ""
					}
					Seq(key, value)
				}
				
				val inputKeyValueSeq = if (inputKeyValuePairs.isEmpty) {
					Seq()
				} else {
					inputKeyValuePairs.reduce {
						(total, toAdd) => total ++ toAdd
					}
				}
				val stableCommand = info.localContext(OrderPrefix._main.toString).split(OrderPrefix.RUN_PREFIX_WHITESPACE).toSeq ++ inputKeyValueSeq
				
				//実行
				runProcess(info, stableCommand)
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
	def runProcess(info : WorkInformation, processSrc : Seq[String]) = {
		val out = new StringBuilder
		val err = new StringBuilder

		try {
			println("processSrc	"+processSrc)	
			val logger = ProcessLogger(
			    (o: String) => {
			    	if (out.isEmpty) out.append(o)
			    	else out.append("\n"+o)
			    },
			    (e: String) => {
			    	if (err.isEmpty) err.append(e)
			    	else err.append("\n"+e)
			    }
			)
			    
			val runResult = processSrc ! logger
			
			val result = out.toString
			println("result	"+result+"	/out	"+out.toString+"	/err	"+err)
			
			messenger.callMyself(WorkerExecs.EXEC_DONE.toString,
				messenger.tagValues(
					new TagValue("result", result),
					new TagValue("info", info)))
		} catch {
			//実行後エラー
			
			case e : Exception => {
				println("e	"+e)
				errorNotice(info, e.toString)
			}
			case other => println("unknown error on " + this)
		}
	}

	/**
	 * Error時の統一的な処理
	 */
	def errorNotice(info : WorkInformation, eStr : String) = {
		println("エラーが出た	"+eStr	+"	/info	"+info)
		WorkerStatus.STATUS_ERROR +=: currentStatus

		//error結果のコンテキストを作成する
		val errorContext = info.localContext ++ Map(OrderPrefix._result.toString -> eStr)

		//結果を残す
		new WorkInformation(info.orderIdentity, errorContext, info.afterWaitIds) +=: currentWorkInformationHistory

		//親に終了を通知
		messenger.callParent(Messages.MESSAGE_ERROR.toString,
			messenger.tagValues(
				new TagValue("erroredWorkerIdentity", identity),
				new TagValue("erroredOrderIdentity", info.orderIdentity),
				new TagValue("eventualContext", errorContext)))

	}
}