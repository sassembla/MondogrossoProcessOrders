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
//	println("ProcessWorker	identity	"+identity + "	/masterName	"+masterName)
	val messenger = new Messenger(this, identity)
	messenger.inputParent(masterName)

	//このworkerのidentity
	val workerIdentity = identity

	//実行中のWork情報履歴
	val currentWorkInformationHistory : ListBuffer[WorkInformation] = ListBuffer()

	
	//process自体の待機を行うキー
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

			/*
			 * 開始が来るのを待っている、初期化待ち状態
			 */
			case WorkerStatus.STATUS_EMPTY => 
			
			/*
			 * 初期化済みからの開始
			 */
			case WorkerStatus.STATUS_SETUP => {
				WorkerMessages.get(execSrc) match {
					case WorkerMessages.MESSAGE_START => procStart(tagValues)
					case other =>
				}
			}

			/*
			 * 準備状態、同期/非同期実行　or 待ち　を行う。
			 */
			case WorkerStatus.STATUS_READY => 

			/*
			 * 他Orderの完了待ちからのProcess開始待ち状態
			 */
			case WorkerStatus.STATUS_SPLIT_WAIT => 
			
			/*
			 * 他Orderの完了待ちからのProcess開始状態
			 */
			case WorkerStatus.STATUS_SPLIT_READY => 
			
			/**
			 * 完了後のロック状態
			 */
			case WorkerStatus.STATUS_AFTER_WAIT => 


			case WorkerStatus.STATUS_DOING => {				
				WorkerExecs.get(execSrc) match {
					case WorkerExecs.EXEC_IGNITION => procIgnite(tagValues)
					case WorkerExecs.EXEC_DONE => procDone(tagValues)
					case other => 
				}
			}

			case WorkerStatus.STATUS_REQUESTING => 
			
			case WorkerStatus.STATUS_ERROR => {
				println("エラーが起こったようです	"+identity)
			}

			case WorkerStatus.STATUS_TIMEOUT => {
				println("タイムアウト発生	"+identity)
			}

			case other => println("予想外のstate	other	" + other)
		}

		/*
		 * いつでも発生する可能性のある外部からのイベント
		 */
		WorkerMessages.get(execSrc) match {

			case WorkerMessages.MESSAGE_SETUP => {
				currentStatus.head match {
					case WorkerStatus.STATUS_EMPTY => procSetUp(tagValues)
					case WorkerStatus.STATUS_REQUESTING => procSetUp(tagValues)
					case _ => 
				}
			}
			case WorkerMessages.MESSAGE_SETUP_AND_START => {
				println("連続したOrderを待ってるWorkerか、finallyProcessIdentity が、受け取っているはず、、、"+identity)
				currentStatus.head match {
					case WorkerStatus.STATUS_SPLIT_READY => procSetupAndStart(tagValues)
					case WorkerStatus.STATUS_REQUESTING => procSetupAndStart(tagValues)
					case _ => 
				}
			}


			//processSplitWait,afterWaitを解除する可能性がある、終了ORDERの通知
			case WorkerMessages.MESSAGE_FINISHEDORDER_NOTIFY => {
				val allfinishedOrderIdentities = messenger.get("allfinishedOrderIdentities", tagValues).asInstanceOf[List[String]]
				println("workerIdentity "+ identity + "	/が、MESSAGE_FINISHEDORDER_NOTIFYを受け取った	ここまでの完了Orderは	"+allfinishedOrderIdentities)

				//リストを更新
				currentFinishedOrdersList.clear
				allfinishedOrderIdentities.foreach(id => currentFinishedOrdersList += id)
				currentFinishedOrdersList

				currentStatus.head match {
					case WorkerStatus.STATUS_SPLIT_WAIT => {
						println("STATUS_SPLIT_WAITに来てる	workerIdentityは"+identity)
						procRestartOrContinueSplitWait(currentFinishedOrdersList.toSet, currentWorkInformationHistory.head.orderIdentity)
					}
					case WorkerStatus.STATUS_AFTER_WAIT => {
						println("STATUS_AFTER_WAITに来てる	workerIdentityは"+identity)
						procRequestOrContinueAfterWait(
							currentFinishedOrdersList.toSet,
							currentWorkInformationHistory.head.afterWaitIds.toSet,
							currentWorkInformationHistory.head.orderIdentity
							)
					}
					case _ => 
				}
			}
			
			//終了命令
			case WorkerMessages.MESSAGE_OVER => messenger.close

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
	 * must条件のキーの存在確認
	 */
	def validateKeys(context : WorkInformation) = {
		val errorString = new StringBuffer("in " + context + OrderPrefix.RUN_PREFIX_WHITESPACE)
		
		val hasMain = if (!context.localContext.isDefinedAt(OrderPrefix._kind.toString)) {
			errorString.append("no" + OrderPrefix.RUN_PREFIX_WHITESPACE + OrderPrefix._kind + OrderPrefix.RUN_PREFIX_WHITESPACE + "key found,")
			false
		} else true
		
		val hasKind = if (!context.localContext.isDefinedAt(OrderPrefix._main.toString)) {
			errorString.append("no" + OrderPrefix.RUN_PREFIX_WHITESPACE + OrderPrefix._main + OrderPrefix.RUN_PREFIX_WHITESPACE + "key found,")
			false
		} else true
		
		val hasCollectTimeoutOrNotContained = if (context.localContext.isDefinedAt(OrderPrefix.__timeout.toString)) {
			val result = try {
				context.localContext.apply(OrderPrefix.__timeout.toString).toInt//定義してあれば、数字が入っているはず
				true
			} catch  {
				case e:Exception => {
					errorString.append(e.toString)
					false
				}
				case other => {
					errorString.append(other.toString)
					false
				}
			}
			result
		} else true
		
		if (hasMain && hasKind && hasCollectTimeoutOrNotContained) true
		else {
			errorNotice(context, errorString.toString)
			false
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
		
		val currentAddedOrderInfo = new WorkInformation(orderIdentity, orderContext, afterWaitIds ++ List(orderIdentity))

		//currentWorkInformationHistoryのheadに、最新のInfoを加算する
		currentAddedOrderInfo +=: currentWorkInformationHistory

		//バリデーション
		if (validateKeys(currentAddedOrderInfo)) {
			if (processSplitIds.isEmpty) 	WorkerStatus.STATUS_SETUP +=: currentStatus
			else 							{
				WorkerStatus.STATUS_SPLIT_WAIT +=: currentStatus
				processSplitWaitOrderIdentitiesList ++ afterWaitIds
			}
		}
	}
	
	
	/**
	 * Split待ちが完了したProcessへの準備を行う
	 */
	def procSetupAndStart(tagValues : Array[TagValue]) = {
		val orderIdentity = (messenger.get("identity", tagValues)).asInstanceOf[String]
		val processSplitIds = (messenger.get("processSplitIds", tagValues)).asInstanceOf[List[OrderIdentity]]
		val afterWaitIds = (messenger.get("afterWaitIds", tagValues)).asInstanceOf[List[String]]
		val orderContext = (messenger.get("context", tagValues)).asInstanceOf[scala.collection.Map[String, String]]
		
		val currentAddedOrderInfo = new WorkInformation(orderIdentity, orderContext, afterWaitIds ++ List(orderIdentity))

		//currentWorkInformationHistoryのheadに、最新のInfoを加算する
		currentAddedOrderInfo +=: currentWorkInformationHistory

		//バリデーション
		if (validateKeys(currentAddedOrderInfo)) {
			WorkerStatus.STATUS_SETUP +=: currentStatus
			
			//即座に実行に入る
			WorkerStatus.STATUS_SETUP +=: currentStatus

			//実行
			procStart(tagValues)
		}
	}

	/**
	 * 実行
	 */
	def procStart (tagValues : Array[TagValue]) = {
		val orderIdentity = (messenger.get("identity", tagValues)).asInstanceOf[String]
		val processSplitIds = (messenger.get("processSplitIds", tagValues)).asInstanceOf[List[OrderIdentity]]
		val afterWaitIds = (messenger.get("afterWaitIds", tagValues)).asInstanceOf[List[String]]
		val orderContext = (messenger.get("context", tagValues)).asInstanceOf[scala.collection.Map[String, String]]

		val currentAddedOrderInfo = new WorkInformation(orderIdentity, orderContext, afterWaitIds ++ List(orderIdentity))
		WorkerStatus.STATUS_READY +=: currentStatus


		//タイムアウトの設定があれば、リミットをセットする
		orderContext.get(OrderPrefix.__timeout.toString) match {
			case Some(v) => setTimeout(currentAddedOrderInfo)

			//値がセットされてない
			case None => //println("タイムリミットのセットが無い")
		}

		/*----------実行----------*/
		
		//非同期	/	同期
		orderContext.contains(OrderPrefix.__delay.toString) match {
			case true => {
				//非同期動作開始の通知
				messenger.callParentWithAsync(WorkerMessages.MESSAGE_ASYNCRONOUSLY_STARTED.toString,
					messenger.tagValues(new TagValue("workerIdentity", identity),
						new TagValue("orderIdentity", orderIdentity)))

				//自分自身を非同期で実行する
				messenger.callMyselfWithAsync(WorkerExecs.EXEC_ASYNC.toString, messenger.tagValues(new TagValue("asyncContextWorkInfo", currentAddedOrderInfo)))
			}
			case false => {
				//動作開始の通知
				messenger.callParentWithAsync(WorkerMessages.MESSAGE_SYNCRONOUSLY_STARTED.toString,
					messenger.tagValues(new TagValue("workerIdentity", identity),
						new TagValue("orderIdentity", orderIdentity)))
				doWork(currentAddedOrderInfo)
			}
		}
	}
	
	/**
	 * SPLIT_WAIT中、解除完了時に行う動作
	 */
	def procRestartOrContinueSplitWait(finished:Set[String], nextOrder:String) = {
		if (processSplitWaitOrderIdentitiesList.toSet.subsetOf(finished)) {
			println("workerIdentity "+identity+"	/が、SPLIT_WAIT中、解除開始、リクエストを親に投げる")
			WorkerStatus.STATUS_SPLIT_READY +=: currentStatus
				
			//Masterへの開始リクエストを行う
			messenger.callParentWithAsync(WorkerMessages.MESSAGE_REQUEST_SPECIFY.toString,
					messenger.tagValues(
							new TagValue("workerIdentity", identity)
							))
		}
	}
	
	/**
	 * リクエストを行うか、待ち続けるか
	 */
	def procRequestOrContinueAfterWait(finished:Set[String], currentAfterWait:Set[String], finishedOrderIdentity:String) = {
		if (currentAfterWait.subsetOf(finished)) {//完了しているのでリクエストを出す
			WorkerStatus.STATUS_REQUESTING +=: currentStatus
			messenger.callParentWithAsync(WorkerMessages.MESSAGE_REQUEST.toString,
					messenger.tagValues(
							new TagValue("workerIdentity", identity),
							new TagValue("finishedOrderIdentity",finishedOrderIdentity)
							))
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
				messenger.callParentWithAsync(WorkerMessages.MESSAGE_TIMEOUT.toString, messenger.tagValues(
					new TagValue("timeoutedWorkerIdentity", identity),
					new TagValue("timeoutedOrderIdentity", timeoutContextWorkInfo.orderIdentity),
					new TagValue("timeoutedOrderContext", timeoutContextWorkInfo.localContext)))

				WorkerStatus.STATUS_TIMEOUT +=: currentStatus
			}
		}, TimeUnit.MILLISECONDS.toMillis(delay));
	}

	/**
		非同期での着火(時間がかかるtaskでのロックを回避する)
	*/
	def procIgnite(tagValues : Array[TagValue]) = {
		val info = messenger.get("info", tagValues).asInstanceOf[WorkInformation]
		val stableCommand = messenger.get("stableCommand", tagValues).asInstanceOf[Seq[String]]
		// println("stableCommand 開始	"+stableCommand + "	/identity	" + identity)
		runProcess(info, stableCommand)
	}

	/**
	 * DOING中の完了
	 */
	def procDone(tagValues : Array[TagValue]) = {
		
		val result = messenger.get("result", tagValues).asInstanceOf[String]
		val info = messenger.get("info", tagValues).asInstanceOf[WorkInformation]
		
		// println("stableCommand 完了!!!	"+result + "	/identity	" + identity)
		
		//結果のコンテキストを作成する
		val eventualContext = info.localContext ++ Map(OrderPrefix._result.toString -> result)
		// println("eventualContext" + eventualContext+"	/identity	" + identity)
		
		//結果を残す
		new WorkInformation(info.orderIdentity, eventualContext, info.afterWaitIds) +=: currentWorkInformationHistory

		//親からのnotifyを待つ
		WorkerStatus.STATUS_AFTER_WAIT +=: currentStatus

		//親に終了を通知
		messenger.callParentWithAsync(WorkerMessages.MESSAGE_DONE.toString,
			messenger.tagValues(
				new TagValue("identity", identity),
				new TagValue("orderIdentity", info.orderIdentity),
				new TagValue("eventualContext", eventualContext)))
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
				
				println("実行開始前1　"+stableCommand)
				//実行
				messenger.callMyselfWithAsync(WorkerExecs.EXEC_IGNITION.toString, messenger.tagValues(new TagValue("info", info), new TagValue("stableCommand", stableCommand)))
				println("実行開始後1　"+stableCommand)
			}

			/*
			 * shell-process実行
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
				messenger.callMyselfWithAsync(WorkerExecs.EXEC_IGNITION.toString, messenger.tagValues(new TagValue("info", info), new TagValue("stableCommand", stableCommand)))
				
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
//			println("result	"+result+"	/out	"+out.toString+"	/err	"+err)
			println("stableCommand 完了!!!の前	"+processSrc + "	/identity	" + identity)
			messenger.callMyselfWithAsync(WorkerExecs.EXEC_DONE.toString,
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
		WorkerStatus.STATUS_ERROR +=: currentStatus

		//error結果のコンテキストを作成する
		val errorContext = info.localContext ++ Map(OrderPrefix._result.toString -> eStr)

		//結果を残す
		new WorkInformation(info.orderIdentity, errorContext, info.afterWaitIds) +=: currentWorkInformationHistory

		//親に終了を通知
		messenger.callParentWithAsync(WorkerMessages.MESSAGE_ERROR.toString,
			messenger.tagValues(
				new TagValue("erroredWorkerIdentity", identity),
				new TagValue("erroredOrderIdentity", info.orderIdentity),
				new TagValue("eventualContext", errorContext)))

	}
}