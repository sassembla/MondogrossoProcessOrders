package com.kissaki.mondogrosso.mondogrossoProcessOrders
import com.kissaki.MessengerProtocol
import scala.collection.mutable.ListBuffer
import com.kissaki.Messenger
import java.util.UUID
import java.util.Timer
import java.util.TimerTask
import java.util.concurrent.TimeUnit
import com.kissaki.TagValue
import java.util.Date
/**
 * コンテキスト
 *
 * 各Orderを実行する、Workerメッセージングのコアになる。
 */

//結果格納クラス
case class ContextResult(status : ContextStatus.Value, currentContext : scala.collection.mutable.Map[String, scala.collection.mutable.Map[String, String]], commentsStack : String)

class MondogrossoProcessContext(contextIdentity : String, contextSrc : ContextSource) extends MessengerProtocol {
	println(contextIdentity + "	contextIdentity	/	this	" + this)

	val MARK_DONE = "MARK_DONE"

	//状態
	var currentStatus : ListBuffer[ContextStatus.Value] = ListBuffer(ContextStatus.STATUS_NOTREADY)

	//メッセージング送信/受信
	val messenger = new Messenger(this, contextIdentity)

	/*プロセスリスト*/

	//実行中Processのリスト
	val runningProcessList : ListBuffer[String] = ListBuffer()

	//実行後Processのリスト
	val doneProcessList : ListBuffer[String] = ListBuffer()

	/*オーダーリスト*/

	//実行中のOrderのリスト
	val doingOrderIdentities : ListBuffer[String] = ListBuffer()

	//実行完了したOrderのリスト
	val doneOrderIdentities : ListBuffer[String] = ListBuffer()

	//現在実行しているIndex(全体を並べたときをもとにした順、実行順とは異なり、個数)
	var currentOrderIndex = 0

	//初期コンテキスト
	val initialContext = contextSrc.initialParam

	//コンテキスト Map(identity:String -> Map(key:String -> value:String)) 実行後の結果入力で上書きされていく。
	val currentContext = contextSrc.initialParam

	//コメント
	val comments : ListBuffer[String] = ListBuffer()

	/*
	 * 評価される瞬間に決定する、コンテキストの結果
	 */
	lazy val result = {
		val linedComments = comments.reduceRight {
			(total, toAdd) => total ++ ("\n" + toAdd)
		}

		ContextResult(currentStatus.head, currentContext, linedComments)
	}

	//現在のコンテキストのIdを返す
	def identity = contextIdentity

	def processNum = contextSrc.totalProcessNum

	def totalOrderNum = contextSrc.totalOrderCount

	//finally
	val finallyProcessIdentity = UUID.randomUUID().toString
	val finallyOrderIdentity = contextSrc.finallyOrder
	val finallyWorker = new ProcessWorker(finallyProcessIdentity, contextIdentity)

	//準備完了
	ContextStatus.STATUS_READY +=: currentStatus
	comments += commentFormat(new Date, "ready.")

	/**
	 * コメント作成
	 */
	def commentFormat(date : Date, comment : String) = date.toString + "	" + comment + "	CONTEXT:" + identity

	/**
	 * このコンテキストの現在のindexからの実行開始
	 */
	def runContext = {
		ContextStatus.STATUS_RUNNING +=: currentStatus
		comments += commentFormat(new Date, "start.")

		//プロセスごとにWorkerをセット、開始命令が送れるように整える
		contextSrc.current.processList.foreach { process =>
			dispachNewWorkerToProcess(process)
		}

		println("スタート時点でリストは	" + runningProcessList)

		//初期状態での、finallyの予約時間起動
		setContextTimeout

		//着火
		startProcess(contextSrc.current.processList(0))
	}

	/**
	 * レシーバ
	 */
	def receiver(execSrc : String, tagValues : Array[TagValue]) = {

		val exec = Messages.get(execSrc)

		currentStatus.head match {
			case ContextStatus.STATUS_NOTREADY => println("何もしない")

			case ContextStatus.STATUS_READY => println("runContext should")

			case ContextStatus.STATUS_RUNNING => procRunning(execSrc, tagValues)

			case ContextStatus.STATUS_TIMEOUT => procTimeout(exec, tagValues)
			case ContextStatus.STATUS_TIMEOUTED => println("STATUS_TIMEOUTED	exec	" + exec + "	/	" + identity)

			case ContextStatus.STATUS_FINALLY => procFinally(exec, tagValues)

			case ContextStatus.STATUS_DONE => println("everything over at this context" + identity + "	/currentStatus	" + currentStatus)

			case ContextStatus.STATUS_ERROR => //println("This Context:"+identity +"	is failed with Error:"+ currentContext)
		}
	}

	/**
	 * ContextTimeoutを起動
	 * 時間設定がある場合は開始になる。
	 */
	def setContextTimeout = {
		val delayValue = currentContext.get(finallyOrderIdentity) match {
			case Some(v) => {
				val candidate = v.get(OrderPrefix.__finallyTimeout.toString).getOrElse(OrderSettingDefault.SETTING_FINALLY_TIMEOUT_ZERO.toString)
				try {
					candidate.toInt
				} catch { //値が数値化できない、など
					case e : Exception => {
						contextErrorProc("CONTEXT->" + identity, finallyOrderIdentity, e.toString)
						OrderSettingDefault.SETTING_FINALLY_TIMEOUT_ZERO
					}
					case other => OrderSettingDefault.SETTING_FINALLY_TIMEOUT_ZERO
				}
			}
			case None => OrderSettingDefault.SETTING_FINALLY_TIMEOUT_ZERO
		}

		delayValue match {
			case OrderSettingDefault.SETTING_FINALLY_TIMEOUT_ZERO => println("thisContext = " + this + "	no timeout") //無ければ0扱い
			case _ => {
				try {
					val timer = new Timer("context_finally_delay_" + this);
					timer.schedule(new TimerTask {
						def run = {
							/*-----------一定時間後実行-----------*/

							//Running間を連続していないと成立しない処理(正常終了後のFailSafeになっている
							messenger.callMyself(ContextExecs.EXEC_TIMEOUT_RUN.toString, null)
						}
					}, TimeUnit.MILLISECONDS.toMillis(delayValue));
				} catch {
					case e : Exception => contextErrorProc("CONTEXT->" + identity, finallyOrderIdentity, e.toString)
					case other => contextErrorProc("CONTEXT->" + identity, finallyOrderIdentity, other.toString)
				}
			}
		}
		//		println("set timeout	" + delayValue)
	}

	/**
	 * Context単位でのError
	 */
	def contextErrorProc(erroredWorkerIdentity : String, erroredOrderIdentity : String, error : String) = {
		println("PROCESS:" + erroredWorkerIdentity + "	Error at Order:" + erroredOrderIdentity + "	reason:" + error)
		ContextStatus.STATUS_ERROR +=: currentStatus
		comments += commentFormat(new Date, "PROCESS:" + erroredWorkerIdentity + "	Error at Order:" + erroredOrderIdentity + "	reason:" + error)
	}

	/**
	 * プロセスに対して新規にWorkerを割当、起動する
	 */
	def dispachNewWorkerToProcess(process : Process) = {
		//リストに追加
		runningProcessList += process.identity

		//Workerを作成
		new ProcessWorker(process.identity, contextIdentity)

		//開始すべきIdentityを取得する(ここでは決めうちで0)
		val currentOrderIdentity = process.orderIdentityList(0)

		//process開始WaitId
		val processSplitIds = process.processSplitHeaders

		//order完了後WaitId
		val afterWaitIds = process.orderAdditional(currentOrderIdentity).waitIdentitiesList

		//Initialコンテキスト + 既存コンテキスト(既存コンテキストで上塗り)
		val actualRuntimeContext = generatePreRuntimeContext(process, currentOrderIdentity)

		println("currentOrderIdentity	" + currentOrderIdentity)
		println("processSplitIds	" + processSplitIds)
		println("afterWaitIds	" + afterWaitIds)
		println("actualRuntimeContext	" + actualRuntimeContext)

		//実行中Ordersにセット
		doingOrderIdentities += currentOrderIdentity

		comments += commentFormat(new Date, "PROCESS:" + process.identity + "	setUp 1st Order:" + currentOrderIdentity)
		messenger.call(process.identity, Messages.MESSAGE_SETUP.toString, messenger.tagValues(
			new TagValue("identity", currentOrderIdentity),
			new TagValue("processSplitIds", processSplitIds),
			new TagValue("afterWaitIds", afterWaitIds),
			new TagValue("context", actualRuntimeContext)))
	}

	/**
	 * 準備完了したWorkerを起動する
	 */
	def startProcess(process : Process) {
		//開始すべきIdentityを取得する(ここでは決めうちで0)
		val currentOrderIdentity = process.orderIdentityList(0)

		//process開始WaitId
		val processSplitIds = process.processSplitHeaders

		//order完了後WaitId
		val afterWaitIds = process.orderAdditional(currentOrderIdentity).waitIdentitiesList

		//Initialコンテキスト + 既存コンテキスト(既存コンテキストで上塗り)
		val actualRuntimeContext = generateActualRuntimeContext(process, currentOrderIdentity)

		comments += commentFormat(new Date, "PROCESS:" + process.identity + "	start 1st Order:" + currentOrderIdentity)
		messenger.call(process.identity, Messages.MESSAGE_START.toString, messenger.tagValues(
			new TagValue("identity", currentOrderIdentity),
			new TagValue("processSplitIds", processSplitIds),
			new TagValue("afterWaitIds", afterWaitIds),
			new TagValue("context", actualRuntimeContext)))
	}

	/**
	 * Workerに対して新しいOrderを割当、起動する
	 */
	def dispachWorkerToNextOrder(process : Process, index : Int) = {
		//開始すべきIdentityを取得する
		val currentOrderIdentity = process.orderIdentityList(index)
		val afterWaitIds = process.orderAdditional(currentOrderIdentity).waitIdentitiesList
		val actualRuntimeContext = generateActualRuntimeContext(process, currentOrderIdentity)

		println("actualRuntimeContext	" + actualRuntimeContext)
		println("currentOrderIdentity2	" + currentOrderIdentity)
		//実行中Ordersにセット
		doingOrderIdentities += currentOrderIdentity

		comments += commentFormat(new Date, "PROCESS:" + process.identity + "	setUp Order:" + currentOrderIdentity)
		messenger.call(process.identity, Messages.MESSAGE_SETUP.toString, messenger.tagValues(
			new TagValue("identity", currentOrderIdentity),
			new TagValue("processSplitIds", List()),
			new TagValue("afterWaitIds", afterWaitIds),
			new TagValue("context", actualRuntimeContext)))

		comments += commentFormat(new Date, "PROCESS:" + process.identity + "	start Order:" + currentOrderIdentity)
		messenger.call(process.identity, Messages.MESSAGE_START.toString, messenger.tagValues(
			new TagValue("identity", currentOrderIdentity),
			new TagValue("processSplitIds", List()),
			new TagValue("afterWaitIds", afterWaitIds),
			new TagValue("context", actualRuntimeContext)))

	}

	/**
	 * OrderIdentityから、実行時Contextを合成。
	 * SetUpに使用するので、仮の値を赦す。
	 * 
	 * OrderInputsがあれば、ここでパラメータをインプットする
	 */
	def generatePreRuntimeContext(process : Process, currentOrderIdentity : String) = {
		//入力するkey-valueを用意する
		try {
			val initial = contextSrc.initialParam.apply(currentOrderIdentity)
			println("initial	" + initial)
			//initialに対して、currentContextに同名の値があれば上書きする
			val currentEventual = currentContext.apply(currentOrderIdentity)
			println("currentEventual	" + currentEventual)
	
			val eventualContext = initial ++ currentEventual
			println("eventualContext	" + eventualContext)
	
			//存在するInputsを発生させ、eventuallyProtの特定の要素をcurrentContextから上書きする
			val context = process.orderAdditional.apply(currentOrderIdentity)
			  
			println("ここのcontext	"+context)
			val totalMap = for (inputs <- context.inputsList) yield {
				println("sourceOrderIdentity	" + inputs.sourceOrderIdentity + "	/from	" + inputs.from + "	/to	" + inputs.to)
				try {
	
					val v = currentContext.apply(inputs.sourceOrderIdentity)
					try {
						val fromValue = v.get(inputs.from).getOrElse()//唯一の違い
						println("fromValue	"+fromValue)
						
						try { //to 対象の有無をチェック
							currentContext.apply(currentOrderIdentity).apply(inputs.to)
						} catch {
							case e : Exception => contextErrorProc(process.identity, currentOrderIdentity, "sourceOrderIdentity	" + inputs.sourceOrderIdentity + "	/from	" + inputs.from + "	/to	" + inputs.to + ":has ERROR. " + e.toString)
						}
	
						//fromValueをtoに代入したMapを作成
						Map(inputs.to -> fromValue.toString)
					} catch {
						case e : Exception => contextErrorProc(process.identity, currentOrderIdentity, "sourceOrderIdentity	" + inputs.sourceOrderIdentity + "	/from	" + inputs.from + "	/to	" + inputs.to + ":has ERROR. " + e.toString)
					}
	
					//souceに指定したIdentityが含まれていない
				} catch {
					case e : Exception => contextErrorProc(process.identity, currentOrderIdentity, "sourceOrderIdentity	" + inputs.sourceOrderIdentity + "	/from	" + inputs.from + "	/to	" + inputs.to + ":has ERROR. " + e.toString)
				}
	
				Map()
			}
	
			//そもそもカラッぽなら従来のコンテキストを実行
			if (context.inputsList.isEmpty) {
				println("inputが用意されていない	"+eventualContext)
				eventualContext
			} else if (totalMap.isEmpty) {
				println("何もしない(エラーが返るはず)")
				//何もしない
			} else {
				println("値の合成	"+totalMap)
				val reduced = totalMap.reduceLeft { (total, toAdd) =>
					total ++ toAdd
				}
				val totalContext = eventualContext ++ reduced
				println("totalContext値の合成	"+totalContext)
				totalContext
			}
		} catch {
			case e : Exception => contextErrorProc(process.identity, currentOrderIdentity, "generateRuntimeContext has ERROR. " + e.toString)
		}
	}
	
	/**
	 * 一切のエラーを赦さない、実際の実行時のコンテキスト生成。
	 */
	def generateActualRuntimeContext(process : Process, currentOrderIdentity : String) = {
		//入力するkey-valueを用意する
		try {
			val initial = contextSrc.initialParam.apply(currentOrderIdentity)
			println("initial	" + initial)
			//initialに対して、currentContextに同名の値があれば上書きする
			val currentEventual = currentContext.apply(currentOrderIdentity)
			println("currentEventual	" + currentEventual)
	
			val eventualContext = initial ++ currentEventual
			println("eventualContext	" + eventualContext)
	
			//存在するInputsを発生させ、eventuallyProtの特定の要素をcurrentContextから上書きする
			val context = process.orderAdditional.apply(currentOrderIdentity)
			  
			println("ここのcontext	"+context)
			val totalMap = for (inputs <- context.inputsList) yield {
				println("sourceOrderIdentity	" + inputs.sourceOrderIdentity + "	/from	" + inputs.from + "	/to	" + inputs.to)
				try {
	
					val v = currentContext.apply(inputs.sourceOrderIdentity)
					try {
						val fromValue = v.apply(inputs.from)
	
						try { //to 対象の有無をチェック
							currentContext.apply(currentOrderIdentity).apply(inputs.to)
						} catch {
							case e : Exception => contextErrorProc(process.identity, currentOrderIdentity, "sourceOrderIdentity	" + inputs.sourceOrderIdentity + "	/from	" + inputs.from + "	/to	" + inputs.to + ":has ERROR. " + e.toString)
						}
	
						//fromValueをtoに代入したMapを作成
						Map(inputs.to -> fromValue.toString)
					} catch {
						case e : Exception => contextErrorProc(process.identity, currentOrderIdentity, "sourceOrderIdentity	" + inputs.sourceOrderIdentity + "	/from	" + inputs.from + "	/to	" + inputs.to + ":has ERROR. " + e.toString)
					}
	
					//souceに指定したIdentityが含まれていない
				} catch {
					case e : Exception => contextErrorProc(process.identity, currentOrderIdentity, "sourceOrderIdentity	" + inputs.sourceOrderIdentity + "	/from	" + inputs.from + "	/to	" + inputs.to + ":has ERROR. " + e.toString)
				}
	
				Map()
			}
	
			//そもそもカラッぽなら従来のコンテキストを実行
			if (context.inputsList.isEmpty) {
				println("inputが用意されていない	"+eventualContext)
				eventualContext
			} else if (totalMap.isEmpty) {
				println("何もしない(エラーが返るはず)")
				//何もしない
			} else {
				println("値の合成	"+totalMap)
				val reduced = totalMap.reduceLeft { (total, toAdd) =>
					total ++ toAdd
				}
				val totalContext = eventualContext ++ reduced
				println("totalContext値の合成	"+totalContext)
				totalContext
			}
		} catch {
			case e : Exception => contextErrorProc(process.identity, currentOrderIdentity, "generateRuntimeContext has ERROR. " + e.toString)
		}
	}

	/**
	 * 存在する有効なWorkerに直近のContext内でのOrderの終了通知を投げる
	 */
	def notifyFinishedOrderInfoToAllWorker(finishedOrderIdentity : String, allfinishedOrderIdentities : List[String]) = {
		for (processName <- runningProcessList) {
			println("notify the end of processName	" + processName + "	/	" + identity)
			messenger.callWithAsync(processName, Messages.MESSAGE_FINISHEDORDER_NOTIFY.toString, messenger.tagValues(
				new TagValue("finishedOrderIdentity", finishedOrderIdentity),
				new TagValue("allfinishedOrderIdentities", allfinishedOrderIdentities)))
		}
	}

	/**
	 * FinallyOrderを開始する
	 */
	def runFinally(finallyContext : scala.collection.mutable.Map[String, String]) = {
		comments += commentFormat(new Date, "FinallyOrder:" + finallyOrderIdentity + "	setUp.")
		messenger.call(finallyProcessIdentity, Messages.MESSAGE_SETUP.toString, messenger.tagValues(
			new TagValue("identity", finallyOrderIdentity),
			new TagValue("processSplitIds", List()),
			new TagValue("afterWaitIds", List()),
			new TagValue("context", finallyContext)))

		comments += commentFormat(new Date, "FinallyOrder:" + finallyOrderIdentity + "	start.")
		messenger.call(finallyProcessIdentity, Messages.MESSAGE_START.toString, messenger.tagValues(
			new TagValue("identity", finallyOrderIdentity),
			new TagValue("processSplitIds", List()),
			new TagValue("afterWaitIds", List()),
			new TagValue("context", finallyContext)))
	}

	/**
	 * Order実行時の動作
	 */
	def procRunning(execSrc : String, tagValues : Array[TagValue]) = {
		ContextExecs.get(execSrc) match {
			//waiterからのdelay時にまだRunningだったら
			case ContextExecs.EXEC_TIMEOUT_RUN => {
				ContextStatus.STATUS_TIMEOUT +=: currentStatus
				comments += commentFormat(new Date, "Timeout break out.	" + identity)

				currentContext.get(finallyOrderIdentity).foreach { finallyContext => runFinally(finallyContext) }
			}
			case other =>
		}

		Messages.get(execSrc) match {
			case Messages.MESSAGE_SYNCRONOUSLY_STARTED => {
				val workerIdentity = messenger.get("workerIdentity", tagValues).asInstanceOf[String]
				val orderIdentity = messenger.get("orderIdentity", tagValues).asInstanceOf[String]
				comments += commentFormat(new Date, "PROCESS:" + workerIdentity + "	started Order:" + orderIdentity)
			}
			case Messages.MESSAGE_ASYNCRONOUSLY_STARTED => {
				val workerIdentity = messenger.get("workerIdentity", tagValues).asInstanceOf[String]
				val orderIdentity = messenger.get("orderIdentity", tagValues).asInstanceOf[String]
				comments += commentFormat(new Date, "PROCESS:" + workerIdentity + "	started asynchronous Order:" + orderIdentity)
			}

			case Messages.MESSAGE_DONE => {
				val currentFinishedWorkerIdentity = messenger.get("identity", tagValues).asInstanceOf[String]
				val currentFinishedOrderIdentity = messenger.get("orderIdentity", tagValues).asInstanceOf[String]
				val currentFinishedEventualContext = messenger.get("eventualContext", tagValues).asInstanceOf[scala.collection.mutable.Map[String, String]]

				println("currentFinishedWorkerIdentity	" + currentFinishedWorkerIdentity)
				println("currentFinishedOrderIdentity	" + currentFinishedOrderIdentity)
				//				println("currentFinishedEventualContext	" + currentFinishedEventualContext)

				//eventualを、currentContextに上書きする
				val appendedContext = (currentFinishedOrderIdentity -> currentFinishedEventualContext)
				currentContext += appendedContext
				comments += commentFormat(new Date, "PROCESS:" + currentFinishedWorkerIdentity + "	Order:" + currentFinishedOrderIdentity + " was done!")

				//動作中リストから除外
				doingOrderIdentities -= currentFinishedOrderIdentity
				//動作済みリストに追記
				doneOrderIdentities += currentFinishedOrderIdentity

				/*
				 * 非同期で、Contextが調整されたことを全Workerに通達する -> Workerからのリクエストを待つ(相手が特定のOrderのWait状態になっていればRequestが来る。) 
				 * PushKVO。
				 * WorkerからみればPassiveKVO。
				 */
				notifyFinishedOrderInfoToAllWorker(currentFinishedOrderIdentity, doneOrderIdentities.toList)
			}

			/*
			 * Notifyを受けたWorkerからのOrderリクエストを受ける
			 * 
			 * そのWorkerへの次のOrderが無ければ、対象Workerを停止する
			 * 全Workerが停止されたら、Context自体のfinallyを実行する
			 */
			case Messages.MESSAGE_REQUEST => {
				val processIdentity = messenger.get("workerIdentity", tagValues).asInstanceOf[String]
				val finishedOrderIdentity = messenger.get("finishedOrderIdentity", tagValues).asInstanceOf[String]

				//終了したOrderの属するProcessを検索、次のOrderを探す
				contextSrc.current.processList.withFilter(_.identity.equals(processIdentity)).foreach { process =>
					//終了したOrderのIndexを出し、次があれば実行する。
					val currentOrderIdentityIndex = (process.orderIdentityList.indexOf(finishedOrderIdentity) + 1)
					println("process.orderIdentityList	" + process.orderIdentityList)
					println("index	" + process.orderIdentityList.indexOf(finishedOrderIdentity))
					println("finishedOrderIdentity	" + process.orderIdentityList.indexOf(finishedOrderIdentity))

					comments += commentFormat(new Date, "PROCESS:" + processIdentity + "	requested the next Order of	" + finishedOrderIdentity)

					currentOrderIdentityIndex match {
						//次のOrderを実行
						case number if (number < process.orderIdentityList.length) => {
							dispachWorkerToNextOrder(process, number)
						}

						//先ほど完了したのがこのProcessのラスト
						case last => {
							//runningProcessListリストからdoneProcessListへとprocessを移す
							//							println("この時点で	runningProcessList	"+runningProcessList+"	/から、	"+processIdentity+"	/が引かれる")
							runningProcessList -= processIdentity
							doneProcessList += processIdentity

							//Workerを停める
							messenger.call(processIdentity, Messages.MESSAGE_OVER.toString, null)

							//							println("この時点でリストは	"+runningProcessList)
							comments += commentFormat(new Date, "PROCESS:" + processIdentity + "	All Orders done!")

							//runningProcessListが空だったらfinallyを実行
							if (runningProcessList.isEmpty) {
								runningProcessList += MARK_DONE
								ContextStatus.STATUS_FINALLY +=: currentStatus
								comments += commentFormat(new Date, "FinallyOrder:" + finallyOrderIdentity + "	ready.")

								messenger.callMyself(Messages.MESSAGE_FINALLY.toString, null)
							}
						}
					}

					//みかけの実行Index+1
					currentOrderIndex += 1
				}
			}

			/*
			 * Orderからのエラー
			 */
			case Messages.MESSAGE_ERROR => {
				ContextStatus.STATUS_ERROR +=: currentStatus

				val erroredWorkerIdentity = messenger.get("erroredWorkerIdentity", tagValues).asInstanceOf[String]
				val erroredOrderIdentity = messenger.get("erroredOrderIdentity", tagValues).asInstanceOf[String]
				val eventualContext = messenger.get("eventualContext", tagValues).asInstanceOf[scala.collection.mutable.Map[String, String]]

				contextErrorProc(erroredWorkerIdentity, erroredOrderIdentity, eventualContext.apply(OrderPrefix._result.toString))
			}

			case _ => println("nothing todo in STATUS_RUNNING	-exec	" + execSrc)
		}
	}

	/**
	 * Timeout時の動作
	 *
	 * 現段階のContextでのfinallyが実行される
	 */
	def procTimeout(exec : Messages.Value, tagValues : Array[TagValue]) = {
		exec match {
			case Messages.MESSAGE_SYNCRONOUSLY_STARTED =>
			case Messages.MESSAGE_ASYNCRONOUSLY_STARTED =>

			case Messages.MESSAGE_DONE => {

				val currentFinishedWorkerIdentity = messenger.get("identity", tagValues).asInstanceOf[String]
				val currentFinishedOrderIdentity = messenger.get("orderIdentity", tagValues).asInstanceOf[String]
				val currentFinishedEventualContext = messenger.get("eventualContext", tagValues).asInstanceOf[scala.collection.mutable.Map[String, String]]

				if (currentFinishedOrderIdentity.equals(finallyOrderIdentity) && currentFinishedWorkerIdentity.equals(finallyProcessIdentity)) {

					val appendedContext = (currentFinishedOrderIdentity -> currentFinishedEventualContext)

					currentContext += appendedContext
					ContextStatus.STATUS_TIMEOUTED +=: currentStatus
					comments += commentFormat(new Date, "Timeout Order:" + currentFinishedOrderIdentity + "	was finished.	" + identity)
				}
			}

			case _ => println("nothing todo in STATUS_TIMEOUT	-exec	" + exec + "	/	" + identity)
		}
	}

	/**
	 * Finallyが通常実行された際の動作
	 */
	def procFinally(exec : Messages.Value, tagValues : Array[TagValue]) = {
		exec match {
			case Messages.MESSAGE_FINALLY => currentContext.get(finallyOrderIdentity).foreach {
				finallyContext => runFinally(finallyContext)
			}

			case Messages.MESSAGE_SYNCRONOUSLY_STARTED => {
				val orderIdentity = messenger.get("orderIdentity", tagValues).asInstanceOf[String]
				comments += commentFormat(new Date, "FinallyOrder:" + orderIdentity + "	started.")
			}
			case Messages.MESSAGE_ASYNCRONOUSLY_STARTED => {
				val orderIdentity = messenger.get("orderIdentity", tagValues).asInstanceOf[String]
				comments += commentFormat(new Date, "FinallyOrder:	" + orderIdentity + "	started asynchronously")
			}

			case Messages.MESSAGE_DONE => {
				val currentFinishedWorkerIdentity = messenger.get("identity", tagValues).asInstanceOf[String]
				val currentFinishedOrderIdentity = messenger.get("orderIdentity", tagValues).asInstanceOf[String]
				val currentFinishedEventualContext = messenger.get("eventualContext", tagValues).asInstanceOf[scala.collection.mutable.Map[String, String]]

				if (currentFinishedOrderIdentity.equals(finallyOrderIdentity) && currentFinishedWorkerIdentity.equals(finallyProcessIdentity)) {
					//eventualを、currentContextに上書きする
					val appendedContext = (currentFinishedOrderIdentity -> currentFinishedEventualContext)
					currentContext += appendedContext

					//動作中リストから除外
					doingOrderIdentities -= currentFinishedOrderIdentity
					//動作済みリストに追記
					doneOrderIdentities += currentFinishedOrderIdentity

					ContextStatus.STATUS_DONE +=: currentStatus
					comments += commentFormat(new Date, "finished!")
				}
			}

			case _ => println("nothing todo in STATUS_FINALLY	-exec	" + exec)
		}
	}
}