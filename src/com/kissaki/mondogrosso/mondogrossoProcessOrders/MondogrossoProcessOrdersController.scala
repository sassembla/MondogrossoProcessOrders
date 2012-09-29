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
class ProcessContext(contextIdentity : String, contextSrc : ContextSource) extends MessengerProtocol {

	//状態
	var currentStatus = ContextStatus.STATUS_NONE

	//メッセージング送信/受信
	val messenger = new Messenger(this, contextIdentity)

	//実行中Processのリスト
	val runningProcessList : ListBuffer[String] = ListBuffer()

	//実行後Processのリスト
	val doneProcessList : ListBuffer[String] = ListBuffer()

	//現在実行しているIndex(全体を並べたときをもとにした順、実行順とは異なり、個数)
	var currentOrderIndex = 0

	//実行中のOrderのList
	val currentExecutingOrders : ListBuffer[String] = ListBuffer()

	//初期コンテキスト
	val initialContext = contextSrc.initialParam

	//コンテキスト Map(identity:String -> Map(key:String -> value:String)) 実行後の結果入力で上書きされていく。
	val currentContext = contextSrc.initialParam

	/**
	 * 現在のコンテキストのIdを返す
	 */
	def identity = contextIdentity

	def processNum = contextSrc.totalProcessNum

	def totalOrderNum = contextSrc.totalOrderCount

	//finally
	val finallyProcessIdentity = UUID.randomUUID().toString
	val finallyOrderIdentity = contextSrc.finallyOrder
	val finallyWorker = new ProcessWorker(finallyProcessIdentity, contextIdentity)

	//準備完了
	currentStatus = ContextStatus.STATUS_READY

	/**
	 * このコンテキストの現在のindexからの実行開始
	 */
	def runContext = {
		currentStatus = ContextStatus.STATUS_RUNNING

		//初期状態での、finallyの予約時間起動
		setContextTimeout

		//プロセスごとにWorkerを起動
		contextSrc.current.processList.foreach { process =>
			dispachNewWorkerToProcess(process)
		}
	}

	/**
	 * ContextTimeoutを起動
	 * 時間設定がある場合は開始になる。
	 */
	def setContextTimeout = {
		messenger.callMyselfWithAsync(Messages.MESSAGE_EXEC_TIMEOUT_READY.toString, null);
	}

	/**
	 * Context単位でのError
	 */
	def contextErrorProc(erroredContext : scala.collection.mutable.Map[String, scala.collection.mutable.Map[String, String]], error : String) = {
		println("エラーが発生	" + erroredContext + "	/error	" + error)
		currentStatus = ContextStatus.STATUS_ERROR
	}

	/**
	 * プロセスに対して新規にWorkerを割当、起動する
	 */
	def dispachNewWorkerToProcess(process : Process) = {
		//新しいProcessを登録する
		runningProcessList += process.identity

		//Workerを作成
		new ProcessWorker(process.identity, contextIdentity)

		//開始すべきIdentityを取得する(ここでは決めうちで0)
		val currentOrderIdentity = process.orderIdentityList(0)

		val actualRuntimeContext = generateRuntimeContext(currentOrderIdentity)

		messenger.call(process.identity, Messages.MESSAGE_START.toString, messenger.tagValues(
			new TagValue("identity", currentOrderIdentity),
			new TagValue("context", actualRuntimeContext)))

		//実行中のOrderをセット
		currentExecutingOrders += currentOrderIdentity
	}

	/**
	 * Workerに対して新しいOrderを割当、起動する
	 */
	def dispachWorkerToNextOrder(process : Process, index : Int) = {
		//開始すべきIdentityを取得する(ここでは決めうちで0)
		val currentOrderIdentity = process.orderIdentityList(index)

		val actualRuntimeContext = generateRuntimeContext(currentOrderIdentity)

		messenger.call(process.identity, Messages.MESSAGE_START.toString, messenger.tagValues(
			new TagValue("identity", currentOrderIdentity),
			new TagValue("context", actualRuntimeContext)))

		//実行中のOrderをセット
		currentExecutingOrders += currentOrderIdentity
	}

	/**
	 * OrderIdentityから、実行時Contextを合成する
	 */
	def generateRuntimeContext(currentOrderIdentity : String) = {

		//入力するkey-valueを用意する
		val initial = contextSrc.initialParam.get(currentOrderIdentity) match {
			case Some(v) => v
			case None => Map("" -> "") //エラー
		}

		//initialに対して、currentContextに同名の値があれば上書きする
		val currentEventual = currentContext.get(currentOrderIdentity) match {
			case Some(v) => v
			case None => Map()
		}
		initial ++ currentEventual
	}

	/**
	 * 存在する有効なWorkerに直近のContext内でのOrderの終了通知を投げる
	 */
	def notifyFinishedOrderInfoToAllWorker(finishedOrderIdentity : String) = {
		for (processName <- runningProcessList) {
			println("processName notify after	" + processName)
			messenger.callWithAsync(processName, Messages.MESSAGE_FINISHEDORDER_NOTIFY.toString, messenger.tagValues(
				new TagValue("finishedOrderIdentity", finishedOrderIdentity)))
		}
	}

	/**
	 * FinallyOrderを開始する
	 */
	def runFinally(finallyContext : scala.collection.mutable.Map[String, String]) = {
		println("finallyContext	" + finallyContext)
		messenger.call(finallyProcessIdentity, Messages.MESSAGE_START.toString, messenger.tagValues(
			new TagValue("identity", finallyOrderIdentity),
			new TagValue("context", finallyContext)))
	}

	/**
	 * レシーバ
	 */
	def receiver(execSrc : String, tagValues : Array[TagValue]) = {

		val exec = Messages.get(execSrc)

		currentStatus match {
			case ContextStatus.STATUS_NONE => println("何も")

			case ContextStatus.STATUS_READY => println("runContext should")

			case ContextStatus.STATUS_RUNNING => procRunning(exec, tagValues)

			case ContextStatus.STATUS_TIMEOUT => procTimeout(exec, tagValues)
			case ContextStatus.STATUS_TIMEOUTED => println("STATUS_TIMEOUTED")

			case ContextStatus.STATUS_FINALLY => procFinally(exec, tagValues)

			case ContextStatus.STATUS_DONE => println("everything over at this context")

			case ContextStatus.STATUS_ERROR => println("in the Error.." + exec)
		}
	}

	/**
	 * Order実行時の動作
	 */
	def procRunning(exec : Messages.Value, tagValues : Array[TagValue]) = {
		exec match {
			case Messages.MESSAGE_EXEC_TIMEOUT_READY => currentContext.get(finallyOrderIdentity).foreach { finallyContext =>
				val source = finallyContext.get(OrderPrefix.__finallyTimeout.toString).getOrElse(OrderSettingDefault.SETTING_FINALLY_NO_TIMEOUT)
				source match {
					case OrderSettingDefault.SETTING_FINALLY_NO_TIMEOUT => println("記述が無いのでタイムアウトしない")
					case other => {
						try {
							val delay = other.toInt
							try {
								Thread.sleep(delay)
								/*-----------一定時間後実行-----------*/

								//Running間を連続していないと成立しない処理(正常終了後のFailSafeになっている
								messenger.callMyself(Messages.MESSAGE_EXEC_TIMEOUT_RUN.toString, null)
							} catch { //sleepに対するException -値など
								case e : Exception => contextErrorProc(currentContext, e.toString)
								case other => println("sleep 例外以外	" + other)
							}
						} catch { //値が数値化できない、など
							case e : Exception => contextErrorProc(currentContext, e.toString)
							case other => println("toInt 例外以外	" + other)
						}
					}
				}

			}
			case Messages.MESSAGE_EXEC_TIMEOUT_RUN => {
				currentStatus = ContextStatus.STATUS_TIMEOUT
				currentContext.get(finallyOrderIdentity).foreach { finallyContext => runFinally(finallyContext) }
			}

			case Messages.MESSAGE_SYNCRONOUSLY_STARTED => println("currentOrderIndex	started	" + currentOrderIndex + "	/tagValues	" + tagValues)
			case Messages.MESSAGE_ASYNCRONOUSLY_STARTED => println("currentOrderIndex	async started	" + currentOrderIndex + "	/tagValues	" + tagValues)

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

				/*
				 * 非同期で、Contextが調整されたことを全Workerに通達する -> Workerからのリクエストを待つ(相手が特定のOrderのWait状態になっていればRequestが来る。) 
				 * PushKVO。
				 * WorkerからみればPassiveKVO。
				 */
				notifyFinishedOrderInfoToAllWorker(currentFinishedOrderIdentity)
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
					val currentOrderIdentityIndex = process.orderIdentityList.lastIndexOf(finishedOrderIdentity)
					println("currentOrderIdentityIndex	" + currentOrderIdentityIndex)

					currentOrderIdentityIndex match {
						//先ほど完了したのがこのProcessのラスト
						case 0 => {
							//runningProcessListリストからdoneProcessListへとprocessを移す
							runningProcessList -= processIdentity
							doneProcessList += processIdentity

							//Workerを停める
							messenger.call(processIdentity, Messages.MESSAGE_OVER.toString, null)

							//runningProcessListが空だったらfinallyを実行
							if (runningProcessList.isEmpty) {
								currentStatus = ContextStatus.STATUS_FINALLY
								messenger.callMyself(Messages.MESSAGE_FINALLY.toString, null)
							}
						}
						case notZero =>
							{
								println("このプロセスは" + (notZero + 1) + "番に移行")
								dispachWorkerToNextOrder(process, notZero + 1)
							}

							//実行Index+1
							currentOrderIndex += 1

					}
				}
			}
			case _ => println("nothing todo in STATUS_RUNNING	-exec	" + exec)
		}
	}

	/**
	 * Timeout時の動作
	 *
	 * 現段階のContextでのfinallyが実行される
	 */
	def procTimeout(exec : Messages.Value, tagValues : Array[TagValue]) = {
		exec match {
			case Messages.MESSAGE_SYNCRONOUSLY_STARTED => println("currentOrderIndex	started	" + currentOrderIndex + "	/tagValues	" + tagValues)
			case Messages.MESSAGE_ASYNCRONOUSLY_STARTED => println("currentOrderIndex	async started	" + currentOrderIndex + "	/tagValues	" + tagValues)

			case Messages.MESSAGE_DONE => {
				val currentFinishedWorkerIdentity = messenger.get("identity", tagValues).asInstanceOf[String]
				val currentFinishedOrderIdentity = messenger.get("orderIdentity", tagValues).asInstanceOf[String]
				val currentFinishedEventualContext = messenger.get("eventualContext", tagValues).asInstanceOf[scala.collection.mutable.Map[String, String]]

				if (currentFinishedOrderIdentity.equals(finallyOrderIdentity) && currentFinishedWorkerIdentity.equals(finallyProcessIdentity)) {
					val appendedContext = (currentFinishedOrderIdentity -> currentFinishedEventualContext)

					currentContext += appendedContext
					currentStatus = ContextStatus.STATUS_TIMEOUTED
				}
			}

			case _ => println("nothing todo in STATUS_TIMEOUT	-exec	" + exec)
		}
	}

	/**
	 * Finallyが通常実行された際の動作
	 */
	def procFinally(exec : Messages.Value, tagValues : Array[TagValue]) = {
		exec match {
			case Messages.MESSAGE_FINALLY => currentContext.get(finallyOrderIdentity).foreach { finallyContext => runFinally(finallyContext) }

			case Messages.MESSAGE_SYNCRONOUSLY_STARTED => println("currentOrderIndex	started	" + currentOrderIndex + "	/tagValues	" + tagValues)
			case Messages.MESSAGE_ASYNCRONOUSLY_STARTED => println("currentOrderIndex	async started	" + currentOrderIndex + "	/tagValues	" + tagValues)

			case Messages.MESSAGE_DONE => {
				val currentFinishedWorkerIdentity = messenger.get("identity", tagValues).asInstanceOf[String]
				val currentFinishedOrderIdentity = messenger.get("orderIdentity", tagValues).asInstanceOf[String]
				val currentFinishedEventualContext = messenger.get("eventualContext", tagValues).asInstanceOf[scala.collection.mutable.Map[String, String]]

				if (currentFinishedOrderIdentity.equals(finallyOrderIdentity) && currentFinishedWorkerIdentity.equals(finallyProcessIdentity)) {
					//eventualを、currentContextに上書きする
					val appendedContext = (currentFinishedOrderIdentity -> currentFinishedEventualContext)
					currentContext += appendedContext

					currentStatus = ContextStatus.STATUS_DONE
				}
			}

			case _ => println("nothing todo in STATUS_FINALLY	-exec	" + exec)
		}
	}
}