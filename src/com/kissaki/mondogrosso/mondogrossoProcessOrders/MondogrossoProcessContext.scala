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
 * 各Orderを実行する、Worker
 * メッセージングの末端になる。
 */

//結果格納クラス
case class ContextResult(contextIdentity: String, status: ContextStatus.Value, currentContext: scala.collection.mutable.Map[String, scala.collection.mutable.Map[String, String]], commentsStack: String)

class MondogrossoProcessContext(contextIdentity: String, contextSrc: ContextSource, masterName: String) extends MessengerProtocol {
  //	println(contextIdentity + "	contextIdentity	/	this	" + this)

  //状態
  val status: ListBuffer[ContextStatus.Value] = ListBuffer(ContextStatus.STATUS_NOTREADY)
  def currentStatus = status.head

  //メッセージング送信/受信
  val messenger = new Messenger(this, contextIdentity)
  messenger.inputParent(masterName)

  /*プロセスリスト*/

  //実行中Processのリスト
  val runningProcessList: ListBuffer[String] = ListBuffer()

  //実行後Processのリスト
  val doneProcessList: ListBuffer[String] = ListBuffer()

  /*オーダーリスト*/

  //実行中のOrderのリスト
  val doingOrderIdentities: ListBuffer[String] = ListBuffer()

  //実行完了したOrderのリスト
  val doneOrderIdentities: ListBuffer[String] = ListBuffer()

  //初期コンテキスト
  val initialContext = contextSrc.initialParam

  //コンテキスト Map(identity:String -> Map(key:String -> value:String)) 実行後の結果入力で上書きされていく。
  val contextKeyValues = contextSrc.initialParam

  //コメント
  val comments: ListBuffer[String] = ListBuffer()

  /**
   * commentsを末尾から読み、改行をくわえる
   * stackTraceもどきを作成
   */
  def linedComments = comments.reduceRight {
    (total, toAdd) => total ++ ("\n" + toAdd)
  }

  /*
	 * コンテキストの結果を返す
	 */
  def currentContextResult = ContextResult(contextIdentity, currentStatus, contextKeyValues, linedComments)

  //現在のコンテキストのIdを返す
  def identity = contextIdentity

  def processNum = contextSrc.totalProcessNum

  def totalOrderNum = contextSrc.totalOrderCount

  //finally
  val finallyProcessIdentity = UUID.randomUUID().toString
  val finallyOrderIdentity = contextSrc.finallyOrder
  val finallyWorker = new ProcessWorker(finallyProcessIdentity, contextIdentity)

  //準備完了
  ContextStatus.STATUS_READY +=: status
  comments += commentFormat(new Date, "ready.")

  //マスターに準備完了を通達
  messenger.callParent(ContextMessages.MESSAGE_READY.toString, messenger.tagValues(
    new TagValue("contextIdentity", identity),
    new TagValue("contextOrderIndex", doneOrderIdentities.length),
    new TagValue("contextOrderTotal", contextSrc.totalOrderCount),
    new TagValue("contextProcessIndex", doneProcessList.length),
    new TagValue("contextProcessTotal", contextSrc.totalProcessNum)))

  /**
   * コメント作成
   */
  def commentFormat(date: Date, comment: String) = date.toString + "	" + comment + "	CONTEXT:" + identity

  /**
   * このコンテキストの現在のindexからの実行開始
   */
  def runContext = {
    ContextStatus.STATUS_RUNNING +=: status
    comments += commentFormat(new Date, "start.")

    //プロセスごとにWorkerをセット、開始命令が送れるように整える
    contextSrc.current.processList.foreach { process =>
      dispachNewWorkerToProcess(process)
    }

    messenger.callParent(ContextMessages.MESSAGE_START.toString, messenger.tagValues(
      new TagValue("contextIdentity", identity),
      new TagValue("contextOrderIndex", doneOrderIdentities.length),
      new TagValue("contextOrderTotal", contextSrc.totalOrderCount),
      new TagValue("contextProcessIndex", doneProcessList.length),
      new TagValue("contextProcessTotal", contextSrc.totalProcessNum)))

    println("このcontextのidentity " + identity + "  /スタート時点でリストは	" + runningProcessList)

    //初期状態での、finallyの予約時間起動
    setContextTimeout

    //着火
    startProcess(contextSrc.current.processList(0))
    identity
  }


  /**
   * レシーバ
   */
  def receiver(execSrc: String, tagValues: Array[TagValue]) = {
    currentStatus match {
      case ContextStatus.STATUS_NOTREADY =>

      case ContextStatus.STATUS_READY =>

      case ContextStatus.STATUS_RUNNING => procRunning(execSrc, tagValues)

      case ContextStatus.STATUS_TIMEOUT => procTimeout(WorkerMessages.get(execSrc), tagValues)
      case ContextStatus.STATUS_TIMEOUTED =>

      case ContextStatus.STATUS_FINALLY => procFinally(WorkerMessages.get(execSrc), tagValues)

      case ContextStatus.STATUS_DONE =>

      case ContextStatus.STATUS_ERROR =>

      case other => println("other	" + other)
    }
  }

  /**
   * ContextTimeoutを起動
   * 時間設定がある場合は開始になる。
   */
  def setContextTimeout = {
    val delayValue = contextKeyValues.get(finallyOrderIdentity) match {
      case Some(v) => {
        val candidate = v.get(OrderPrefix.__finallyTimeout.toString).getOrElse(OrderSettingDefault.SETTING_FINALLY_TIMEOUT_ZERO.toString)
        try {
          candidate.toInt
        } catch { //値が数値化できない、など
          case e: Exception => {
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
          println("タイムアウトのセット " + delayValue)
          val timer = new Timer("context_finally_delay_" + this);
          timer.schedule(new TimerTask {
            def run = {
              /*-----------一定時間後実行-----------*/

              //Running間を連続していないと成立しない処理(正常終了後のFailSafeになっている
              messenger.callMyself(ContextExecs.EXEC_TIMEOUT_RUN.toString, null)
            }
          }, TimeUnit.MILLISECONDS.toMillis(delayValue));
        } catch {
          case e: Exception => contextErrorProc("CONTEXT->" + identity, finallyOrderIdentity, e.toString)
          case other => contextErrorProc("CONTEXT->" + identity, finallyOrderIdentity, other.toString)
        }
      }
    }
    //		println("set timeout	" + delayValue)
  }

  /**
   * Context単位でのError
   */
  def contextErrorProc(erroredWorkerIdentity: String, erroredOrderIdentity: String, error: String) = {
    println("PROCESS:" + erroredWorkerIdentity + "	Error at Order:" + erroredOrderIdentity + "	reason:" + error)
    ContextStatus.STATUS_ERROR +=: status
    comments += commentFormat(new Date, "PROCESS:" + erroredWorkerIdentity + "	Error at Order:" + erroredOrderIdentity + "	reason:" + error)
  }

  /**
   * プロセスに対して新規にWorkerを割当、起動する
   */
  def dispachNewWorkerToProcess(process: Process) = {
    //リストに追加
    runningProcessList += process.identity

    //Workerを作成
    new ProcessWorker(process.identity, contextIdentity)

    //開始すべきIdentityを取得する(ここでは決めうちで0)
    val currentOrderIdentity = process.orderIdentityList.head

    //process開始WaitId
    val processSplitIds = process.processSplitHeaders

    //order完了後WaitId
    val afterWaitIds = process.orderAdditional(currentOrderIdentity).waitIdentitiesList

    //Initialコンテキスト + 既存コンテキスト(既存コンテキストで上塗り)
    val actualRuntimeContext = generatePreRuntimeContext(process, currentOrderIdentity)

    println("実行開始 currentOrderIdentity	" + currentOrderIdentity)
    // println("processSplitIds	" + processSplitIds)
    // println("afterWaitIds	" + afterWaitIds)
    // println("actualRuntimeContext	" + actualRuntimeContext)

    //実行中Ordersにセット
    doingOrderIdentities += currentOrderIdentity

    comments += commentFormat(new Date, "PROCESS:" + process.identity + "	setUp 1st Order:" + currentOrderIdentity)
    messenger.call(process.identity, WorkerMessages.MESSAGE_SETUP.toString, messenger.tagValues(
      new TagValue("identity", currentOrderIdentity),
      new TagValue("processSplitIds", processSplitIds),
      new TagValue("afterWaitIds", afterWaitIds),
      new TagValue("context", actualRuntimeContext)))
  }

  /**
   * 準備完了したWorkerを起動する
   */
  def startProcess(process: Process) {
    //開始すべきIdentityを取得する(ここでは決めうちで0)
    val currentOrderIdentity = process.orderIdentityList.head

    //process開始WaitId
    val processSplitIds = process.processSplitHeaders

    //order完了後WaitId
    val afterWaitIds = process.orderAdditional(currentOrderIdentity).waitIdentitiesList

    //Initialコンテキスト + 既存コンテキスト(既存コンテキストで上塗り)
    val actualRuntimeContext = generateActualRuntimeContext(process, currentOrderIdentity)

    //実行中Ordersにセット
    doingOrderIdentities += currentOrderIdentity

    comments += commentFormat(new Date, "PROCESS:" + process.identity + "	start 1st Order:" + currentOrderIdentity)

    messenger.call(process.identity, WorkerMessages.MESSAGE_START.toString, messenger.tagValues(
      new TagValue("identity", currentOrderIdentity),
      new TagValue("processSplitIds", processSplitIds),
      new TagValue("afterWaitIds", afterWaitIds),
      new TagValue("context", actualRuntimeContext)))
  }

  /**
   * Workerに対して新しいOrderを割当、起動する
   */
  def dispachNextOrderToWorker(process: Process, index: Int) = {

    val currentOrderIdentity = process.orderIdentityList(index)

    //process開始WaitId
    val processSplitIds = process.processSplitHeaders

    //order完了後WaitId
    val afterWaitIds = process.orderAdditional(currentOrderIdentity).waitIdentitiesList

    //Initialコンテキスト + 既存コンテキスト(既存コンテキストで上塗り)
    val actualRuntimeContext = generateActualRuntimeContext(process, currentOrderIdentity)

    println("dispachNextOrderToWorker	currentOrderIdentity	" + currentOrderIdentity + "	/actualRuntimeContext	" + actualRuntimeContext)

    //実行中Ordersにセット
    doingOrderIdentities += currentOrderIdentity

    comments += commentFormat(new Date, "PROCESS:" + process.identity + "	setUp then start Order:" + currentOrderIdentity)

    messenger.call(process.identity, WorkerMessages.MESSAGE_SETUP_AND_START.toString, messenger.tagValues(
      new TagValue("identity", currentOrderIdentity),
      new TagValue("processSplitIds", processSplitIds),
      new TagValue("afterWaitIds", afterWaitIds),
      new TagValue("context", actualRuntimeContext)))
  }

  /**
   * OrderIdentityから、実行時Contextを合成。
   * SetUpに使用するので、仮の値を赦す。
   *
   * OrderInputsがあれば、ここでパラメータをインプットする
   */
  def generatePreRuntimeContext(process: Process, currentOrderIdentity: String) = {
    //入力するkey-valueを用意する
    try {
      val initial = contextSrc.initialParam.apply(currentOrderIdentity)

      //initialに対して、currentContextに同名の値があれば上書きする
      val currentEventual = contextKeyValues.apply(currentOrderIdentity)

      //結果としてのcontext　このあとinputをくわえる。
      val eventualContext = initial ++ currentEventual

      //存在するInputsを発生させ、eventuallyProtの特定の要素をcurrentContextから上書きする
      val context = process.orderAdditional.apply(currentOrderIdentity)

      val totalMap = for (inputs <- context.inputsList) yield {
        try {
          val v = contextKeyValues.apply(inputs.sourceOrderIdentity)
          try {
            val fromValue = v.get(inputs.from).getOrElse() //唯一のActualとの違い

            try { //to 対象の有無をチェック
              contextKeyValues.apply(currentOrderIdentity).apply(inputs.to)
            } catch {
              case e: Exception => contextErrorProc(process.identity, currentOrderIdentity, "sourceOrderIdentity	" + inputs.sourceOrderIdentity + "	/from	" + inputs.from + "	/to	" + inputs.to + ":has ERROR. " + e.toString)
            }

            //fromValueをtoに代入したMapを作成
            val s = Map(inputs.to -> fromValue.toString)
            println("pre	" + s)
            s
          } catch {
            case e: Exception => {
              contextErrorProc(process.identity, currentOrderIdentity, "sourceOrderIdentity	" + inputs.sourceOrderIdentity + "	/from	" + inputs.from + "	/to	" + inputs.to + ":has ERROR. " + e.toString)
              Map("" -> "")
            }
          }

          //souceに指定したIdentityが含まれていない
        } catch {
          case e: Exception => {
            contextErrorProc(process.identity, currentOrderIdentity, "sourceOrderIdentity	" + inputs.sourceOrderIdentity + "	/from	" + inputs.from + "	/to	" + inputs.to + ":has ERROR. " + e.toString)
            Map("" -> "")
          }
        }
      }

      //そもそもカラッぽなら従来のコンテキストを実行
      if (context.inputsList.isEmpty) {
        eventualContext
      } else if (totalMap.isEmpty) {
        //何もしない
        Map("" -> "")
      } else {
        val reduced = totalMap.reduceLeft { (total, toAdd) => total ++ toAdd }
        eventualContext ++ reduced
      }

    } catch {
      case e: Exception => contextErrorProc(process.identity, currentOrderIdentity, "generateRuntimeContext has ERROR. " + e.toString)
    }
  }

  /**
   * 一切のエラーを赦さない、実際の実行時のコンテキスト生成。
   */
  def generateActualRuntimeContext(process: Process, currentOrderIdentity: String) = {
    //入力するkey-valueを用意する
    try {
      val initial = contextSrc.initialParam.apply(currentOrderIdentity)

      //initialに対して、currentContextに同名の値があれば上書きする
      val currentEventual = contextKeyValues.apply(currentOrderIdentity)

      //結果としてのcontext　このあとinputをくわえる。
      val eventualContext = initial ++ currentEventual

      //存在するInputsを発生させ、eventuallyProtの特定の要素をcurrentContextから上書きする
      val context = process.orderAdditional.apply(currentOrderIdentity)

      val totalMap = for (inputs <- context.inputsList) yield {
        try {
          val v = contextKeyValues.apply(inputs.sourceOrderIdentity)
          try {
            val fromValue = v.apply(inputs.from)

            try { //to 対象の有無をチェック
              contextKeyValues.apply(currentOrderIdentity).apply(inputs.to)
            } catch {
              case e: Exception => {
                contextErrorProc(process.identity, currentOrderIdentity, "sourceOrderIdentity	" + inputs.sourceOrderIdentity + "	/from	" + inputs.from + "	/to	" + inputs.to + ":has ERROR. " + e.toString)
                println("エラー1 " + process.identity + ":" + currentOrderIdentity + ":" + "sourceOrderIdentity " + inputs.sourceOrderIdentity + "  /from " + inputs.from + " /to " + inputs.to + ":has ERROR. " + e.toString)
                sys.error("エラー1")
                sys.exit(-1)
              }
            }
            //fromValueをtoに代入したMapを作成
            Map(inputs.to -> fromValue.toString)
          } catch {
            case e: Exception => {
              contextErrorProc(process.identity, currentOrderIdentity, "sourceOrderIdentity	" + inputs.sourceOrderIdentity + "	/from	" + inputs.from + "	/to	" + inputs.to + ":has ERROR. " + e.toString)
              println("エラー2 " + process.identity + ":" + currentOrderIdentity, "sourceOrderIdentity " + inputs.sourceOrderIdentity + "  /from " + inputs.from + " /to " + inputs.to + ":has ERROR. " + e.toString)
              sys.error("エラー2")
              sys.exit(-1)

              Map("" -> "")
            }
          }

          //souceに指定したIdentityが含まれていない
        } catch {
          case e: Exception => {
            contextErrorProc(process.identity, currentOrderIdentity, "sourceOrderIdentity	" + inputs.sourceOrderIdentity + "	/from	" + inputs.from + "	/to	" + inputs.to + ":has ERROR. " + e.toString)
            println("エラー3 " + process.identity + ":" + currentOrderIdentity, "sourceOrderIdentity " + inputs.sourceOrderIdentity + "  /from " + inputs.from + " /to " + inputs.to + ":has ERROR. " + e.toString)
            sys.error("エラー3")
            sys.exit(-1)
            Map("" -> "")
          }
        }
      }

      //そもそもカラッぽなら従来のコンテキストを実行
      if (context.inputsList.isEmpty) {
        eventualContext
      } else if (totalMap.isEmpty) {
        //何もしない
        Map("" -> "")
      } else {
        val reduced = totalMap.reduceLeft { (total, toAdd) => total ++ toAdd }
        eventualContext ++ reduced
      }

    } catch {
      case e: Exception => {
        contextErrorProc(process.identity, currentOrderIdentity, "generateRuntimeContext has ERROR. " + e.toString)
        println("エラー4 " + process.identity + ":" + currentOrderIdentity, "generateRuntimeContext has ERROR. " + e.toString)
        sys.error("エラー4")
        sys.exit(-1)
      }
    }
  }
  /**
   * 存在する有効なWorkerに直近のContext内でのOrderの終了通知を投げる
   */
  def notifyFinishedOrderInfoToAllWorker(finishedOrderIdentity: String, allfinishedOrderIdentities: List[String]) = {
    for (processName <- runningProcessList) {
      // println("次のprocessに通知 " + processName)
      messenger.callWithAsync(processName, WorkerMessages.MESSAGE_FINISHEDORDER_NOTIFY.toString, messenger.tagValues(
        new TagValue("finishedOrderIdentity", finishedOrderIdentity),
        new TagValue("allfinishedOrderIdentities", allfinishedOrderIdentities)))
    }

    //masterに進捗を通知
    messenger.callParent(ContextMessages.MESSAGE_PROCEEDED.toString, messenger.tagValues(
      new TagValue("contextIdentity", identity),
      new TagValue("contextOrderIndex", doneOrderIdentities.length),
      new TagValue("contextOrderTotal", contextSrc.totalOrderCount),
      new TagValue("contextProcessIndex", doneProcessList.length),
      new TagValue("contextProcessTotal", contextSrc.totalProcessNum)))
  }

  /**
   * FinallyOrderを開始する
   */
  def runFinally(finallyContext: scala.collection.mutable.Map[String, String]) = {
    println("finally開始するんだけど、、"+finallyContext)
    comments += commentFormat(new Date, "FinallyOrder:" + finallyOrderIdentity + "	setUp.")
    messenger.call(finallyProcessIdentity, WorkerMessages.MESSAGE_SETUP.toString, messenger.tagValues(
      new TagValue("identity", finallyOrderIdentity),
      new TagValue("processSplitIds", List()),
      new TagValue("afterWaitIds", List()),
      new TagValue("context", finallyContext)))

    comments += commentFormat(new Date, "FinallyOrder:" + finallyOrderIdentity + "	start.")

    messenger.call(finallyProcessIdentity, WorkerMessages.MESSAGE_START.toString, messenger.tagValues(
      new TagValue("identity", finallyOrderIdentity),
      new TagValue("processSplitIds", List()),
      new TagValue("afterWaitIds", List()),
      new TagValue("context", finallyContext)))
  }

  /**
   * Order実行時の動作
   */
  def procRunning(execSrc: String, tagValues: Array[TagValue]) = {
    ContextExecs.get(execSrc) match {
      //waiterからのdelay時にまだRunningだったら
      case ContextExecs.EXEC_TIMEOUT_RUN => {
        println("EXEC_TIMEOUT_RUNが発生  " + identity)
        ContextStatus.STATUS_TIMEOUT +=: status
        comments += commentFormat(new Date, "Timeout break out.	" + identity)

        contextKeyValues.get(finallyOrderIdentity).foreach { finallyContext => runFinally(finallyContext) }
      }
      case _ =>
    }

    WorkerMessages.get(execSrc) match {
      case WorkerMessages.MESSAGE_SYNCRONOUSLY_STARTED => {
        val workerIdentity = messenger.get("workerIdentity", tagValues).asInstanceOf[String]
        val orderIdentity = messenger.get("orderIdentity", tagValues).asInstanceOf[String]
        comments += commentFormat(new Date, "PROCESS:" + workerIdentity + "	started Order:" + orderIdentity)
      }
      case WorkerMessages.MESSAGE_ASYNCRONOUSLY_STARTED => {
        val workerIdentity = messenger.get("workerIdentity", tagValues).asInstanceOf[String]
        val orderIdentity = messenger.get("orderIdentity", tagValues).asInstanceOf[String]
        comments += commentFormat(new Date, "PROCESS:" + workerIdentity + "	started asynchronous Order:" + orderIdentity)
      }

      case WorkerMessages.MESSAGE_DONE => {
        val currentFinishedWorkerIdentity = messenger.get("identity", tagValues).asInstanceOf[String]
        val currentFinishedOrderIdentity = messenger.get("orderIdentity", tagValues).asInstanceOf[String]
        val currentFinishedEventualContext = messenger.get("eventualContext", tagValues).asInstanceOf[scala.collection.mutable.Map[String, String]]
        //				println("結果はここ	"+currentFinishedOrderIdentity + "	/currentFinishedEventualContext	"+currentFinishedEventualContext)

        //eventualを、contextKeyValuesに上書きする
        val appendedContext = (currentFinishedOrderIdentity -> currentFinishedEventualContext)
        contextKeyValues += appendedContext

        // println("currentFinishedWorkerIdentity  " + currentFinishedWorkerIdentity + " /currentFinishedOrderIdentity  " + currentFinishedOrderIdentity + "  /currentFinishedEventualContext " + currentFinishedEventualContext + " /contextKeyValues " + contextKeyValues)
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
      case WorkerMessages.MESSAGE_REQUEST => {

        val processIdentity = messenger.get("workerIdentity", tagValues).asInstanceOf[String]
        val finishedOrderIdentity = messenger.get("finishedOrderIdentity", tagValues).asInstanceOf[String]

        println("requestまでは来た processIdentity " + processIdentity + " /finishedOrderIdentity  " + finishedOrderIdentity)

        //終了したOrderの属するProcessを検索、次のOrderを探す
        contextSrc.current.processList.withFilter(_.identity.equals(processIdentity)).foreach { process =>
          //終了したOrderのIndexを出し、次があれば実行する。
          val currentOrderIdentityIndex = (process.orderIdentityList.indexOf(finishedOrderIdentity) + 1)
          // println("process.orderIdentityList	" + process.orderIdentityList)
          // println("index	" + process.orderIdentityList.indexOf(finishedOrderIdentity))
          // println("finishedOrderIdentity	" + process.orderIdentityList.indexOf(finishedOrderIdentity))

          comments += commentFormat(new Date, "PROCESS:" + processIdentity + "	requested the next Order of	" + finishedOrderIdentity)

          currentOrderIdentityIndex match {
            //次のOrderを実行
            case number if (number < process.orderIdentityList.length) => {
              println("次のOrderを実行へ  " + process + " /num  " + number)
              dispachNextOrderToWorker(process, number)
            }

            //先ほど完了したのがこのProcessのラスト
            case last => {
              //runningProcessListリストからdoneProcessListへとprocessを移す
              println("この時点で	runningProcessList	"+runningProcessList+"	/から、	"+processIdentity+"	/が引かれる")
              runningProcessList -= processIdentity
              doneProcessList += processIdentity

              //Workerを停める
              messenger.callWithAsync(processIdentity, WorkerMessages.MESSAGE_OVER.toString, null)

              println("このプロセスが完了したので引かれた後  "+processIdentity + " /この時点でリストは	"+runningProcessList)
              comments += commentFormat(new Date, "PROCESS:" + processIdentity + "	All Orders done!")

              //runningProcessListが空になったらfinallyを実行
              if (runningProcessList.isEmpty) {
                ContextStatus.STATUS_FINALLY +=: status
                comments += commentFormat(new Date, "FinallyOrder:" + finallyOrderIdentity + "	ready.")

                messenger.callMyselfWithAsync(WorkerMessages.MESSAGE_FINALLY.toString, null)
              }
            }
          }
        }
      }

      /**
       * 特定のOrderIdentityのリクエスト
       */
      case WorkerMessages.MESSAGE_REQUEST_SPECIFY => {
        val processIdentity = messenger.get("workerIdentity", tagValues).asInstanceOf[String]
        contextSrc.current.processList.withFilter(_.identity.equals(processIdentity)).foreach { process =>
          dispachNextOrderToWorker(process, 0)
        }
      }

      /*
			 * Orderからのエラー
			 */
      case WorkerMessages.MESSAGE_ERROR => {
        ContextStatus.STATUS_ERROR +=: status

        val erroredWorkerIdentity = messenger.get("erroredWorkerIdentity", tagValues).asInstanceOf[String]
        val erroredOrderIdentity = messenger.get("erroredOrderIdentity", tagValues).asInstanceOf[String]
        val eventualContext = messenger.get("eventualContext", tagValues).asInstanceOf[scala.collection.mutable.Map[String, String]]

        contextErrorProc(erroredWorkerIdentity, erroredOrderIdentity, eventualContext.apply(OrderPrefix._result.toString))
      }

      case _ =>
    }
  }

  /**
   * Timeout時の動作
   *
   * 現段階のContextでのfinallyが実行される
   */
  def procTimeout(exec: WorkerMessages.Value, tagValues: Array[TagValue]) = {
    exec match {
      case WorkerMessages.MESSAGE_SYNCRONOUSLY_STARTED =>
      case WorkerMessages.MESSAGE_ASYNCRONOUSLY_STARTED =>

      case WorkerMessages.MESSAGE_DONE => {

        val currentFinishedWorkerIdentity = messenger.get("identity", tagValues).asInstanceOf[String]
        val currentFinishedOrderIdentity = messenger.get("orderIdentity", tagValues).asInstanceOf[String]
        val currentFinishedEventualContext = messenger.get("eventualContext", tagValues).asInstanceOf[scala.collection.mutable.Map[String, String]]

        if (currentFinishedOrderIdentity.equals(finallyOrderIdentity) && currentFinishedWorkerIdentity.equals(finallyProcessIdentity)) {

          val appendedContext = (currentFinishedOrderIdentity -> currentFinishedEventualContext)

          contextKeyValues += appendedContext
          ContextStatus.STATUS_TIMEOUTED +=: status
          comments += commentFormat(new Date, "Timeout Order:" + currentFinishedOrderIdentity + "	was finished.	" + identity)


          //masterにタイムアウトを通知
          messenger.callParent(ContextMessages.MESSAGE_TIMEOUT.toString, messenger.tagValues(
            new TagValue("contextIdentity", identity),
            new TagValue("contextOrderIndex", doneOrderIdentities.length),
            new TagValue("contextOrderTotal", contextSrc.totalOrderCount),
            new TagValue("contextProcessIndex", doneProcessList.length),
            new TagValue("contextProcessTotal", contextSrc.totalProcessNum),
            new TagValue("contextResult", currentContextResult)))
        }
      }

      case _ => println("nothing todo in STATUS_TIMEOUT	-exec	" + exec + "	/	" + identity)
    }
  }

  /**
   * Finallyが通常実行された際の動作
   */
  def procFinally(exec: WorkerMessages.Value, tagValues: Array[TagValue]) = {
    exec match {
      case WorkerMessages.MESSAGE_FINALLY => contextKeyValues.get(finallyOrderIdentity).foreach {finallyContext => 
        println("Finallyの実行が始まった  "+finallyOrderIdentity)
        runFinally(finallyContext)
        println("finallyContextが実行開始されたはず "+ finallyContext)
      }

      case WorkerMessages.MESSAGE_SYNCRONOUSLY_STARTED => {
        val orderIdentity = messenger.get("orderIdentity", tagValues).asInstanceOf[String]
        comments += commentFormat(new Date, "FinallyOrder:" + orderIdentity + "	started.")
      }
      case WorkerMessages.MESSAGE_ASYNCRONOUSLY_STARTED => {
        val orderIdentity = messenger.get("orderIdentity", tagValues).asInstanceOf[String]
        comments += commentFormat(new Date, "FinallyOrder:	" + orderIdentity + "	started asynchronously")
      }

      case WorkerMessages.MESSAGE_DONE => {
        val currentFinishedWorkerIdentity = messenger.get("identity", tagValues).asInstanceOf[String]
        val currentFinishedOrderIdentity = messenger.get("orderIdentity", tagValues).asInstanceOf[String]
        val currentFinishedEventualContext = messenger.get("eventualContext", tagValues).asInstanceOf[scala.collection.mutable.Map[String, String]]

        if (currentFinishedOrderIdentity.equals(finallyOrderIdentity) && currentFinishedWorkerIdentity.equals(finallyProcessIdentity)) {
          //eventualを、contextKeyValuesに上書きする
          val appendedContext = (currentFinishedOrderIdentity -> currentFinishedEventualContext)
          contextKeyValues += appendedContext

          //動作中リストから除外
          doingOrderIdentities -= currentFinishedOrderIdentity
          //動作済みリストに追記
          doneOrderIdentities += currentFinishedOrderIdentity

          //最終結果をcurrentContextResultに上書き
          println("before	" + contextKeyValues)
          contextKeyValues ++ currentFinishedEventualContext
          println("after	" + contextKeyValues)

          ContextStatus.STATUS_DONE +=: status
          comments += commentFormat(new Date, "finished!")

          val contextResultString = contextResultToString(currentContextResult)

          //masterに完了を通知
          messenger.callParent(ContextMessages.MESSAGE_DONE.toString, messenger.tagValues(
            new TagValue("contextIdentity", identity),
            new TagValue("contextOrderIndex", doneOrderIdentities.length),
            new TagValue("contextOrderTotal", contextSrc.totalOrderCount),
            new TagValue("contextProcessIndex", doneProcessList.length),
            new TagValue("contextProcessTotal", contextSrc.totalProcessNum),
            new TagValue("contextResultString", contextResultString)))
        }
      }

      case _ => println("nothing todo in STATUS_FINALLY	-exec	" + exec)
    }
  }

  def contextResultToString(contextResult: ContextResult) = {
    "testResult"
  }

}