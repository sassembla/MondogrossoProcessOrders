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
import java.util.Timer
import java.util.TimerTask
import java.util.concurrent.TimeUnit

/**
 * コンテキストのコントローラ
 *
 * 各コンテキストの終了を見る
 */
class MondogrossoContextController (masterName : String) extends MessengerProtocol {
  val controllerUuid = UUID.randomUUID().toString
  
  val messenger = new Messenger(this, controllerUuid)
  def name = messenger.getName

  messenger.inputParent(masterName)

  /*
	 * ステータス
	 */
  val statusHistory: ListBuffer[ContextContStatus.Value] = ListBuffer(ContextContStatus.STATUS_EMPTY)
  def currentStatus = statusHistory.head

  //実行前/中のContext
  val activeContexts: ListBuffer[MondogrossoProcessContext] = ListBuffer()

  //実行済みのContext
  val doneContexts: ListBuffer[MondogrossoProcessContext] = ListBuffer()
  
  //投入されたOrderの名称とcontextIdentityをヒモづけるマップ
  val processNameToContextIdentityMap: scala.collection.mutable.Map[String, String] = scala.collection.mutable.Map()

  
  /**
   * レシーバ
   */
  def receiver(exec: String, tagValues: Array[TagValue]) = {
    println("MondogrossoProcessOrdersController	exec	" + exec)

    currentStatus match {
      case ContextContStatus.STATUS_EMPTY => println("no context running")
      case ContextContStatus.STATUS_RUNNING => {
        ContextMessages.get(exec) match {
          case ContextMessages.MESSAGE_READY => {
            println("MESSAGE_READYを受け取った	")
            report(ProcessOrdersMasterMessages.MESSAGE_READY, tagValues)
            
          }
          case ContextMessages.MESSAGE_START => {
            println("MESSAGE_STARTを受け取った	")
            report(ProcessOrdersMasterMessages.MESSAGE_START, tagValues)
          }

          case ContextMessages.MESSAGE_PROCEEDED => {
            println("MESSAGE_PROCEEDED受け取った")
            report(ProcessOrdersMasterMessages.MESSAGE_PROCEEDED, tagValues)
          }

          case ContextMessages.MESSAGE_TIMEOUT => {
          	val contextIdentity = messenger.get("contextIdentity", tagValues).asInstanceOf[String]
    		    contextOvered(contextIdentity)
    		    messenger.callParent(ProcessOrdersMasterMessages.MESSAGE_TIMEOUTED.toString, tagValues)
          }

          case ContextMessages.MESSAGE_ERROR => {
          	val contextIdentity = messenger.get("contextIdentity", tagValues).asInstanceOf[String]
            contextOvered(contextIdentity)
            messenger.callParent(ProcessOrdersMasterMessages.MESSAGE_ERROR.toString, tagValues)
          }

          case ContextMessages.MESSAGE_DONE => {
            val contextIdentity = messenger.get("contextIdentity", tagValues).asInstanceOf[String]
            contextOvered(contextIdentity)
            messenger.callParent(ProcessOrdersMasterMessages.MESSAGE_DONE.toString, tagValues)
          }
          
          case other =>
        }
      }
      case other => println("othe	" + other)
    }
  }
  
  /**
  mainが提供した親へとレポートを吐き出す
  */
  def report (message:ProcessOrdersMasterMessages.Value, tagValues: Array[TagValue]) = {
  	//ここで、インデックスなどを整形する。
    val contextIdentity = messenger.get("contextIdentity",tagValues).asInstanceOf[String]
		val contextOrderIndex = messenger.get("contextOrderIndex", tagValues).asInstanceOf[Int]
		val contextOrderTotal = messenger.get("contextOrderTotal", tagValues).asInstanceOf[Int]
		val contextProcessIndex = messenger.get("contextProcessIndex", tagValues).asInstanceOf[Int]
		val contextProcessTotal = messenger.get("contextProcessTotal", tagValues).asInstanceOf[Int]
    
    println("contextCont received contextIdentity  "+contextIdentity)
    println("currentMap = "+processNameToContextIdentityMap)
    //100件が踏みやすいバグがあるみたいなので、ここでは一時的に「ファイルに情報を吐く」ことに集中する。
    //含まれてないケースがあるみたいね。

    messenger.callParent(message.toString, messenger.tagValues(
    	new TagValue("contextOrderIndex",contextOrderIndex),
    	new TagValue("contextOrderTotal",contextOrderTotal),
    	new TagValue("contextProcessIndex",contextProcessIndex),
    	new TagValue("contextProcessTotal",contextProcessTotal),
    	new TagValue("processNameToContextIdentityMap",processNameToContextIdentityMap),
    	new TagValue("contextIdentity",contextIdentity)
    ))
  }


  /**
   * コンテキストが終了した事を受け取り、ProcessOrders全体のステータスを管理する
   */
  def contextOvered(overedContextIdentity: String) = {
    val currentContext = activeContexts.filter(_.identity.equals(overedContextIdentity))
    println("contextOvered  currentContext "+currentContext + " /overedContextIdentity  " +overedContextIdentity )
    currentContext.isEmpty match {
      case false => {
        activeContexts -= currentContext.head
        doneContexts += currentContext.head

        activeContexts.isEmpty match {
          case true => {
            ContextContStatus.STATUS_EMPTY +=: statusHistory
          }
          case false => {
            //続く
          }
        }
      }
      case true => {
        println("空っぽっていうのはおかしい	、このidentityがactiveContextsに含まれてない	" + overedContextIdentity)
        
        sys.error("空っぽっていうのはおかしい  、このidentityがactiveContextsに含まれてない " + overedContextIdentity)
        sys.exit(1)

      }
    }

  }

  /**
   * 新しいProcessOrdersをアタッチする
   */
  def attachProcessOrders(identity: String, contextSrc: ContextSource) = {
    //使用者側が与えたIdentityとは別に、システムがIdentityを振る。
    val processIdentity = UUID.randomUUID.toString

    val context = new MondogrossoProcessContext(processIdentity, contextSrc, controllerUuid)

    //生成したprocessIdentity と 命名されたidentityをペアにする
    processNameToContextIdentityMap += processIdentity -> identity

    //コンテキスト自体をアクティブなコンテキストの集合へ加算
    activeContexts += context

    //attachしたContextのidentityから、ユーザー命名のidentityを取り出す
    getContextUserDefinedIdentityListFromContextIdentities(ListBuffer(processIdentity))
  }

  /**
   * ContextIdentityのリストからユーザー定義Identityのリストを返す
   */
  def getContextUserDefinedIdentityListFromContextIdentities(contextIdentities: scala.collection.mutable.ListBuffer[String]) = (for (identity <- contextIdentities) yield processNameToContextIdentityMap.apply(identity)).toList

  /**
   * 特定の状態のコンテキストのID一覧を取得する
   */
  def contextCountOfStatus(contextStatusString: String) = {
    val totalSourceContexts = activeContexts.filter(_.currentStatus.equals(ContextStatus.get(contextStatusString))) ++ doneContexts.filter(_.currentStatus.equals(ContextStatus.get(contextStatusString)))
    val contextIdentities = for (context <- totalSourceContexts) yield context.identity

    //Contextのidentityから、ユーザー命名のidentityを取り出す
    getContextUserDefinedIdentityListFromContextIdentities(contextIdentities)
  }

  /**
   * 準備済みのコンテキストを実行、開始したcontextのユーザー指定idを返す
   */
  def runAllContext = {
    val startCandidates = activeContexts.filter(_.currentStatus.equals(ContextStatus.STATUS_READY))
    val startedContextIdentities = for (context <- startCandidates) yield {
      ContextContStatus.STATUS_RUNNING +=: statusHistory
      context.runContext
    }

    //開始したContextのidentityから、ユーザー命名のidentityを取り出す
    getContextUserDefinedIdentityListFromContextIdentities(startedContextIdentities)
  }

  /**
   * 現時点での全コンテキストの結果を取得する
   */
  def currentResults = {
    val running = for (context <- activeContexts) yield getContextResult(context)

    val done = for (context <- doneContexts) yield getContextResult(context)

    running ++ done
  }

  /**
   * 現時点での特定のコンテキストの結果を得る
   */
  def currentResultsOfContext(userDefinedContextId: String) = {
    val candidateKeys = processNameToContextIdentityMap.filter(_._2.equals(userDefinedContextId))
    val contextIds = candidateKeys.keys.toList
    val totalSourceContexts = for (contextId <- contextIds) yield {
      doneContexts.filter(_.identity.equals(contextId))(0)
    }

    for (targetContext <- totalSourceContexts) yield getContextResult(targetContext)
  }

  /**
   * 特定のコンテキストの結果を得る
   * ユーザー指定のIdentity @ contextIdentity
   */
  def getContextResult(targetContext: MondogrossoProcessContext) = {
    // println("<------------")
    // println(processNameToContextIdentityMap.apply(targetContext.currentContextResult.contextIdentity) + "@" + targetContext.currentContextResult.contextIdentity)
    // println(targetContext.currentContextResult)
    // println("------------>")

    targetContext.currentContextResult
  }
}