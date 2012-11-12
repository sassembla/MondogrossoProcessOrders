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
    currentStatus match {
      case ContextContStatus.STATUS_EMPTY => println("no context runningに来た、exec  "+exec)
      case ContextContStatus.STATUS_RUNNING => {
        ContextMessages.get(exec) match {
          case ContextMessages.MESSAGE_READY => {
          	val s = new StringBuffer
            tagValues.foreach(contents => s.append(contents))
            println("MESSAGE_READYを受け取った  "+s)
            
            report(ProcessOrdersMasterMessages.MESSAGE_READY, tagValues)
            
          }
          case ContextMessages.MESSAGE_START => {
            val s = new StringBuffer
            tagValues.foreach(contents => s.append(contents))
            val identity = messenger.get("contextIdentity", tagValues).asInstanceOf[String]
            println("ContextControllerがMESSAGE_STARTを受け取った、これはcontextのidentity  "+identity)
            
            report(ProcessOrdersMasterMessages.MESSAGE_START, tagValues)
          }

          case ContextMessages.MESSAGE_PROCEEDED => {
          	val s = new StringBuffer
            tagValues.foreach(contents => s.append(contents))
            val identity = messenger.get("contextIdentity", tagValues).asInstanceOf[String]
            println("ContextControllerがMESSAGE_READYを受け取った、これはcontextのidentity  "+identity)
            
            report(ProcessOrdersMasterMessages.MESSAGE_PROCEEDED, tagValues)
          }

          case ContextMessages.MESSAGE_TIMEOUT => {
          	val s = new StringBuffer
            tagValues.foreach(contents => s.append(contents))
            val identity = messenger.get("contextIdentity", tagValues).asInstanceOf[String]
            println("ContextControllerがMESSAGE_TIMEOUTを受け取った、これはcontextのidentity  "+identity)
            
          	val contextIdentity = messenger.get("contextIdentity", tagValues).asInstanceOf[String]
    		    report(ProcessOrdersMasterMessages.MESSAGE_TIMEOUTED, tagValues)
            contextOvered(contextIdentity)
          }

          case ContextMessages.MESSAGE_ERROR => {
          	val s = new StringBuffer
            tagValues.foreach(contents => s.append(contents))
            val identity = messenger.get("contextIdentity", tagValues).asInstanceOf[String]
            println("ContextControllerがMESSAGE_ERRORを受け取った、これはcontextのidentity  "+identity)
            
          	val contextIdentity = messenger.get("contextIdentity", tagValues).asInstanceOf[String]
            report(ProcessOrdersMasterMessages.MESSAGE_ERROR, tagValues)
            contextOvered(contextIdentity)
          }

          case ContextMessages.MESSAGE_DONE => {
          	val s = new StringBuffer
            tagValues.foreach(contents => s.append(contents))
            val identity = messenger.get("contextIdentity", tagValues).asInstanceOf[String]
            println("ContextControllerがMESSAGE_DONEを受け取った  "+identity)
            
            val contextIdentity = messenger.get("contextIdentity", tagValues).asInstanceOf[String]
            report(ProcessOrdersMasterMessages.MESSAGE_DONE, tagValues)
            contextOvered(contextIdentity)
          }

          case other =>
        }
      }
      case other => println("other	" + other)
    }
  }
  
  /**
  interfaceへとレポートを吐き出す
  */
  def report (message:ProcessOrdersMasterMessages.Value, tagValues: Array[TagValue]) = {    
    val contextIdentity = messenger.get("contextIdentity", tagValues).asInstanceOf[String]

    // println("currentMap = "+processNameToContextIdentityMap)
    // println("applied  "+processNameToContextIdentityMap.apply(contextIdentity))

    processNameToContextIdentityMap.get(contextIdentity) match {
      case Some(v) => 
      case None => sys.error("含まれていない "+processNameToContextIdentityMap + " に、contextIdentity "+contextIdentity)
    }
    
    println("100件が踏みやすいバグ、inside")
    //含まれてないケースがあるみたいね。

    //tagValuesの値に、processNameToContextIdentityMapを足す
    val newTagValues = tagValues ++ Array(new TagValue("userDefinedIdentity", processNameToContextIdentityMap.apply(contextIdentity)))
    
    newTagValues.foreach(tagVal => println("このへんかなーーうーーむtagVal  "+tagVal))

    messenger.callParent(message.toString, newTagValues)
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
            messenger.callParent(ProcessOrdersMasterMessages.MESSAGE_CONTEXT_OVER.toString, null)
          }
          case false => {
            //続く
          }
        }
      }
      case true => {
        println("空っぽっていうのはおかしい	、このidentityがactiveContextsに含まれてない	" + overedContextIdentity)
        
        sys.error("空っぽっていうのはおかしい  、このidentityがactiveContextsに含まれてない " + overedContextIdentity)
        sys.exit(-1)

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

    
    // println("processIdentity/identity  "+processIdentity +"  as "+identity)

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