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
class MondogrossoContextController extends MessengerProtocol {
	val controllerUuid = UUID.randomUUID().toString

	val messenger = new Messenger(this, controllerUuid)
	def name = messenger.getName

	/*
	 * ステータス
	 */
	val statusHistory : ListBuffer[ContextContStatus.Value] = ListBuffer(ContextContStatus.STATUS_EMPTY)
	def currentStatus = statusHistory.head

	
	//実行前/中のContext
	val activeContexts : ListBuffer[MondogrossoProcessContext] = ListBuffer()

	//実行済みのContext
	val doneContexts : ListBuffer[MondogrossoProcessContext] = ListBuffer()
	
	//投入されたOrderの名称とcontextIdentityをヒモづけるマップ
	val processNameToContextIdentityMap : ListBuffer[Map[String, String]] = ListBuffer()
	
	def receiver(exec : String, tagValues : Array[TagValue]) = {
		println("MondogrossoProcessOrdersController	exec	" + exec)

		currentStatus match {
			case ContextContStatus.STATUS_EMPTY => println("no context running")
			case ContextContStatus.STATUS_RUNNING => {
				ContextMessages.get(exec) match {
					case ContextMessages.MESSAGE_READY => {
						println("MESSAGE_READYを受け取った	")
						tagValues.foreach(tagValue => println(tagValue))
					}
					case ContextMessages.MESSAGE_START => {
						println("MESSAGE_STARTを受け取った	")
						tagValues.foreach(tagValue => println(tagValue))
					}

					case ContextMessages.MESSAGE_PROCEEDED => {
						println("MESSAGE_PROCEEDED受け取った")
						tagValues.foreach(tagValue => println(tagValue))
					}

					case ContextMessages.MESSAGE_TIMEOUT => {
						println("MESSAGE_TIMEOUT受け取った")
						tagValues.foreach(tagValue => println(tagValue))
					}
					case ContextMessages.MESSAGE_ERROR => {
						println("MESSAGE_ERROR受け取った")
						tagValues.foreach(tagValue => println(tagValue))
					}
					case ContextMessages.MESSAGE_DONE => procDone(tagValues)
					case other =>
				}
			}
			case other => println("othe	" + other)
		}
	}
	
	/**
	 * コンテキスト完了時の処理
	 */
	def procDone(tagValues : Array[TagValue]) = {
		/*
		 * new TagValue("contextIdentity", identity),
							new TagValue("contextResult", currentContextResult)))
		 */
		val contextIdentity = messenger.get("contextIdentity", tagValues).asInstanceOf[String]
		val contextResultString = messenger.get("contextResultString", tagValues).asInstanceOf[String]
		
//		val contextOrderIndex = messenger.get("contextOrderIndex", tagValues).asInstanceOf[Int]
//		val contextOrderTotal = messenger.get("contextOrderTotal", tagValues).asInstanceOf[Int]
//		val contextProcessIndex = messenger.get("contextProcessIndex", tagValues).asInstanceOf[Int]
//		val contextProcessTotal = messenger.get("contextProcessTotal", tagValues).asInstanceOf[Int]
		
		val currentContext = activeContexts.filter(_.identity.equals(contextIdentity))
		
		println("currentContext	"+currentContext(0))
		activeContexts -= currentContext(0)
		doneContexts += currentContext(0)
		
		activeContexts.isEmpty match {
			case true => {
				ContextContStatus.STATUS_EMPTY +=: statusHistory
			}
			case false => {
				//続く
			}
		} 
	}
	
	
	/**
	 * 新しいProcessOrdersをアタッチする
	 */
	def attachProcessOrders(identity : String, contextSrc : ContextSource) = {
		//使用者側が与えたIdentityとは別に、システムがIdentityを振る。
		val processIdentity = UUID.randomUUID.toString

		val context = new MondogrossoProcessContext(processIdentity, contextSrc, controllerUuid)
		processNameToContextIdentityMap ++ Map(identity -> processIdentity)
		
		println("processNameToContextIdentityMap	"+processNameToContextIdentityMap)
		
		activeContexts += context
	}

	/**
	 * 特定の状態のコンテキストのID一覧を取得する
	 */
	def contextCountOfStatus(contextStatusString : String) = {
		val totalSourceContexts = activeContexts.filter(_.currentStatus.equals(ContextStatus.get(contextStatusString))) ++ doneContexts.filter(_.currentStatus.equals(ContextStatus.get(contextStatusString)))
		for (context <- totalSourceContexts) yield context.identity
	}

	/**
	 * 準備済みのコンテキストを実行する
	 */
	def runAllContext = {
		ContextContStatus.STATUS_RUNNING +=: statusHistory
		activeContexts.withFilter(_.currentStatus.equals(ContextStatus.STATUS_READY)).foreach { context => 
			context.runContext
		}
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
	def currentResultsOfContext(contextId : String) = {
		
		val totalSourceContexts = activeContexts.filter(_.identity.equals(contextId)) ++ doneContexts.filter(_.identity.equals(contextId))
		
		for (targetContext <- totalSourceContexts) yield { getContextResult(targetContext) }
	}

	/**
	 * 特定のコンテキストの結果を得る
	 */
	def getContextResult(targetContext : MondogrossoProcessContext) = {
		println("<------------")
		println(targetContext.currentContextResult.contextIdentity)
		println(targetContext.currentContextResult)
		println("------------>")

		targetContext.currentContextResult
	}
}