package com.kissaki.mondogrosso.mondogrossoProcessOrders
import scala.collection.mutable.ListBuffer
import com.kissaki.Messenger
import com.kissaki.TagValue
import com.kissaki.MessengerProtocol
import com.kissaki.Messenger
import java.util.UUID
import scala.collection.mutable.MapBuilder
import scala.sys.process.{ Process ⇒ scalaProcess }

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
		println("attach	Context" + contextSrc)
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
class ProcessContext(processIdentity : String, contextSrc : ContextSource) extends MessengerProtocol {
	val messenger = new Messenger(this, processIdentity)

	val DEFAULT_INDEX = 0


	//Processの名簿
	val processList : ListBuffer[String] = ListBuffer()

	//現在実行しているIndex(全体を並べたときをもとにした順、実行順とは異なり、個数)
	var currentOrderIndex = DEFAULT_INDEX

	//実行中のOrderのList
	val currentExecutingOrders : ListBuffer[String] = ListBuffer()

	//初期コンテキスト
	val initialContext = contextSrc.initialParam
	
	//コンテキスト Map(identity:String -> Map(key:String -> value:String)) 実行後の結果入力で上書きされていく。
	val currentContext = contextSrc.initialParam
	
	/**
	 * 現在のコンテキストのIdを返す
	 */
	def identity = processIdentity

	def processNum = contextSrc.totalProcessNum

	def totalOrderNum = contextSrc.totalOrderCount

	val finallyOrder = contextSrc.finallyOrder

	/**
	 * このコンテキストの現在のindexからの実行開始
	 */
	def runContext = {
		println("コンテキスト	" + processIdentity)

		println("currentOrderIndex	" + currentOrderIndex)

		if (currentOrderIndex == DEFAULT_INDEX) { //新しいプロセスが生まれるタイミング

			//新しいProcessを登録する
			val newProcessName = UUID.randomUUID().toString()
			processList += newProcessName

			//Workerを作成
			new ProcessWorker(newProcessName, processIdentity)

			//次に開始すべきIdentityを取得する
			val currentOrderIdentity = contextSrc.current.processList(0).orderIdentityList(currentOrderIndex)

			//入力するkey-valueを用意する
			val initial = contextSrc.initialParam.get(currentOrderIdentity) match {
				case Some(v) => v
				case None => Map(""->"")//エラー
			}
			
			val kind = initial(OrderPrefix._kind.toString)
			val main = initial(OrderPrefix._main.toString)
			
			//initialに対して、currentContextに同名の値があれば上書きする
			val currentEventual = currentContext.get(currentOrderIdentity) match {
				case Some(v) => v
				case None => Map()
			}
			
			val actual = initial ++ currentEventual
			println("起動1	" + actual)
			
			messenger.call(newProcessName, Messages.MESSAGE_START.toString, messenger.tagValues(
				new TagValue("identity", currentOrderIdentity),
				new TagValue("context", actual)))

			//実行中のOrderをセット
			currentExecutingOrders += currentOrderIdentity
		}

	}

	/**
	 * レシーバ
	 */
	def receiver(execSrc : String, tagValues : Array[TagValue]) = {

		Messages.get(execSrc) match {
			case Messages.MESSAGE_STARTED => {
				println("currentOrderIndex	started	" + currentOrderIndex + "	/tagValues	" + tagValues)
			}
			
			case Messages.MESSAGE_DONE => {
				val currentFinishedWorkerIdentity = messenger.get("identity", tagValues).asInstanceOf[String]
				val currentFinishedOrderIdentity = messenger.get("orderIdentity", tagValues).asInstanceOf[String]
				val currentFinishedEventualContext = messenger.get("eventualContext", tagValues).asInstanceOf[scala.collection.mutable.Map[String,String]]
				
				println("currentFinishedWorkerIdentity	"+currentFinishedWorkerIdentity)
				println("currentFinishedOrderIdentity	"+currentFinishedOrderIdentity)
				println("currentFinishedEventualContext	"+currentFinishedEventualContext)
				
				//eventualを、currentContextに上書きする
				println("b	currentContext	"+currentContext)
				val appendedContext = (currentFinishedOrderIdentity -> currentFinishedEventualContext)
				currentContext += appendedContext
				println("a	currentContext	"+currentContext)
			}

			case other => {
				println("未整理のメッセージ	" + other)
			}
		}
	}
}

case class WorkInformation(orderIdentity : String, localContext : scala.collection.Map[String,String])
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

	
	def getLatestWorkInformation = {
		currentWorkInformationHistory.head
	}
	
	//状態
	var currentStatus = WorkerStatus.STATUS_READY
	
	/**
	 * レシーバ
	 */
	def receiver(execSrc : String, tagValues : Array[TagValue]) = {
		Messages.get(execSrc) match {
			case Messages.MESSAGE_START => {
				println("tagValues	" + tagValues)

				val orderIdentity = (messenger.get("identity", tagValues)).asInstanceOf[String]
				val orderContext = (messenger.get("context", tagValues)).asInstanceOf[scala.collection.Map[String,String]]
				
				val currentAddedOrderInfo = new WorkInformation(orderIdentity, orderContext)

				//currentWorkInformationHistoryのheadに、最新のInfoを加算する
				currentAddedOrderInfo +=: currentWorkInformationHistory
				
				
				//動作開始の通知
				messenger.callParent(Messages.MESSAGE_STARTED.toString,
					messenger.tagValues(new TagValue("workerIdentity", identity),
						new TagValue("orderIdentity", orderIdentity)))
				
				
				//非同期	/	同期
				orderContext.contains(OrderPrefix.__trigger.toString) match {
					case true => {
						runWithAsync(currentAddedOrderInfo)
					}
					case false => {
						runWithSync(currentAddedOrderInfo)
					}
				}
			}
			case other => {
				println("未整理の何か	" + other)
			}
		}
	}

	/**
	 * 非同期実行
	 */
	def runWithAsync (info : WorkInformation) = {
		//開始
		currentStatus = WorkerStatus.STATUS_DOING
		
		//予約語以外
		val currentKeys = OrderPrefix.getXORWithKeyword(info.localContext.keys.toSet)
		
		info.localContext(OrderPrefix._kind.toString) match {
			case WorkKinds.kindJar => {
				val inputKeyValuePair = for(key <- currentKeys) yield key + " " + info.localContext.get(key)
				println("非同期Java	ここに来てる")
			}
			case WorkKinds.kindProcess => {
				val inputKeyValuePair = for(key <- currentKeys) yield key + " " + info.localContext.get(key)
				
				val reduced = inputKeyValuePair match {
					case v if (v.isEmpty) => {
						""
					}
					case default => {
						inputKeyValuePair.reduceLeft {
							(total, add) => total + " " + add 
						}
					}
				}
				
				val stableCommand = info.localContext(OrderPrefix._main.toString) + " " + reduced
				
				println("stableCommand	"+stableCommand)
				
				//run
				val process = scalaProcess(stableCommand)
				
				val result = process !!
				
				
			}
			case other => {
				println("type not found	:"+other)
			}
		}
		//監視を続ける
	}
	
	/**
	 * 同期実行
	 */
	def runWithSync(info : WorkInformation) = {
		//開始
		currentStatus = WorkerStatus.STATUS_DOING
		
		//予約語以外
		val currentKeys = OrderPrefix.getXORWithKeyword(info.localContext.keys.toSet)
		
		info.localContext(OrderPrefix._kind.toString) match {
			case WorkKinds.kindJar => {
				val inputKeyValuePair = for(key <- currentKeys) yield {
					val value = info.localContext.get(key) match {
						case Some(v) => {
							v
						}
						case None => {
							""
						}
					}
					key + " " + value
				}
				
				val reduced = inputKeyValuePair match {
					case v if (v.isEmpty) => {
						""
					}
					case default => {
						println("kok	"+default)
						inputKeyValuePair.reduceLeft {
							(total, add) => total + " " + add 
						}
					}
				}
				
				val stableCommand = "java -jar ./" + info.localContext(OrderPrefix._main.toString) + ".jar " + reduced
				
				//run
				val process = scalaProcess(stableCommand)
				
				val result = process !!
				
				println("result	"+result)
				//結果のコンテキストを作成する
				val eventualContext = info.localContext ++ Map(OrderPrefix._result.toString -> result) 
				
				//結果を残す
				new WorkInformation(info.orderIdentity, eventualContext) +=: currentWorkInformationHistory 
				
				//親に終了を通知
				messenger.callParent(Messages.MESSAGE_DONE.toString,
					messenger.tagValues(
							new TagValue("identity", identity),
							new TagValue("orderIdentity", info.orderIdentity),
							new TagValue("eventualContext", eventualContext))
							)
			}
			case WorkKinds.kindProcess => {
				val inputKeyValuePair = for(key <- currentKeys) yield {
					val value = info.localContext.get(key) match {
						case Some(v) => {
							v
						}
						case None => {
							""
						}
					}
					key + " " + value
				}
				
				val reduced = inputKeyValuePair match {
					case v if (v.isEmpty) => {
						""
					}
					case default => {
						inputKeyValuePair.reduceLeft {
							(total, add) => total + " " + add 
						}
					}
				}
				
				val stableCommand = info.localContext(OrderPrefix._main.toString) + " " + reduced
				
				println("shell sync stableCommand	"+stableCommand)
				
				//run
				val process = scalaProcess(stableCommand)
				
				val result = process !!
				
				//結果のコンテキストを作成する
				val eventualContext = info.localContext ++ Map(OrderPrefix._result.toString -> result) 
				
				//結果を残す
				new WorkInformation(info.orderIdentity, eventualContext) +=: currentWorkInformationHistory 
				
				//親に終了を通知
				messenger.callParent(Messages.MESSAGE_DONE.toString,
					messenger.tagValues(
							new TagValue("identity", identity),
							new TagValue("orderIdentity", info.orderIdentity),
							new TagValue("eventualContext", eventualContext))
							)
			}
			case other => {
				println("type not found	:"+other)
			}
		}
		
		currentStatus = WorkerStatus.STATUS_DONE			
	}
}
	
