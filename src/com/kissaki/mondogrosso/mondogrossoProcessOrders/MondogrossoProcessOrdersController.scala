package com.kissaki.mondogrosso.mondogrossoProcessOrders
import scala.collection.mutable.ListBuffer
import com.kissaki.Messenger
import com.kissaki.TagValue
import com.kissaki.MessengerProtocol
import com.kissaki.Messenger
import java.util.UUID
import scala.collection.mutable.MapBuilder

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
			
			val kind = initial(OrderPrefix.prefixKind)
			val main = initial(OrderPrefix.prefixMain)
			
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
				println("done, by	"+tagValues)
//				//前回のプロセス終了時点でeventualがあれば、currentContextに上書きする
//				currentContext ++ eventual
			}

			case other => {
				println("未整理のメッセージ	" + other)
			}
		}
	}
}

case class WorkInformation(orderIdentity : String, context : scala.collection.Map[String,String])
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
				println("orderIdentity	"+orderIdentity)
				
				val orderContext = (messenger.get("context", tagValues)).asInstanceOf[scala.collection.Map[String,String]]
//				match {
//					case Some(v) => {
//						println("v	"+v)
//						Map("" -> "")
//					}
//					case None => {
//						Map("" -> "")
//					}
//				}
				
				println("orderContext	"+orderContext)

				val currentAddedOrderInfo = new WorkInformation(orderIdentity, orderContext)

				//最新のInfoを加算する
				currentWorkInformationHistory += currentAddedOrderInfo
				println("currentWorkInformationHistory	" + currentWorkInformationHistory)

				//動作開始の通知
				messenger.callParent(Messages.MESSAGE_STARTED.toString,
					messenger.tagValues(new TagValue("identity", identity),
						new TagValue("orderIdentity", orderIdentity)))
				
				println("start")
				//動作開始
				run(currentAddedOrderInfo)
				
				println("ec")
			}
			case other => {
				println("未整理の何か	" + other)
			}
		}
	}

	/**
	 *
	 */
	def run(info : WorkInformation) = {
		
		info.context(OrderPrefix.prefixKind) match {
			case WorkKinds.kindJar => {
				println("jarだよ")
			}
			case WorkKinds.kindProcess => {
				println("processだよ")
			}
			case _ => {
				println("not found")
			}
		}
		println("で、ここにきてなう")
		
		currentStatus = WorkerStatus.STATUS_DOING
		
		currentStatus = WorkerStatus.STATUS_DONE
		//挙動完了
		//		messenger.callParent(MESSAGE_DONE,
		//			messenger.tagValues(new TagValue("identity", identity)))
	}
}
	
