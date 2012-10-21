package com.kissaki.mondogrosso.mondogrossoProcessOrders.option
import com.kissaki.Messenger
import com.kissaki.MessengerProtocol
import com.kissaki.TagValue
import com.kissaki.mondogrosso.mondogrossoProcessOrders.ProcessOrdersMasterMessages
import scala.collection.mutable.ListBuffer
import java.io.File
import java.io.PrintWriter


class FileWriteReceiver(name: String) extends MessengerProtocol {
  println("ファイル書き出し機能初期化	" + name)
  val messenger = new Messenger(this, name)
  val sb :ListBuffer[String] = new ListBuffer()

  def receiver(exec: String, tagValues: Array[TagValue]) = {
    ProcessOrdersMasterMessages.get(exec) match {
      case ProcessOrdersMasterMessages.MESSAGE_READY => addLog(ProcessOrdersMasterMessages.MESSAGE_READY.toString, tagValues)
      case ProcessOrdersMasterMessages.MESSAGE_START => addLog(ProcessOrdersMasterMessages.MESSAGE_START.toString, tagValues)
      case ProcessOrdersMasterMessages.MESSAGE_PROCEEDED => addLog(ProcessOrdersMasterMessages.MESSAGE_PROCEEDED.toString, tagValues)
      case ProcessOrdersMasterMessages.MESSAGE_ERROR => addLog(ProcessOrdersMasterMessages.MESSAGE_ERROR.toString, tagValues)
      case ProcessOrdersMasterMessages.MESSAGE_TIMEOUTED => addLog(ProcessOrdersMasterMessages.MESSAGE_TIMEOUTED.toString, tagValues)
      case ProcessOrdersMasterMessages.MESSAGE_DONE => addLog(ProcessOrdersMasterMessages.MESSAGE_DONE.toString, tagValues)
    }
  }

  /**
   * ログ追記
   */
  def addLog(status: String, tagValues: Array[TagValue]) = {
    sb += status + "\n"
    
    val line = tagValues.toSeq
    for (tagValue <- tagValues) yield tagValue
    tagValues.foreach { v => sb += v.toString + "\n" }
    println("sbは  "+sb)
  }

  /**
   * ログ書き出し
   */
  def writeoutLog(file: File) = {
  	val writer = new PrintWriter(file)
    sb.foreach {writer.write}
    writer.close()
  }

}