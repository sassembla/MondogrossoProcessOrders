package com.kissaki.mondogrosso.mondogrossoProcessOrders.option
import com.kissaki.Messenger
import com.kissaki.MessengerProtocol
import com.kissaki.TagValue
import com.kissaki.mondogrosso.mondogrossoProcessOrders.ProcessOrdersMasterMessages
import scala.collection.mutable.ListBuffer
import java.io.File
import java.io.PrintWriter
import com.kissaki.mondogrosso.mondogrossoProcessOrders.MondogrossoContextController


class WriteOnceFileWriter(name: String) extends MessengerProtocol {
  val messenger = new Messenger(this, name)
  val sb :ListBuffer[String] = new ListBuffer()

  def receiver(exec: String, tagValues: Array[TagValue]) = {}

  /**
   * ログ追記
   */
  def addLog(status: String, tagValues: Array[TagValue]) = {
    val userDefinedIdentity = messenger.get("userDefinedIdentity", tagValues).asInstanceOf[String]

    sb += userDefinedIdentity + "@" + status + "\n"
    
    val line = tagValues.toSeq
    for (tagValue <- tagValues) yield tagValue
    tagValues.foreach { v => sb += userDefinedIdentity + "@" + v.toString + "\n" }

    sb += "/"+userDefinedIdentity + "@"+ status + "\n"

  }

  /**
   * ログ書き出し(これだと最後に一気に吐き出されちゃうが、まあ一応。)
   */
  def writeoutLog(file: File) = {
  	val writer = new PrintWriter(file)
    sb.foreach {writer.write}
    writer.close
  }

}