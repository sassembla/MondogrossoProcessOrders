package com.kissaki.mondogrosso.mondogrossoProcessOrders

import org.clapper.argot._
import ArgotConverters._
import java.util.UUID
import java.io.File
import com.kissaki.mondogrosso.mondogrossoProcessOrders.fileWriters.AppendFileWriter
import com.kissaki.MessengerProtocol
import com.kissaki.Messenger
import com.kissaki.TagValue
 // import scala.concurrent._
 // import ExecutionContext.Implicits.global
//import scala.actors.Future


class MondogrossoCommandLineInterface(identity:String, targetFilePath:String) extends MessengerProtocol {
	 var status:Int = _ 
	// val firstOccurence: Future[Int] = future {
	//   val source = scala.io.Source.fromFile("myText.txt")
	//   source.toSeq.indexOfSlice("myKeyword")
	// }
//	firstOccurence.//
	
  val messenger = new Messenger(this, identity)

  val writerId = UUID.randomUUID.toString
  val fileWriteReceiver = new AppendFileWriter(writerId, identity, targetFilePath) //追加書き出し

  var logOutputFile: org.clapper.argot.SingleValueOption[java.io.File] = _

  def receiver(exec: String, tagValues: Array[TagValue]) = {
    ProcessOrdersMasterMessages.get(exec) match {
      case ProcessOrdersMasterMessages.MESSAGE_READY => {
        val userDefinedIdentity = messenger.get("userDefinedIdentity", tagValues).asInstanceOf[String] + "@" + ProcessOrdersMasterMessages.MESSAGE_READY.toString
        messenger.call(writerId, "addLog", messenger.tagValues(new TagValue("title", userDefinedIdentity), new TagValue("tagValues", tagValues)))
      }
      case ProcessOrdersMasterMessages.MESSAGE_START => {
        val userDefinedIdentity = messenger.get("userDefinedIdentity", tagValues).asInstanceOf[String] + "@" + ProcessOrdersMasterMessages.MESSAGE_START.toString
        messenger.call(writerId, "addLog", messenger.tagValues(new TagValue("title", userDefinedIdentity), new TagValue("tagValues", tagValues)))
      }
      case ProcessOrdersMasterMessages.MESSAGE_PROCEEDED => {
        val userDefinedIdentity = messenger.get("userDefinedIdentity", tagValues).asInstanceOf[String] + "@" + ProcessOrdersMasterMessages.MESSAGE_PROCEEDED.toString
        messenger.call(writerId, "addLog", messenger.tagValues(new TagValue("title", userDefinedIdentity), new TagValue("tagValues", tagValues)))
      }
      case ProcessOrdersMasterMessages.MESSAGE_ERROR => {
        val userDefinedIdentity = messenger.get("userDefinedIdentity", tagValues).asInstanceOf[String] + "@" + ProcessOrdersMasterMessages.MESSAGE_ERROR.toString
        messenger.call(writerId, "addLog", messenger.tagValues(new TagValue("title", userDefinedIdentity), new TagValue("tagValues", tagValues)))
      }

      case ProcessOrdersMasterMessages.MESSAGE_TIMEOUTED => {
        val userDefinedIdentity = messenger.get("userDefinedIdentity", tagValues).asInstanceOf[String] + "@" + ProcessOrdersMasterMessages.MESSAGE_TIMEOUTED.toString
        messenger.call(writerId, "addLog", messenger.tagValues(new TagValue("title", userDefinedIdentity), new TagValue("tagValues", tagValues)))
      }
      case ProcessOrdersMasterMessages.MESSAGE_DONE => {
        val userDefinedIdentity = messenger.get("userDefinedIdentity", tagValues).asInstanceOf[String] + "@" + ProcessOrdersMasterMessages.MESSAGE_DONE.toString
        messenger.call(writerId, "addLog", messenger.tagValues(new TagValue("title", userDefinedIdentity), new TagValue("tagValues", tagValues)))
      }

      case ProcessOrdersMasterMessages.MESSAGE_CONTEXT_OVER => {
        println("MESSAGE_CONTEXT_OVER が届いた")
        messenger.call(writerId, "wtiteOut", messenger.tagValues(new TagValue("fileName", logOutputFile.value.get)))
        
        status = 1
      }
      case _ =>
    }
  }
  
  /**
   * インプット
   */
  def input(inputArgs: Array[String]) = {
    //コマンドラインのパーサ
    val argotParser = new ArgotParser("MondogrossoProcessOrders", preUsage = Some("Mondogrosso processOrder"))

    //commandのソース
    val commandSource = ProcessOrdersModes.MODE_DEFAULT.toString

    //processOrderのユーザー定義identity
    val orderIdentitySource = argotParser.option[String](List("i", "identity"), "processOrder's append-identity", "the String of the process-order identity to identify same-name-processOrders") { (i, opt) =>
      try {
        i.toString
      } catch {
        case _ => throw new ArgotConversionException("should set filePath for -sf option")
      }
    }

    //processOrderのソース
    val processOrderSource = argotParser.option[String](List("p", "processorder"), "processOrder", "the String of the process-order pattern") { (p, opt) =>

      try {
        p.toString
      } catch {
        case _ => throw new ArgotConversionException("no process imput, please set -p & input.")
      }
    }

    //JSONString
    val sourceJSONSource = argotParser.option[String](List("s", "sourcejson"), "sourceJSON", "the JSON-string-representeation of the process-order's detail JSON") { (s, opt) =>
      try {
        s.toString
      } catch {
        case _ => throw new ArgotConversionException("can not define sourceJSON description")
      }
    }

    //JSONFile
    val sourceJSONFile = argotParser.option[File](List("sf", "sourcejsonfile"), "sourceJSONFile", "the JSON-file-path or file of the process-order's detail JSON") { (sf, opt) =>
      // println("sf " + sf)
      // println("sf	opt  " + opt)

      val file = new File(sf)
      file.exists match {
        case true => {
          file
        }
        case false => {
          throw new ArgotConversionException("should set filePath for -sf option")
        }
      }
    }

    //ログのoutput場所の指定
    logOutputFile = argotParser.option[File](List("o", "outputlog"), "outputLog", "the outputlog of the process-order's result & proceed") { (o, opt) =>
      // println("o " + o)
      // println("o	opt " + opt)
      val file = new File(o)

      file.exists match {
        case true => {
          file
        }
        case false => {
          //logファイルをデフォルト箇所に作り出す
          file
        }
      }
    }
    // println("logOutputFile	"+logOutputFile)

    //パース
    argotParser.parse(inputArgs)

    //このMondogrossoProcessOrdersのParseで作成するプロセスに対するid
    val id = UUID.randomUUID().toString

    //パース処理　processOrderSource と sourceJSONSource を使用
    val parser = new MondogrossoProcessParser(id, processOrderSource.value.get, sourceJSONSource.value.get)
    val parseResult = parser.parseProcess

    /**
     * このパーツは完全に「外部で使う用」のコネクタの域なので、
     * 適当。
     *
     * 実行開始したら、処理が終わってもプロセスが生き続けるモードと、
     * 実行完了したら終了するモードの2つをもつ
     *
     * mode once
     * 	とりあえずattachからrunまでこなす
     *
     * mode alive
     * 	プロセスとして常駐する
     * 	repl
     *
     * onceモードがデフォルト。
     * 	alive時には、現在動作中のものとかを表示する機能とか
     * 	attachしたりrunしたりabortしたりさせたい。
     *
     * 	まあ後回しだな。起動できればいいや。
     */

    val contextCont = new MondogrossoContextController(identity)

    //処理
    ProcessOrdersModes.get(commandSource) match {
      case ProcessOrdersModes.MODE_DEFAULT => { //デフォルト動作
        //アタッチ
        contextCont.attachProcessOrders(orderIdentitySource.value.get, parseResult)
        //起動
        contextCont.runAllContext
      }
      case ProcessOrdersModes.MODE_ATTACH => {
        //アタッチ
        contextCont.attachProcessOrders(orderIdentitySource.value.get, parseResult)
      }
      case ProcessOrdersModes.MODE_RUN => {
        //起動
        contextCont.runAllContext

      }
      case other => {
        println("other	" + other)
      }
    }
  }

  
  /**
		この関数で、状態を返したい。とりあえず変数で返す

  */
  def isAlive = {
  	status
  }

}