package com.kissaki.mondogrosso.mondogrossoProcessOrders

import org.clapper.argot._
import ArgotConverters._
import java.util.UUID
import java.io.File
import com.kissaki.mondogrosso.mondogrossoProcessOrders.option.FileWriteReceiver

/**
 * ファイルからのJSON読み込みの為の機構
 * app化	→ 起動時に何らかのWebGuiを出すセットの実行
 *
 *
 * ver 0.1.0	基礎機能作成完了
 * 					オーダーの解釈
 * 					JSONからの
 */
object MondogrossoProcessOrders {
	def main(args : Array[String]) : Unit = {
		println("main開始")
		val writerId = UUID.randomUUID.toString
		val fileWriteReceiver = new FileWriteReceiver(writerId)

		//コマンドラインのパーサ
		val argotParser = new ArgotParser("MondogrossoProcessOrders", preUsage = Some("Mondogrosso processOrder"))

		//commandのソース
		val commandSource = "exec"
		println("commandSource  " + commandSource)

		//processOrderのユーザー定義identity
		val orderIdentitySource = argotParser.option[String](List("i", "identity"), "processOrder's append-identity", "the String of the process-order identity to identify same-name-processOrders") { (i, opt) =>
			println("i  " + i)
			println("opt  " + opt)
			try { i.toString } catch {
				case _ => UUID.randomUUID.toString
			}
		}

		//processOrderのソース
		val processOrderSource = argotParser.option[String](List("p", "processorder"), "processOrder", "the String of the process-order pattern") { (p, opt) =>
			println("p  " + p)
			println("opt  " + opt)

			try { p.toString } catch {
				case _ => throw new ArgotConversionException("no input")
			}
		}

		//JSONFile
		val sourceJSONFile = argotParser.option[File](List("sf", "sourcejsonfile"), "sourceJSONFile", "the JSON-file-path or file of the process-order's detail JSON") { (sf, opt) =>
			println("sf " + sf)
			println("opt  " + opt)

			val file = new File(sf)
			file.exists match {
				case true => {
					file
				}
				case false => {
					null
				}
			}
		}

		//JSONString
		val sourceJSONSource = argotParser.option[String](List("s", "sourcejson"), "sourceJSON", "the JSON-string-representeation of the process-order's detail JSON") { (s, opt) =>
			println("s  " + s)
			println("opt  " + opt)

			try { s.toString } catch {
				case _ =>
					throw new ArgotConversionException(
						"can not define sourceJSON description")
			}
		}

		//ログのoutput場所の指定
		val logOutputFile = argotParser.option[File](List("o", "outputlog"), "outputLog", "the outputlog of the process-order's result & proceed") { (o, opt) =>
			println("o " + o)
			println("opt " + opt)

			val file = new File(o)
			file.exists match {
				case true => {
					file
				}
				case false => {
					//logファイルをデフォルト箇所に作り出す
					println("logファイルをデフォルト箇所に作り出す")
					file
				}
			}
		}

		//パース
		argotParser.parse(args)

		println("orderIdentitySource	" + orderIdentitySource.value.get)
		println("processOrderSource	" + processOrderSource.value.get)
		println("sourceJSONSource	" + sourceJSONSource.value)

		val id = UUID.randomUUID().toString

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
		
		val contextCont = new MondogrossoContextController(writerId)

		//処理
		commandSource match {
			case "exec" => { //デフォルト動作
				println("デフォルトの動作開始")
				
				println("fileWriteReceiver	"+fileWriteReceiver.messenger.getParentName)
				//アタッチ
				contextCont.attachProcessOrders(orderIdentitySource.value.get, parseResult)

				//起動
				contextCont.runAllContext
				while (!contextCont.currentStatus.equals(ContextContStatus.STATUS_EMPTY)) {
					Thread.sleep(100)
					println("wait	マスター実行中")
				}
//				//ログの書き出し
//				fileWriteReceiver.writeoutLog
				contextCont.currentResultsOfContext(orderIdentitySource.value.get)
				
				"値を返すフリ"
			}
			case "attach" => {
				//アタッチ
				contextCont.attachProcessOrders(orderIdentitySource.value.get, parseResult)
			}
			case "run" => {
				//起動
				contextCont.runAllContext
			}
			case other => {
				println("other	" + other)
			}
		}

	}

}