package com.github.hexx

import java.io.File
import scala.io.Source
import scala.sys.process._
import scala.util.control.Exception._
import play.core.jscompile.JavascriptCompiler
import play.PlayExceptions.AssetCompilationException
import scala.util.parsing.json.JSON

object HaxeCompiler {
  def compile(file: File, options: Seq[String]) = {
    val (js, dependecies) = executeNativeCompiler(file, options)
    (js, minify(js, file), dependecies)
  }

  def minify(js: String, file: File) = {
    catching(classOf[AssetCompilationException])
      .opt(JavascriptCompiler.minify(js, Some(file.getName)))
  }

  def executeNativeCompiler(src: File, options: Seq[String]): (String, Seq[File]) = {
    // JS output
    val dest = File.createTempFile(src.getName, ".js")
    // JS map output generated with flag -debug, it is used to get dependencies
    val destMap = new File(dest.getAbsolutePath + ".map")
    try {
      val params = Seq("haxe", "-main", src.getName, "-js", dest.getAbsolutePath, "-debug") ++ options ++
          {if (!options.contains("-cp")) Seq("-cp", src.getParentFile.getAbsolutePath) else Seq.empty}
      val process = Process(params)
      val out = new StringBuilder
      val err = new StringBuilder
      val logger = ProcessLogger(s => out.append(s + "\n"), s => err.append(s + "\n"))
      val exit = process ! logger
      if (exit == 0) {
        (Source.fromFile(dest).mkString,
          JSON.parseFull(Source.fromFile(destMap).mkString).get
          .asInstanceOf[Map[String, Any]]("sources")
          .asInstanceOf[Seq[String]].map(path => new File(path)))
      } else {
        var errString = err.mkString

        // .hx files without main function is used to be imported from other files.
        // Returns empty string in this case.
        if (errString.contains(" does not have static function main"))
          ("", Seq.empty[File])
        else {
          // Following regex assumes that a path name
          // 1. starts with a non-space character
          // 2. doesn't contain : (colon)
          val regex = """(\S[^:]*\.hx):(\d+): (?:characters (\d+))?""".r
          val (file, line, column) = regex.findFirstMatchIn(errString).map { (m) =>
            (
              Option(m.group(1)).map(new File(_)),
              Option(m.group(2)).map(_.toInt),
              Option(m.group(3)).map(_.toInt))
          }.getOrElse( {
            // add parameters send to compiler to better identify a problem
            errString += "\n" + params.mkString(" ")
            (Some(src), None, None)
          })
          throw AssetCompilationException(file, errString, line, column)
        }
      }
    } finally {
      dest.delete()
      destMap.delete()
    }
  }
}
