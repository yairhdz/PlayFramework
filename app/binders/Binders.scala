package binders

import play.api.mvc.{PathBindable, QueryStringBindable}

import scala.collection.immutable.ListMap
import scala.util.matching.Regex

/**
  * Created by yair on 13/04/16.
  */
object Binders {

  implicit def mapStringIntBindable = new QueryStringBindable[Map[String, Int]] {

    override def bind(key: String, params: Map[String, Seq[String]]): Option[Either[String, Map[String, Int]]] = {
      var bindMap: Map[String, Int] = Map()

      val pattern = new Regex("[A-Z]+|[a-z]+|/")

      params.foreach{ case (k,v) =>
        val patternMatch = pattern.findFirstIn(v(0))
        //println("PATTERN: " + patternMatch)
        patternMatch match {
          case None => bindMap += k -> v(0).toInt
          case Some(s) => "No debe estar en los params, son los titulos de la gráfica"
        }
      }

      if (bindMap.size > 0) {
        Some(Right(bindMap))
      } else {
        Some(Left("No se ha proporcionado el parametro"))
      }
    }

    override def unbind(key: String, value: Map[String, Int]): String = {
      value.map{case (k, v) => k + "=" + v}.mkString("&")
    }
  }
}