package controllers

import javax.inject.Inject

import play.api.db.Database
import play.api.mvc._

import scala.collection.immutable.ListMap

/**
  * Created by yair on 13/04/16.
  */
class Data @Inject()(db: Database)extends Controller {

  def getVentas(SQL: String, tempTable: String): Map[String, Int] = {
    var data: Map[String, Int] = Map()
    db.withConnection{ connection =>
      val statement = connection.createStatement()
      statement.execute(s"DROP TABLE IF EXISTS ${tempTable};")
      statement.execute(SQL)

      val resultSet = statement.executeQuery(s"SELECT * FROM ${tempTable}")

      while (resultSet.next()) {
        val categoryId = resultSet.getString(1)
        val venta = resultSet.getInt(2)
        data += categoryId -> venta
      }
    }
    //println(data.mkString)
    data = ListMap(data.toSeq.sortWith(_._2 >_._2):_*)
    data
  }
}
