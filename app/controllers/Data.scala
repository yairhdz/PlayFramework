package controllers

import javax.inject.Inject

import play.api.db.Database
import play.api.mvc._

import scala.collection.immutable.ListMap
import scala.collection.mutable.ArrayBuffer

/**
  * Created by yair on 13/04/16.
  */
class Data @Inject()(db: Database)extends Controller {

  /**
    *
    * getQueryResultMap
    * Método que se utiliza para hacer las consultas en la base de datos y regresa todos los
    * registros con todas los campos.
    *
    */
  def getQueryResultMap(SQLIntoTemp: String, SQLSelect: String, tempTable: String): Seq[Map[String, String]] = {
    var resultMap: Seq[Map[String, String]] = Seq()
    val records = new ArrayBuffer[Map[String, String]]()
    db.withConnection{ connection =>
      val statement = connection.createStatement()
      statement.execute(s"DROP TABLE IF EXISTS ${tempTable};")
      statement.execute(SQLIntoTemp)

      val resultSet = statement.executeQuery(SQLSelect)
      val resultSetMetaData = resultSet.getMetaData
      var recordsCount = 0
      while (resultSet.next()) {
        var record: Map[String, String] = Map()
        for ( a <- 1 to resultSetMetaData.getColumnCount) {
          val key = resultSetMetaData.getColumnName(a)
          val value = resultSet.getString(a)
          record += key -> value
        }
        records.insert(recordsCount, record)
        recordsCount += 1
      }
    }
    resultMap = records
    resultMap
  }


  /**
    *
    * getMatrixData
    * Método que se utiliza para generar la matriz de productos de una familia
    *
    */
  def getMatrixData(familia: String, tempTable: String): (Array[Array[String]]) = {
    val ejeX = new ArrayBuffer[String]()
    val ejeY = new ArrayBuffer[String]()
    var prods: Map[Int, String]= Map()

    db.withConnection { connection =>
      val statement = connection.createStatement
      val resultSet = statement.executeQuery(s"""
        SELECT
          *
        FROM
          i_l_t_atributos_matrix
        WHERE
          product_category_id = '${familia}'""")

      while(resultSet.next) {
        if (resultSet.getString(2) == "X") {
          ejeX.insert(resultSet.getInt(4), resultSet.getString(3))
        } else if (resultSet.getString(2) == "Y") {
          ejeY.insert(resultSet.getInt(4), resultSet.getString(3))
        }
      }

    }

    db.withConnection { connection =>
      val statement = connection.createStatement
      val prod = statement.executeQuery(s"""
        SELECT
          product.product_id, product.posicion_matriz AS pos,
          cast(coalesce( sum(${tempTable}.cantidad), 0)AS int) AS venta
        FROM
          product LEFT OUTER JOIN ${tempTable} ON product.product_id = ${tempTable}.product_id
        WHERE
          product.primary_product_category_id = '${familia}'
        GROUP BY 1, 2
        ORDER BY 2""")
      while(prod.next()){
        val pos = prod.getInt("pos")
        val venta = prod.getString("venta")
        val productId = prod.getString("product_id")
        prods +=  pos -> venta
      }
    }

    prods = ListMap(prods.toSeq.sortBy(_._1):_*)

    val numRenglones = ejeY.length
    val numColumnas =   ejeX.length

    var matrix = Array.ofDim[String](numRenglones + 1, numColumnas + 1)

    var x = 1
    for(value <- ejeX){
      matrix(0)(x) = value
      x += 1
    }

    var y = 1
    for (valueY <- ejeY) {
      matrix(y)(0) = valueY
      y += 1
    }

    var renglon = 1
    var newIdx = 1

    prods.foreach { case (k, v) =>
      if (renglon <= numRenglones) {
        if (k <= numColumnas) {
          matrix(1)(k) = v
        } else {
          if (newIdx <= numColumnas) {
            matrix(renglon)(newIdx) = v
          } else {
            newIdx = 1
            renglon += 1
            matrix(renglon)(newIdx) = v
          }
        }
        newIdx += 1
      }
    }
    // print(matrix.map(_.mkString).mkString(" \n "))
    matrix
  }
}
