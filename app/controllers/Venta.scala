package controllers

import javax.inject.Inject

import play.api.db.Database
import play.api.mvc._

import scala.collection.immutable.ListMap
import scala.collection.mutable.ArrayBuffer

/**
  * Created by yair on 11/04/16.
  */
 class Venta @Inject()(db: Database, dataDB: Data) extends Controller {

  def menuVentas() = Action {
    Ok(views.html.ventas.menuVentas())
   }

  def ventasPeriodo = Action {
    val data = getVentasPeriodo()
    Ok(views.html.ventas.ventasPeriodo(data))
  }

  def ventasPeriodoFamilia = Action { request =>
    val params = request.queryString.map { case (k,v) => k -> v.mkString }
    val familia = params.get("familia").get
    val data = getVentasPeriodoFamilia(familia)
    val matriz = getMatrixData(familia)
    Ok(views.html.ventas.ventasPeriodoFamilia(familia, data, matriz))
  }

  def ventasPorPeriodoForm = Action {
    Ok(views.html.ventas.ventasPorPeriodoForm())
  }

  def ventasPorPeriodo(tempTable: String) = Action { request =>
    val params = request.queryString.map { case(k,v) => k -> v.mkString}
    val periodoInicio = params.get("inicio").getOrElse("")
    val periodoFin    = params.get("fin").getOrElse("")
    try {
      val data = dataDB.getVentas(s"""SELECT product.primary_product_category_id, coalesce( sum(invoice_item.quantity),0) as cantidad
        INTO TEMP ${tempTable}
        FROM product, invoice_item, invoice
        where 1 = 1
          AND invoice.invoice_id = invoice_item.invoice_id
          AND product.product_id = invoice_item.product_id
          AND invoice.invoice_type_id = 'SALES_INVOICE'
          AND invoice.invoice_fis <> 'HISTORICA'
          AND invoice.status_id in ('INVOICE_READY', 'INVOICE_PAID', 'INVOICE_IN_PROCESS')
          AND invoice.invoice_date >= '${periodoInicio}'
          AND invoice.invoice_date <=  '${periodoFin}'
        GROUP BY 1
        ORDER BY 1,2;
        """, s"SELECT * FROM ${tempTable}", tempTable)
      Ok(views.html.ventas.ventasPorPeriodo(data, periodoInicio, periodoFin))
    } catch {
      case e: Exception =>
        BadRequest("No se pudo generar la consulta, " + e.getMessage)
    }

  }

  def ventasPorPeriodoFamilia = Action { request =>
    val params = request.queryString.map { case(k,v) => k -> v.mkString}
    val familia = params.get("familia").getOrElse("")
    val periodoInicio = params.get("inicio").getOrElse("")
    val periodoFin = params.get("fin").getOrElse("")
    try {
      val data = dataDB.getVentas(s"""SELECT product.product_id, product.primary_product_category_id, coalesce( sum(invoice_item.quantity),0) as cantidad
      INTO TEMP ventas_por_periodo_familia
      FROM product, invoice_item, invoice
      WHERE 1 = 1
        AND invoice.invoice_id = invoice_item.invoice_id
        AND product.product_id = invoice_item.product_id
        AND invoice.invoice_type_id = 'SALES_INVOICE'
        AND invoice.invoice_fis <> 'HISTORICA'
        AND invoice.status_id in ( 'INVOICE_READY', 'INVOICE_PAID', 'INVOICE_IN_PROCESS')
        AND invoice.invoice_date >= '${periodoInicio}'
        AND invoice.invoice_date <=  '${periodoFin}'
        AND product.primary_product_category_id = '${familia}'
      GROUP BY 1
      ORDER BY 1,2;
      """,
      s"""SELECT product.product_id, coalesce( ventas_por_periodo_familia.cantidad, 0) as venta
      FROM product left outer join ventas_por_periodo_familia on product.product_id = ventas_por_periodo_familia.product_id
        WHERE  1=1
          AND product.primary_product_category_id = '${familia}'
      ORDER BY 2 DESC;""", "ventas_por_periodo_familia")
      val matriz = dataDB.getMatrixData(familia, "ventas_por_periodo_familia")
      Ok(views.html.ventas.ventasPorPeriodoFamilia(familia, periodoInicio, periodoFin, data, matriz))
    } catch {
      case e: Exception =>
        BadRequest("No se pudo generar la consula, " + e.getMessage)
    }
  }

  def getVentasPeriodo(): Map[String, Int] = {
    var data: Map[String, Int] = Map()
    db.withConnection{ connection =>
      val statement = connection.createStatement()
      statement.execute("DROP TABLE IF EXISTS venta_familias;")
      statement.execute("""SELECT product.primary_product_category_id, coalesce(sum(invoice_item.quantity),0) AS cantidad
        INTO TEMP venta_familias
        FROM product, invoice_item, invoice
        WHERE 1 = 1
           AND invoice.invoice_id = invoice_item.invoice_id
           AND product.product_id = invoice_item.product_id
           AND invoice.invoice_type_id = 'SALES_INVOICE'
           AND invoice.invoice_fis <> 'HISTORICA'
           AND invoice.status_id in ( 'INVOICE_READY', 'INVOICE_PAID', 'INVOICE_IN_PROCESS')
        GROUP BY 1
        ORDER BY 1,2;""")

      val resultSet = statement.executeQuery("SELECT * FROM venta_familias")

      while (resultSet.next()) {
        val categoryId = resultSet.getString(1)
        val venta = resultSet.getInt(2)
        data += categoryId -> venta
      }
    }
    println(data.mkString)
    data = ListMap(data.toSeq.sortWith(_._2 >_._2):_*)
    data
  }

  def getVentasPeriodoFamilia(familia: String): Map[String, Int] = {
    var data: Map[String, Int] = Map()
    db.withConnection{ connection =>
      val statement = connection.createStatement()
      statement.execute("DROP TABLE IF EXISTS venta_familias;")
      statement.execute( s"""SELECT product.product_id, product.primary_product_category_id, coalesce( sum(invoice_item.quantity),0) AS cantidad
        INTO TEMP venta_familias
          FROM product, invoice_item, invoice
          WHERE 1 = 1
            AND invoice.invoice_id = invoice_item.invoice_id
            AND product.product_id = invoice_item.product_id
            AND invoice.invoice_type_id = 'SALES_INVOICE'
            AND invoice.invoice_fis <> 'HISTORICA'
            AND invoice.status_id in ( 'INVOICE_READY', 'INVOICE_PAID', 'INVOICE_IN_PROCESS')
            AND product.primary_product_category_id = '${familia}'
          GROUP BY 1, 2
          ORDER BY 1,2;""")

      val resultSet = statement.executeQuery( s"""SELECT product.product_id, coalesce( venta_familias.cantidad, 0) as venta
        FROM product left outer join venta_familias on product.product_id = venta_familias.product_id
        WHERE  1=1
          AND product.primary_product_category_id = '${familia}'
        ORDER BY 2 DESC;""");

      while (resultSet.next()) {
        val categoryId = resultSet.getString(1)
        val venta = resultSet.getInt(2)
        data += categoryId -> venta
      }
    }
    println(data.mkString)
    data = ListMap(data.toSeq.sortWith(_._2 >_._2):_*)
    data
  }

  def selectData(indicator: Option[String]) = {
    indicator match {
      case Some(s) => getVentasPeriodoFamilia(s)
      case None => getVentasPeriodo()
    }
  }

  def createChart(data: Map[String, Int], title: String, titleX: String, titleY: String) = Action { request =>
    val sortedData = ListMap(data.toList.sortWith(_._2 > _._2):_*)
    println("DATA: " + data.mkString)
    println(data.map{case (k, v) => k + "=" + v}.mkString("&"))
    val MimeType = "image/png"
    try {
      val chart = new JFreeChart()
      val imageData = chart.generateBarChart(sortedData, title, titleX, titleY)
      Ok(imageData).as(MimeType)
    } catch {
      case e: Exception =>
      BadRequest("Couldn’t generate chart. Error: " + e.getMessage)
    }
  }



  def getMatrixData(familia: String): (Array[Array[String]]) = {
    val ejeX = new ArrayBuffer[String]()
    val ejeY = new ArrayBuffer[String]()
    var prods: Map[Int, String]= Map()

    db.withConnection { connection =>
      val statement = connection.createStatement
      val resultSet = statement.executeQuery(s"SELECT * FROM i_l_t_atributos_matrix WHERE product_category_id = '${familia}'")

      while(resultSet.next) {
        println(resultSet.getString(1))
        if (resultSet.getString(2) == "X") {
          ejeX.insert(resultSet.getInt(4), resultSet.getString(3))
        } else if (resultSet.getString(2) == "Y") {
          ejeY.insert(resultSet.getInt(4), resultSet.getString(3))
        }
      }

    }

    db.withConnection { connection =>
      val statement = connection.createStatement
      val prod = statement.executeQuery(s"""SELECT product.product_id, product.posicion_matriz AS pos,
        to_char(coalesce( venta_familias.cantidad, 0), '999G999D') AS venta
        FROM product LEFT OUTER JOIN venta_familias ON product.product_id = venta_familias.product_id
        WHERE product.primary_product_category_id = '${familia}'
        ORDER BY 2""")
      while(prod.next()){
        val pos = prod.getInt("pos")
        val venta = prod.getString("venta")
        prods +=  pos -> venta
      }
    }

    prods = ListMap(prods.toSeq.sortBy(_._1):_*)

    val numRenglones = ejeY.length
    val numColumnas =   ejeX.length

    var mat = Array.ofDim[String](numRenglones + 1, numColumnas + 1)

    var x = 1
    for(value <- ejeX){
      mat(0)(x) = value
      x += 1
    }

    var y = 1
    for (valueY <- ejeY) {
      mat(y)(0) = valueY
      y += 1
    }

    var renglon = 1
    var newIdx = 1

    prods.foreach { case (k, v) =>
      if (renglon <= numRenglones) {
        if (k <= numColumnas) {
          mat(1)(k) = v
        } else {
          if (newIdx <= numColumnas) {
            mat(renglon)(newIdx) = v
          } else {
            newIdx = 1
            renglon += 1
            mat(renglon)(newIdx) = v
          }
        }
        newIdx += 1
      }
    }

    print(mat.map(_.mkString).mkString(" \n "))

    mat
  }
}
