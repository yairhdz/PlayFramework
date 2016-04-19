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

  def ventasPeriodo(tempTable: String) = Action {
    try {
      val data = dataDB.getVentas(s"""
        SELECT product.primary_product_category_id, coalesce(sum(invoice_item.quantity),0) AS cantidad
        INTO TEMP ${tempTable}
        FROM
          product, invoice_item, invoice
        WHERE 1 = 1
           AND invoice.invoice_id = invoice_item.invoice_id
           AND product.product_id = invoice_item.product_id
           AND invoice.invoice_type_id = 'SALES_INVOICE'
           AND invoice.invoice_fis <> 'HISTORICA'
           AND invoice.status_id in ( 'INVOICE_READY', 'INVOICE_PAID', 'INVOICE_IN_PROCESS')
        GROUP BY 1
        ORDER BY 1,2;""", s"SELECT * FROM ${tempTable}", tempTable)
      Ok(views.html.ventas.ventasPeriodo(data))
    } catch {
      case e: Exception =>
        BadRequest("No se pudo generar la consulta, " + e.getMessage)
    }
  }

  def ventasPeriodoFamilia(tempTable: String) = Action { request =>
    val params = request.queryString.map { case (k,v) => k -> v.mkString }
    val familia = params.get("familia").get
    try {
      val data = dataDB.getVentas(s"""
        SELECT
          product.product_id,
          product.primary_product_category_id,
          coalesce( sum(invoice_item.quantity),0) AS cantidad
        INTO TEMP ${tempTable}
          FROM
            product, invoice_item, invoice
          WHERE 1 = 1
            AND invoice.invoice_id = invoice_item.invoice_id
            AND product.product_id = invoice_item.product_id
            AND invoice.invoice_type_id = 'SALES_INVOICE'
            AND invoice.invoice_fis <> 'HISTORICA'
            AND invoice.status_id in ( 'INVOICE_READY', 'INVOICE_PAID', 'INVOICE_IN_PROCESS')
            AND product.primary_product_category_id = '${familia}'
          GROUP BY 1, 2
          ORDER BY 1,2;""", s"""
        SELECT
          product.product_id,
          coalesce( ${tempTable}.cantidad, 0) as venta
        FROM
          product LEFT OUTER JOIN ${tempTable} ON product.product_id = ${tempTable}.product_id
        WHERE  1=1
          AND product.primary_product_category_id = '${familia}'
        ORDER BY 2 DESC;""", tempTable)
      val matriz = dataDB.getMatrixData(familia, tempTable)
      Ok(views.html.ventas.ventasPeriodoFamilia(familia, data, matriz))
    } catch {
      case e: Exception =>
        BadRequest("No se pudo generar la consulta, " + e.getMessage)
    }
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

  def ventasPorPeriodoFamilia(tempTable: String) = Action { request =>
    val params = request.queryString.map { case(k,v) => k -> v.mkString}
    val familia = params.get("familia").getOrElse("")
    val periodoInicio = params.get("inicio").getOrElse("")
    val periodoFin = params.get("fin").getOrElse("")
    try {
      val data = dataDB.getVentas(s"""SELECT product.product_id, product.primary_product_category_id, coalesce( sum(invoice_item.quantity),0) as cantidad
      INTO TEMP ${tempTable}
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
      s"""SELECT product.product_id, coalesce( ${tempTable}.cantidad, 0) as venta
      FROM product left outer join ${tempTable} on product.product_id = ${tempTable}.product_id
        WHERE  1=1
          AND product.primary_product_category_id = '${familia}'
      ORDER BY 2 DESC;""", tempTable)
      val matriz = dataDB.getMatrixData(familia, tempTable)
      Ok(views.html.ventas.ventasPorPeriodoFamilia(familia, periodoInicio, periodoFin, data, matriz))
    } catch {
      case e: Exception =>
        BadRequest("No se pudo generar la consula, " + e.getMessage)
    }
  }

  def ventasPeriodoMesForm() = Action {
    Ok(views.html.ventas.ventasPeriodoMesForm())
  }

  def matchMonthNames(month: Int) = {
    month match {
      case 1 => "Enero"
      case 2 => "Febrero"
      case 3 => "Marzo"
      case 4 => "Abril"
      case 5 => "Mayo"
      case 6 => "Junio"
      case 7 => "Julio"
      case 8 => "Agosto"
      case 9 => "Septiembre"
      case 10 => "Octubre"
      case 11 => "Noviembre"
      case 12 => "Diciembre"
      case _ => " "
    }
  }

  def changeMonthNames(groupedData: ListMap[Int, Map[String, Int]]): Map[String, Map[String, Int]]= {
    var groupedDataMonthNames: Map[String, Map[String, Int]] = Map()
    groupedData.map { case (k, v) =>
      println(k)
        groupedDataMonthNames += matchMonthNames(k) -> v
    }
    println("FINAL DATA: " + groupedDataMonthNames)
    groupedDataMonthNames
  }

  def groupDataByMonth(data: Seq[Map[String, String]]): Map[Int, Map[String, Int]] = {
    var groupedData: Map[Int, Map[String, Int]] = Map()
    data.map { record =>
      val mes = record.get("month").get.toInt
      var newMap: Map[String, Int] = Map()
      val venta = record.get("items").get
      val familia = record.get("primary_product_category_id").get
      if (groupedData.contains(mes)) {
        newMap = groupedData.get(mes).get
        newMap += familia -> venta.toInt
        groupedData += mes -> newMap
      } else {
        newMap += familia -> venta.toInt
        groupedData += mes -> newMap
      }
    }
//    println(groupedData.mkString("\n"))
    groupedData
  }

  def ventasPeriodoMes(tempTable: String) = Action { request =>
    val params = request.queryString.map { case (k, v) => k -> v.mkString}
    val periodo = params.get("periodo").getOrElse("")
    println("PERIODO: " + periodo)
    try {
      val data = dataDB.getVentasAllColumns(s"""
        SELECT
          product.primary_product_category_id,
          extract(year from invoice.invoice_date) as year,
          extract(month from invoice.invoice_date) as month,
          cast(coalesce(sum(invoice_item.quantity),0) as int) as items
        INTO TEMP ${tempTable}
        FROM
          product, invoice_item, invoice
        WHERE 1 = 1
          AND invoice.invoice_id = invoice_item.invoice_id
          AND product.product_id = invoice_item.product_id
          AND invoice.invoice_type_id = 'SALES_INVOICE'
          AND invoice.invoice_fis <> 'HISTORICA'
          AND invoice.status_id in ( 'INVOICE_READY', 'INVOICE_PAID', 'INVOICE_IN_PROCESS')
        GROUP BY 1, 2, 3
        ORDER BY 1, 4, 2, 3;""", s"SELECT * FROM ${tempTable} WHERE ${tempTable}.year = '${periodo}'", tempTable)
//      println("DATA: " + data.mkString("\n"))
      val groupedData = ListMap(groupDataByMonth(data).toSeq.sortBy(_._1):_*)
      val groupedDataMothNames  = changeMonthNames(groupedData)
//      println("GROUPED: " + groupedData)
      val meses = Seq("enero", "febrero")
      Ok(views.html.ventas.ventasPeriodoMes(periodo, meses, groupedDataMothNames))
    } catch {
      case e: Exception =>
        BadRequest("No se pudo general la consulta, " + e.getMessage)
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
  
}
