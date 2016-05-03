package controllers

import java.text.NumberFormat
import java.util.Calendar
import javax.inject.Inject

import play.api.db.Database
import play.api.mvc._

import scala.collection.immutable.ListMap
import scala.collection.mutable.ArrayBuffer

/**
  * Created by yair on 11/04/16.
  */
 class Venta @Inject()(db: Database, dataDB: Data, chart: Chart) extends Controller {

  def menuVentas() = Action {
    Ok(views.html.ventas.menuVentas())
   }

  def ventasPeriodo = Action { request =>
    val params = request.queryString.map { case (k, v) => k -> v.mkString }
    val tempTable = params.getOrElse("src", "tempTable")
    try {
      val data = dataDB.getVentas(s"""
        SELECT product.primary_product_category_id, coalesce(sum(invoice_item.quantity),0) AS cantidad
        INTO TEMP $tempTable
        FROM
          product, invoice_item, invoice
        WHERE 1 = 1
           AND invoice.invoice_id = invoice_item.invoice_id
           AND product.product_id = invoice_item.product_id
           AND invoice.invoice_type_id = 'SALES_INVOICE'
           AND invoice.invoice_fis <> 'HISTORICA'
           AND invoice.status_id in ( 'INVOICE_READY', 'INVOICE_PAID', 'INVOICE_IN_PROCESS')
        GROUP BY 1
        ORDER BY 1,2;""", s"SELECT * FROM $tempTable", tempTable)
      val imageData = chart.generateBarChart(data, "Top 20 Ventas por familia", "Familias", "Venta")
      Ok(views.html.ventas.ventasPeriodo(imageData, data))
    } catch {
      case e: Exception =>
        BadRequest("No se pudo generar la consulta, " + e.getMessage)
    }
  }

  def ventasPeriodoFamilia = Action { request =>
    val params = request.queryString.map { case (k,v) => k -> v.mkString }
    val tempTable = params.getOrElse("src", "tempTable")
    val familia   = params.get("familia").get
    try {
      val data = dataDB.getVentas(s"""
        SELECT
          product.product_id,
          product.primary_product_category_id,
          coalesce( sum(invoice_item.quantity),0) AS cantidad
        INTO TEMP $tempTable
          FROM
            product, invoice_item, invoice
          WHERE 1 = 1
            AND invoice.invoice_id = invoice_item.invoice_id
            AND product.product_id = invoice_item.product_id
            AND invoice.invoice_type_id = 'SALES_INVOICE'
            AND invoice.invoice_fis <> 'HISTORICA'
            AND invoice.status_id in ( 'INVOICE_READY', 'INVOICE_PAID', 'INVOICE_IN_PROCESS')
            AND product.primary_product_category_id = '$familia'
          GROUP BY 1, 2
          ORDER BY 1,2;""", s"""
        SELECT
          product.product_id,
          coalesce( $tempTable.cantidad, 0) as venta
        FROM
          product LEFT OUTER JOIN $tempTable ON product.product_id = $tempTable.product_id
        WHERE  1=1
          AND product.primary_product_category_id = '$familia'
        ORDER BY 2 DESC;""", tempTable)
      val matriz = dataDB.getMatrixData(familia, tempTable)
      val imageData = chart.generateBarChart(data, s"Top 20 Ventas $familia", "Productos", "Venta")
//      Ok(views.html.ventas.ventasPeriodoFamilia(familia, data, matriz))
      Ok(views.html.ventas.detalleFGMMain(familia, imageData,matriz))
    } catch {
      case e: Exception =>
        BadRequest("No se pudo generar la consulta, " + e.getMessage)
    }
  }

  def ventasPorPeriodoForm = Action {
    Ok(views.html.ventas.ventasPorPeriodoForm())
  }

  def ventasPorPeriodo = Action { request =>
    val params = request.queryString.map { case(k,v) => k -> v.mkString}
    val periodoInicio = params.get("inicio").getOrElse("")
    val periodoFin    = params.get("fin").getOrElse("")
    val tempTable     = params.getOrElse("src", "tempTable")
    try {
      val data = dataDB.getVentas(s"""SELECT product.primary_product_category_id, coalesce( sum(invoice_item.quantity),0) as cantidad
        INTO TEMP $tempTable
        FROM product, invoice_item, invoice
        where 1 = 1
          AND invoice.invoice_id = invoice_item.invoice_id
          AND product.product_id = invoice_item.product_id
          AND invoice.invoice_type_id = 'SALES_INVOICE'
          AND invoice.invoice_fis <> 'HISTORICA'
          AND invoice.status_id in ('INVOICE_READY', 'INVOICE_PAID', 'INVOICE_IN_PROCESS')
          AND invoice.invoice_date >= '$periodoInicio'
          AND invoice.invoice_date <=  '$periodoFin'
        GROUP BY 1
        ORDER BY 1,2;
        """, s"SELECT * FROM $tempTable", tempTable)
      val imageData = chart.generateBarChart(data, s"Top 20 Ventas, periodo ${periodoInicio} - ${periodoFin}", "Familias", "Venta")
      Ok(views.html.ventas.ventasPorPeriodo(imageData, data, periodoInicio, periodoFin))
    } catch {
      case e: Exception =>
        BadRequest("No se pudo generar la consulta, " + e.getMessage)
    }

  }

  def ventasPorPeriodoFamilia = Action { request =>
    val params = request.queryString.map { case(k,v) => k -> v.mkString}
    val periodoInicio = params.get("inicio").getOrElse("")
    val periodoFin    = params.get("fin").getOrElse("")
    val familia       = params.get("familia").getOrElse("")
    val tempTable     = params.getOrElse("src", "tempTable")

    try {
      val data = dataDB.getVentas(s"""SELECT product.product_id, product.primary_product_category_id, coalesce( sum(invoice_item.quantity),0) as cantidad
      INTO TEMP $tempTable
      FROM product, invoice_item, invoice
      WHERE 1 = 1
        AND invoice.invoice_id = invoice_item.invoice_id
        AND product.product_id = invoice_item.product_id
        AND invoice.invoice_type_id = 'SALES_INVOICE'
        AND invoice.invoice_fis <> 'HISTORICA'
        AND invoice.status_id in ( 'INVOICE_READY', 'INVOICE_PAID', 'INVOICE_IN_PROCESS')
        AND invoice.invoice_date >= '$periodoInicio'
        AND invoice.invoice_date <=  '$periodoFin'
        AND product.primary_product_category_id = '$familia'
      GROUP BY 1
      ORDER BY 1,2;
      """,
      s"""SELECT product.product_id, coalesce( $tempTable.cantidad, 0) as venta
      FROM product left outer join $tempTable on product.product_id = $tempTable.product_id
        WHERE  1=1
          AND product.primary_product_category_id = '$familia'
      ORDER BY 2 DESC;""", tempTable)
      val matriz = dataDB.getMatrixData(familia, tempTable)
      val imageData = chart.generateBarChart(data, s"Top 20 Ventas ${familia}, periodo ${periodoInicio} - ${periodoFin}", "Productos", "Venta")
//      Ok(views.html.ventas.ventasPorPeriodoFamilia(familia, periodoInicio, periodoFin, data, matriz))
      Ok(views.html.ventas.detalleFGMNoMain(familia, imageData, matriz))
    } catch {
      case e: Exception =>
        BadRequest("No se pudo generar la consula, " + e.getMessage)
    }
  }

  def matchMonthNames(month: String) = {
    month match {
      case "1" => "Enero"
      case "2" => "Febrero"
      case "3" => "Marzo"
      case "4" => "Abril"
      case "5" => "Mayo"
      case "6" => "Junio"
      case "7" => "Julio"
      case "8" => "Agosto"
      case "9" => "Septiembre"
      case "10" => "Octubre"
      case "11" => "Noviembre"
      case "12" => "Diciembre"
      case "Enero" => "1"
      case "Febrero" => "2"
      case "Marzo" => "3"
      case "Abril" => "4"
      case "Mayo" => "5"
      case "Junio" => "6"
      case "Julio" => "7"
      case "Agosto" => "8"
      case "Septiembre" => "9"
      case "Octubre" => "10"
      case "Noviembre" => "11"
      case "Diciembre" => "12"
      case _ => " "
    }
  }

  def itemsFacturasForm = Action {
    Ok(views.html.ventas.itemsFacturasForm())
  }

  def itemsFacturas = Action { request =>
    val params = request.queryString.map { case (k,v) => k -> v.mkString}
    println(params)
    val periodo = params.get("periodo").getOrElse("")
    val tempTable = params.get("src").getOrElse("")
    val MimeType = "image/png"
    try {
      val dataDB = new Data(db)
      val data = dataDB.getQueryResultMap(s"""
        SELECT
           extract(year from invoice.invoice_date) AS year,
           extract(month from invoice.invoice_date) AS month,
           cast(coalesce( sum(invoice_item.quantity),0) AS int) AS items,
           count(distinct invoice.invoice_id) AS facturas
        INTO TEMP $tempTable
        FROM product, invoice_item, invoice
        WHERE 1 = 1
            AND invoice.invoice_id = invoice_item.invoice_id
            AND product.product_id = invoice_item.product_id
            AND invoice.invoice_type_id = 'SALES_INVOICE'
            AND invoice.invoice_fis <> 'HISTORICA'
            AND invoice.status_id in ( 'INVOICE_READY', 'INVOICE_PAID', 'INVOICE_IN_PROCESS')
        GROUP BY 1,2
        ORDER BY 2;""", s"SELECT * FROM $tempTable WHERE year = '$periodo'", tempTable)

      var primaryData: ListMap[String, Int] = ListMap()
      var secondaryData: ListMap[String, Int] = ListMap()

      data.foreach { record =>
        primaryData += matchMonthNames(record.get("month").getOrElse("")) -> record.get("items").get.toInt
        secondaryData += matchMonthNames(record.get("month").getOrElse("")) -> record.get("facturas").get.toInt
      }

      val charter = new Chart()
      val imageData = charter.generateCombinedChart(primaryData, "Items", secondaryData, "Facturas", "Meses", "No. Items", "No. Facturas", s"Ventas totales $periodo / No. Facturas")
      Ok(views.html.ventas.itemsFacturas(imageData, primaryData, periodo))
    } catch {
      case e: Exception =>
        BadRequest("Couldn’t generate chart. Error: " + e.getMessage)
    }
  }

  def itemsFacturasFamilias = Action { implicit request =>
    val params = request.queryString.map { case (k, v) => k -> v.mkString }
    val periodo = params.get("periodo").getOrElse("")
    val mes = matchMonthNames(params.get("mes").getOrElse(""))
    val tempTable = params.get("src").getOrElse("")
    println(periodo + " " + mes + " " + tempTable)
    try {
      val data = dataDB.getQueryResultMap(s"""
        SELECT
           primary_product_category_id as familia,
           extract(year from invoice.invoice_date) as year,
           extract(month from invoice.invoice_date) as month,
           cast(coalesce( sum(invoice_item.quantity),0) as int) as items,
           cast(count(distinct invoice.invoice_id) as int) as facturas
        INTO TEMP $tempTable
        FROM product, invoice_item, invoice
        WHERE 1 = 1
            AND invoice.invoice_id = invoice_item.invoice_id
            AND product.product_id = invoice_item.product_id
            AND invoice.invoice_type_id = 'SALES_INVOICE'
            AND invoice.invoice_fis <> 'HISTORICA'
            AND invoice.status_id in ( 'INVOICE_READY', 'INVOICE_PAID', 'INVOICE_IN_PROCESS')
        GROUP BY 1,2,3
        ORDER BY 5 DESC;""", s"""
        SELECT
          *
        FROM $tempTable
        WHERE 1 = 1
          AND year = '$periodo'
          AND month = '$mes'""", tempTable)
      var primaryData: ListMap[String, Int] = ListMap()
      var secondaryData: ListMap[String, Int] = ListMap()

      data.foreach { record =>
        primaryData += record.get("familia").getOrElse("") -> record.get("items").get.toInt
        secondaryData += record.get("familia").getOrElse("") -> record.get("facturas").get.toInt
      }

      val imageData = chart.generateCombinedChart(primaryData, "Items", secondaryData, "Facturas", "Familias", "No. Items", "No. Facturas", s"Top 20 Ventas ${matchMonthNames(mes)} $periodo / No. Facturas")
      Ok(views.html.ventas.itemsFacturasFamilias(imageData, ListMap(primaryData.toSeq.sortBy(_._2):_*), periodo, mes))
    } catch {
      case e: Exception =>
        BadRequest("No se pudo generar la consulta, " + e.getMessage)
    }
  }

  def itemsFacturasFamilia = Action { request =>
    val params = request.queryString.map { case (k, v) => k -> v.mkString }
    val periodo = params.get("periodo").getOrElse("")
    val mes = params.get("mes").getOrElse("")
    val familia = params.get("familia").getOrElse("")
    val tempTable = params.get("src").getOrElse("")
    try {
      val records = dataDB.getQueryResultMap(s"""
        SELECT
           product.product_id,
           primary_product_category_id,
           extract(year from invoice.invoice_date) as year,
           extract(month from invoice.invoice_date) as month,
           cast(coalesce( sum(invoice_item.quantity),0) as int) as cantidad
        INTO temp $tempTable
        FROM product, invoice_item, invoice
        WHERE 1 = 1
            AND invoice.invoice_id = invoice_item.invoice_id
            AND product.product_id = invoice_item.product_id
            AND invoice.invoice_type_id = 'SALES_INVOICE'
            AND invoice.invoice_fis <> 'HISTORICA'
            AND invoice.status_id in ( 'INVOICE_READY', 'INVOICE_PAID', 'INVOICE_IN_PROCESS')
            AND primary_product_category_id = '$familia'
            AND extract(year from invoice.invoice_date) = '$periodo'
            AND extract(month from invoice.invoice_date)  = '$mes'
        GROUP BY 1,2,3,4
        ORDER BY 5 DESC;""", s"""
        SELECT
          product.product_id,
          coalesce($tempTable.cantidad, 0) as venta
        FROM
          product LEFT OUTER JOIN $tempTable ON product.product_id = $tempTable.product_id
        WHERE 1 = 1
          AND year = '$periodo'
          AND month = '$mes'
        ORDER BY 2 DESC;""", tempTable)
      var data: ListMap[String, Int] = ListMap()

      records.foreach { record =>
        data += record.get("product_id").get -> record.get("venta").get.toInt
      }

      val matriz = dataDB.getMatrixData(familia, tempTable)
      val imageData = chart.generateBarChart(data, s"Top 20 Ventas ${matchMonthNames(mes)} - $periodo Familia $familia", "Productos", "Venta")
      Ok(views.html.ventas.detalleFGMNoMain(familia, imageData, matriz))
    } catch {
      case e: Exception =>
        BadRequest("No se pudo generar la consulta, " + e.getMessage)
    }
  }

  /*
  *
  * matchValue()
  * Método que se utiliza para comparar valor de un parametro,
  * se regresa un valor de default en caso de que sea None o ""
  *
  */
  def matchValue(value: Option[String], defaultValue: String): String = {
    value match {
      case Some(s) => if (s.matches("")) defaultValue else s
      case None    => defaultValue
    }
  }


  def ventasGananciaForm = Action {
    Ok(views.html.ventas.ventasGanancia.ventasGananciaForm())
  }

  def ventasGananciaPeriodo = Action { request =>
    val params = request.queryString.map { case (k,v) => k -> v.mkString}
    val currentPeriod = Calendar.getInstance().get(Calendar.YEAR)
    val periodo = matchValue(params.get("periodo"), currentPeriod.toString)
    val tempTable = matchValue(params.get("src"), "tempTable")
    try {
      val resultMap = dataDB.getQueryResultMap(s"""
        SELECT
          PR.PRODUCT_ID,
          PR.PRIMARY_PRODUCT_CATEGORY_ID,
          sum(SI.QUANTITY) AS qty,
          sum((amount * conversion_factor)) AS converted_amount,
          (sum((amount * conversion_factor)) * sum(SI.quantity)) AS venta,
          extract(year from invoice_date) as year,
          extract(month from invoice_date) as month,
          SI.SHIPMENT_ID
        INTO TEMP $tempTable
        FROM ((((public.INVOICE INV INNER JOIN public.INVOICE_ITEM INVI ON INV.INVOICE_ID = INVI.INVOICE_ID)
          INNER JOIN public.SHIPMENT_ITEM_BILLING SIB ON INVI.INVOICE_ID = SIB.INVOICE_ID AND INVI.INVOICE_ITEM_SEQ_ID = SIB.INVOICE_ITEM_SEQ_ID)
          INNER JOIN public.PRODUCT PR ON INVI.PRODUCT_ID = PR.PRODUCT_ID)
          INNER JOIN public.UOM_CONVERSION_DATED CURR ON INV.CURRENCY_UOM_ID = CURR.UOM_ID)
          INNER JOIN public.SHIPMENT_ITEM SI ON SIB.SHIPMENT_ID = SI.SHIPMENT_ID AND SIB.SHIPMENT_ITEM_SEQ_ID = SI.SHIPMENT_ITEM_SEQ_ID
        WHERE 1 = 1
          AND CURR.UOM_ID_TO = 'CRC'
          AND CURR.THRU_DATE IS NULL
          AND INV.INVOICE_TYPE_ID = 'SALES_INVOICE'
          AND INV.invoice_fis <> 'HISTORICA'
          AND INV.status_id in ( 'INVOICE_READY', 'INVOICE_PAID', 'INVOICE_IN_PROCESS')
          AND extract(year FROM invoice_date) = '$periodo'
        GROUP BY 1, 6, 7, 8
        ORDER BY 4, 5;""", s"""
        SELECT
          cast(sum(TMP.qty) AS int) AS cantidad,
          to_char(sum(TMP.venta), '9999999999999D99') AS venta_total,
          TMP.year,
          TMP.month,
          to_char(sum(J1.amount), '9999999999999D99') AS cogs,
          to_char((sum(TMP.venta) - sum(J1.amount)), '9999999999999D99') AS ganancia
        FROM $tempTable TMP INNER JOIN (
            SELECT
              PR.product_id,
              ACT.SHIPMENT_ID,
              sum(ATE.AMOUNT) as amount
              FROM public.ACCTG_TRANS ACT INNER JOIN public.ACCTG_TRANS_ENTRY ATE ON ACT.ACCTG_TRANS_ID = ATE.ACCTG_TRANS_ID
              INNER JOIN product PR ON ATE.product_id = PR.product_id
            WHERE ATE.GL_ACCOUNT_TYPE_ID = 'COGS_ACCOUNT'
            GROUP BY 1, 2) J1 ON TMP.shipment_id = J1.shipment_id AND TMP.product_id = J1.product_id
         GROUP BY 3, 4
         ORDER BY 4;""", tempTable)
      var items: ListMap[String, Int] = ListMap()
      var ventas: ListMap[String, Double] = ListMap()
      var ganancia: ListMap[String, Double] = ListMap()

      val numberFormat = NumberFormat.getCurrencyInstance()
      var ventasCurrency: ListMap[String, String] = ListMap()
      var gananciaCurrency: ListMap[String, String] = ListMap()

      resultMap.foreach { record =>
        items += matchMonthNames(record.get("month").get) -> record.get("cantidad").get.toInt
        ventas += matchMonthNames(record.get("month").get) -> record.get("venta_total").get.toDouble
        ganancia += matchMonthNames(record.get("month").get) -> record.get("ganancia").get.toDouble
        ventasCurrency += matchMonthNames(record.get("month").get) -> numberFormat.format(record.get("venta_total").get.toDouble)
        gananciaCurrency += matchMonthNames(record.get("month").get) -> numberFormat.format(record.get("ganancia").get.toDouble)
      }
      val imageData = chart.generateDualAxisCategoryChart(ventas, "ventas", ganancia, "Ganancia", items, "Items", s"Ventas / Ganancia - $periodo", "Meses", "$", "No. Items")
      Ok(views.html.ventas.ventasGanancia.ventasGananciaPeriodo(imageData, items, ventasCurrency, gananciaCurrency, periodo))
    } catch {
      case e: Exception => BadRequest("No se pudo generar la consulta, " + e.getMessage)
    }
  }

  def ventasGananciaFamilias = Action { request =>
    val params = request.queryString.map { case (k, v) => k -> v.mkString }
    val periodo = params.get("periodo").get
    val mes = matchMonthNames(params.get("mes").get)
    val tempTable = matchValue(params.get("src"), "tempTable")
    try {
      val resultMap = dataDB.getQueryResultMap(s"""
        SELECT  PR.PRODUCT_ID, 
          PR.PRIMARY_PRODUCT_CATEGORY_ID,
          sum(SI.QUANTITY) AS qty,
          sum((amount * conversion_factor)) AS converted_amount,
          (sum((amount * conversion_factor)) * sum(SI.quantity)) AS venta,
          extract(year from invoice_date) as year,
          extract(month from invoice_date) as month,
          SI.SHIPMENT_ID
        INTO TEMP $tempTable
        FROM ((((public.INVOICE INV INNER JOIN public.INVOICE_ITEM INVI ON INV.INVOICE_ID = INVI.INVOICE_ID) 
        	INNER JOIN public.SHIPMENT_ITEM_BILLING SIB ON INVI.INVOICE_ID = SIB.INVOICE_ID AND INVI.INVOICE_ITEM_SEQ_ID = SIB.INVOICE_ITEM_SEQ_ID) 
        	INNER JOIN public.PRODUCT PR ON INVI.PRODUCT_ID = PR.PRODUCT_ID) 
        	INNER JOIN public.UOM_CONVERSION_DATED CURR ON INV.CURRENCY_UOM_ID = CURR.UOM_ID) 
        	INNER JOIN public.SHIPMENT_ITEM SI ON SIB.SHIPMENT_ID = SI.SHIPMENT_ID AND SIB.SHIPMENT_ITEM_SEQ_ID = SI.SHIPMENT_ITEM_SEQ_ID 
        WHERE 
        	CURR.UOM_ID_TO = 'CRC' 
        	AND CURR.THRU_DATE IS NULL 
        	AND INV.INVOICE_TYPE_ID = 'SALES_INVOICE'
        	AND INV.invoice_fis <> 'HISTORICA'
          AND INV.status_id in ( 'INVOICE_READY', 'INVOICE_PAID', 'INVOICE_IN_PROCESS')
        	AND extract(year FROM invoice_date) = '$periodo'
        	AND extract(month FROM invoice_date) = '$mes'
        GROUP BY 1, 6, 7, 8
        ORDER BY 2 ASC; """, s"""
        SELECT  
          TMP.primary_product_category_id as familia,
          cast(sum(TMP.qty) as int) AS cantidad,
          sum(TMP.venta) AS venta_total,
          TMP.year,
          TMP.month,
          sum(J1.amount) AS cogs,
          (sum(TMP.venta) - sum(J1.amount)) AS ganancia
        FROM $tempTable TMP INNER JOIN (
          SELECT 
            PR.product_id,
            ACT.SHIPMENT_ID, 
            sum(ATE.AMOUNT) as amount
          FROM public.ACCTG_TRANS ACT INNER JOIN public.ACCTG_TRANS_ENTRY ATE ON ACT.ACCTG_TRANS_ID = ATE.ACCTG_TRANS_ID 
            INNER JOIN product PR ON ATE.product_id = PR.product_id
          WHERE ATE.GL_ACCOUNT_TYPE_ID = 'COGS_ACCOUNT' 
          GROUP BY 1, 2) J1 ON TMP.shipment_id = J1.shipment_id AND TMP.product_id = J1.product_id
        GROUP BY 1, 4, 5
        ORDER BY 2 DESC, 3 DESC, 6 DESC; """, tempTable)

      var items: ListMap[String, Int] = ListMap()
      var ventas: ListMap[String, Double] = ListMap()
      var ganancia: ListMap[String, Double] = ListMap()

      val numberFormat = NumberFormat.getCurrencyInstance()
      var ventasCurrency: ListMap[String, String] = ListMap()
      var gananciaCurrency: ListMap[String, String] = ListMap()

      resultMap.foreach { record =>
        items += record.get("familia").get -> record.get("cantidad").get.toInt
        ventas += record.get("familia").get -> record.get("venta_total").get.toDouble
        ganancia += record.get("familia").get -> record.get("ganancia").get.toDouble
        ventasCurrency += record.get("familia").get -> numberFormat.format(record.get("venta_total").get.toDouble)
        gananciaCurrency += record.get("familia").get -> numberFormat.format(record.get("ganancia").get.toDouble)
      }
      val imageData = chart.generateDualAxisCategoryChart(ventas, "Ventas", ganancia, "Ganancia", items, "Items", s"Ventas / Ganancia - ${matchMonthNames(mes)} $periodo", "Familias", "", "No. Items")
      Ok(views.html.ventas.ventasGanancia.familias(imageData, items, ventasCurrency, gananciaCurrency, periodo))
    } catch {
      case e: Exception => BadRequest("No se pudo generar la consulta, " + e.getMessage)
    }
  }

  def createChart(data: Map[String, Int], title: String, titleX: String, titleY: String) = Action { request =>
    val sortedData = ListMap(data.toList.sortWith(_._2 > _._2):_*)
    println("DATA: " + data.mkString)
    println(data.map{case (k, v) => k + "=" + v}.mkString("&"))
    val MimeType = "image/png"
    try {
      val chart = new Chart()
      val imageData = chart.generateBarChart(sortedData, title, titleX, titleY)
      Ok(imageData).as(MimeType)
    } catch {
      case e: Exception =>
      BadRequest("Couldn’t generate chart. Error: " + e.getMessage)
    }
  }
  
}
