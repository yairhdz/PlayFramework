package controllers

import java.text.NumberFormat
import java.util.Calendar
import javax.inject.Inject

import play.api.db.Database
import play.api.mvc._

import scala.collection.immutable.ListMap

/**
  * Created by yair on 11/04/16.
  */
 class Venta @Inject()(db: Database, dataDB: Data, chart: Chart) extends Controller {

  /**
    *
    * ventasPeriodo
    * Action que muestra la vista de ventas de todo el periodo por familia(ventasPeriodo).
    *
    */
  def ventasPeriodo = Action { request =>
    val params = request.queryString.map { case (k, v) => k -> v.mkString }
    val tempTable = params.getOrElse("src", "tempTable")
    try {
      val resultMap = dataDB.getQueryResultMap(s"""
        SELECT
          product.primary_product_category_id AS familia,
          cast(coalesce(sum(invoice_item.quantity),0) AS int) AS cantidad
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
        ORDER BY 2 DESC;""", s"SELECT * FROM $tempTable", tempTable)

      var data: ListMap[String, Int] = ListMap()

      resultMap.foreach { record =>
        data += record.get("familia").get -> record.get("cantidad").get.toInt
      }
      val imageData = chart.generateBarChart(data, "Top 20 Ventas por familia", "Familias", "Venta")
      Ok(views.html.ventas.totales.familias(imageData, data))
    } catch {
      case e: Exception =>
        BadRequest("No se pudo generar la consulta, " + e.getMessage)
    }
  }

  /**
    *
    * ventasPeriodoFamilia
    * Action que muestra la vista de ventas de todo el periodo de una familia mostrando la matriz
    * de productos.
    *
    */
  def ventasPeriodoFamilia = Action { request =>
    val params = request.queryString.map { case (k,v) => k -> v.mkString }
    val tempTable = params.getOrElse("src", "tempTable")
    val familia   = params.get("familia").get
    try {
      val resultMap = dataDB.getQueryResultMap(s"""
        SELECT
          product.product_id,
          product.primary_product_category_id AS familia,
          cast(coalesce( sum(invoice_item.quantity),0) AS int) AS cantidad
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
          coalesce( $tempTable.cantidad, 0) as cantidad
        FROM
          product LEFT OUTER JOIN $tempTable ON product.product_id = $tempTable.product_id
        WHERE  1 = 1
          AND product.primary_product_category_id = '$familia'
        ORDER BY 2 DESC;""", tempTable)

      var data: ListMap[String, Int] = ListMap()

      resultMap.foreach { record =>
        data += record.get("product_id").get -> record.get("cantidad").get.toInt
      }

      val matriz = dataDB.getMatrixData(familia, tempTable)
      val imageData = chart.generateBarChart(data, s"Top 20 Ventas $familia", "Productos", "Venta")
      Ok(views.html.ventas.detalleFamiliaMatrizNavbar(familia, imageData,matriz))
    } catch {
      case e: Exception =>
        BadRequest("No se pudo generar la consulta, " + e.getMessage)
    }
  }

  /**
    *
    * ventasPorPeriodoForm
    * Action que muestra la vista del form para generar la grafica de ventasPorperiodo.
    *
    */
  def ventasPorPeriodoForm = Action {
    Ok(views.html.ventas.porPeriodo.form())
  }

  /**
    *
    * ventasPorPeriodo
    * Action que muestra la vista de ventas por familias del periodo ingresado el ventasPorPeriodoForm.
    *
    */
  def ventasPorPeriodo = Action { request =>
    val params = request.queryString.map { case(k,v) => k -> v.mkString}
    val periodoInicio = params.getOrElse("inicio", "")
    val periodoFin    = params.getOrElse("fin", "")
    val tempTable     = params.getOrElse("src", "tempTable")
    try {
      val resultMap = dataDB.getQueryResultMap(s"""
        SELECT
          product.primary_product_category_id AS familia,
          cast(coalesce( sum(invoice_item.quantity),0) AS int) as cantidad
        INTO TEMP $tempTable
        FROM product, invoice_item, invoice
        WHERE 1 = 1
          AND invoice.invoice_id = invoice_item.invoice_id
          AND product.product_id = invoice_item.product_id
          AND invoice.invoice_type_id = 'SALES_INVOICE'
          AND invoice.invoice_fis <> 'HISTORICA'
          AND invoice.status_id in ('INVOICE_READY', 'INVOICE_PAID', 'INVOICE_IN_PROCESS')
          AND invoice.invoice_date >= '$periodoInicio'
          AND invoice.invoice_date <=  '$periodoFin'
        GROUP BY 1
        ORDER BY 2 DESC;
        """, s"SELECT * FROM $tempTable", tempTable)

      var data: ListMap[String, Int] = ListMap()

      resultMap.foreach { record =>
        data += record.get("familia").get -> record.get("cantidad").get.toInt
      }

      val imageData = chart.generateBarChart(data, s"Top 20 Ventas, periodo $periodoInicio - $periodoFin", "Familias", "Venta")
      Ok(views.html.ventas.porPeriodo.familias(imageData, data, periodoInicio, periodoFin))
    } catch {
      case e: Exception =>
        BadRequest("No se pudo generar la consulta, " + e.getMessage)
    }

  }

  /**
    *
    * ventasPorPeriodoFamilia
    * Action que muestra la vista de ventas de una familia del periodo ingresado el ventasPorPeriodoForm y la
    * familia seleccionada en ventasPorPeriodo.
    *
    */
  def ventasPorPeriodoFamilia = Action { request =>
    val params = request.queryString.map { case(k,v) => k -> v.mkString}
    val periodoInicio = params.getOrElse("inicio", "")
    val periodoFin    = params.getOrElse("fin", "")
    val familia       = params.getOrElse("familia", "")
    val tempTable     = params.getOrElse("src", "tempTable")

    try {
      val resultMap = dataDB.getQueryResultMap(s"""
        SELECT
          product.product_id,
          product.primary_product_category_id,
          coalesce( sum(invoice_item.quantity),0) as cantidad
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
        """, s"""
        SELECT
          product.product_id,
          cast(coalesce( $tempTable.cantidad, 0) AS int) AS cantidad
        FROM product left outer join $tempTable on product.product_id = $tempTable.product_id
        WHERE  1=1
          AND product.primary_product_category_id = '$familia'
        ORDER BY 2 DESC;""", tempTable)

      var data: ListMap[String, Int] = ListMap()

      resultMap.foreach { record =>
        data += record.get("product_id").get -> record.get("cantidad").get.toInt
      }

      val matriz = dataDB.getMatrixData(familia, tempTable)
      val imageData = chart.generateBarChart(data, s"Top 20 Ventas $familia, periodo $periodoInicio - $periodoFin", "Productos", "Venta")
      Ok(views.html.ventas.detalleFamiliaMatrizNoNavbar(familia, imageData, matriz))
    } catch {
      case e: Exception =>
        BadRequest("No se pudo generar la consula, " + e.getMessage)
    }
  }

  /**
    *
    * matchMonthNames
    * Método que se utiliza para cambiar el nombre de los meses ya sea de numero a nombre correspondiente
    * o viceversa.
    *
    */
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

  /**
    *
    * itemsFacturasForm
    * Action que muestra la vista del form para generar la gráfica de ventas por items y número de facturas.
    *
    */
  def itemsFacturasForm = Action {
    Ok(views.html.ventas.itemsFacturas.form())
  }

  /**
    *
    * itemsFacturas
    * Action que muestra las ventas y numero de facturas por mes del periodo ingresado en itemsFacturasForm.
    *
    */
  def itemsFacturas = Action { request =>
    val params = request.queryString.map { case (k,v) => k -> v.mkString}
    val currentPeriod = Calendar.getInstance().get(Calendar.YEAR)
    val periodo = matchValue(params.get("periodo"), currentPeriod.toString)
    val tempTable = matchValue(params.get("src"), "tempTable")
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
        primaryData += matchMonthNames(record.get("month").get) -> record.get("items").get.toInt
        secondaryData += matchMonthNames(record.get("month").get) -> record.get("facturas").get.toInt
      }

      val charter = new Chart()
      val imageData = charter.generateCombinedChart(primaryData, "Items", secondaryData, "Facturas", "Meses", "No. Items", "No. Facturas", s"Ventas totales / No. Facturas - $periodo")
      Ok(views.html.ventas.itemsFacturas.periodo(imageData, primaryData, periodo))
    } catch {
      case e: Exception =>
        BadRequest("Couldn’t generate chart. Error: " + e.getMessage)
    }
  }

  /**
    *
    * itemsFacturasFamilias
    * Action que muestra las ventas y numero de facturas por familias del mes y periodo seleccionados
    *
    */
  def itemsFacturasFamilias = Action { implicit request =>
    val params = request.queryString.map { case (k, v) => k -> v.mkString }
    val periodo = params.getOrElse("periodo", "")
    val tempTable = params.getOrElse("src", "tempTable")
    val mes = matchMonthNames(params.getOrElse("mes", ""))
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
        primaryData += record.get("familia").get -> record.get("items").get.toInt
        secondaryData += record.get("familia").get -> record.get("facturas").get.toInt
      }

      val imageData = chart.generateCombinedChart(primaryData, "Items", secondaryData, "Facturas", "Familias", "No. Items", "No. Facturas", s"Top 20 Ventas / No. Facturas - ${matchMonthNames(mes)} $periodo")
      Ok(views.html.ventas.itemsFacturas.familias(imageData, ListMap(primaryData.toSeq.sortBy(_._2):_*), periodo, mes))
    } catch {
      case e: Exception =>
        BadRequest("No se pudo generar la consulta, " + e.getMessage)
    }
  }

  /**
    *
    * itemsFacturasFamilia
    * Action que muestra las ventas y numero de facturas de la familia, mes y periodo seleccionados,
    * mostrando la matiz de productos.
    *
    */
  def itemsFacturasFamilia = Action { request =>
    val params = request.queryString.map { case (k, v) => k -> v.mkString }
    val periodo = params.getOrElse("periodo", "")
    val mes = params.getOrElse("mes", "")
    val familia = params.getOrElse("familia", "")
    val tempTable = params.getOrElse("src", "tempTable")
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
      val imageData = chart.generateBarChart(data, s"Top 20 Ventas / No. Facturas $familia - ${matchMonthNames(mes)} $periodo", "Productos", "Venta")
      Ok(views.html.ventas.detalleFamiliaMatrizNoNavbar(familia, imageData, matriz))
    } catch {
      case e: Exception =>
        BadRequest("No se pudo generar la consulta, " + e.getMessage)
    }
  }

  /**
    *
    * matchValue
    * Método que se utiliza para comparar valor de un parametro, se regresa un valor de default en caso
    * de que sea None o "" utilizado para parametros ingresados en forms.
    *
    */
  def matchValue(value: Option[String], defaultValue: String): String = {
    value match {
      case Some(s) => if (s.matches("")) defaultValue else s
      case None    => defaultValue
    }
  }


  /**
    *
    * ventasGananciaForm
    * Action que muestra la vista del form para generar la gráfica de ventas / ganancia
    *
    */
  def ventasGananciaForm = Action {
    Ok(views.html.ventas.ventasGanancia.form())
  }

  /**
    *
    * ventasGananciaPeriodo
    * Action que muestra la grafica de ventas / ganancia por mes del periodo ingresado en ventasGananciaForm.
    *
    */
  def ventasGananciaPeriodo = Action { request =>
    val params = request.queryString.map { case (k,v) => k -> v.mkString}
    val currentPeriod = Calendar.getInstance().get(Calendar.YEAR)
    val periodo = matchValue(params.get("periodo"), currentPeriod.toString)
    val tempTable = params.getOrElse("src", "tempTable")
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
      Ok(views.html.ventas.ventasGanancia.periodo(imageData, items, ventasCurrency, gananciaCurrency, periodo))
    } catch {
      case e: Exception => BadRequest("No se pudo generar la consulta, " + e.getMessage)
    }
  }

  /**
    *
    * ventasGananciaPeriodoFamilias
    * Action que muestra la grafica de ventas / ganancia por familias del periodo y mes seleccionados.
    *
    */
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
      val imageData = chart.generateDualAxisCategoryChart(ventas, "Ventas", ganancia, "Ganancia", items, "Items", s"Top 20 Ventas / Ganancia - ${matchMonthNames(mes)} $periodo", "Familias", "", "No. Items")
      Ok(views.html.ventas.ventasGanancia.familias(imageData, items, ventasCurrency, gananciaCurrency, periodo, mes))
    } catch {
      case e: Exception => BadRequest("No se pudo generar la consulta, " + e.getMessage)
    }
  }

  /**
    *
    * ventasGananciaPeriodoFamilia
    * Action que muestra la grafica de ventas / ganancia de la familia, periodo y mes seleccionados,
    * mostrando la matriz de productos.
    *
    */
  def ventasGananciaFamilia = Action { request =>
    val params = request.queryString.map { case (k, v) => k -> v.mkString }
    val periodo = params.get("periodo").get
    val mes = matchMonthNames(params.get("mes").get)
    val tempTable = matchValue(params.get("src"), "tempTable")
    val familia = params.get("familia").get
    try {
      val resultMap = dataDB.getQueryResultMap(s"""
        SELECT
          PR.PRODUCT_ID,
          PR.PRIMARY_PRODUCT_CATEGORY_ID,
          sum(SI.QUANTITY) AS cantidad,
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
        	AND extract(month FROM invoice_date) = '${matchMonthNames(mes)}'
          AND primary_product_category_id = '$familia'
        GROUP BY 1, 6, 7, 8
        ORDER BY 1, 2; """, s"""
        SELECT
          TMP.product_id,
          TMP.primary_product_category_id as familia,
          cast(sum(TMP.cantidad) as int) AS qty,
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
        GROUP BY 1, 2, 5, 6
        ORDER BY 3 DESC, 4 DESC, 7 DESc; """, tempTable)

      val matriz = dataDB.getMatrixData(familia, tempTable)

      var items: ListMap[String, Int] = ListMap()
      var ventas: ListMap[String, Double] = ListMap()
      var ganancia: ListMap[String, Double] = ListMap()

      val numberFormat = NumberFormat.getCurrencyInstance()
      var ventasCurrency: ListMap[String, String] = ListMap()
      var gananciaCurrency: ListMap[String, String] = ListMap()

      resultMap.foreach { record =>
        items += record.get("product_id").get -> record.get("qty").get.toInt
        ventas += record.get("product_id").get -> record.get("venta_total").get.toDouble
        ganancia += record.get("product_id").get -> record.get("ganancia").get.toDouble
        ventasCurrency += record.get("product_id").get -> numberFormat.format(record.get("venta_total").get.toDouble)
        gananciaCurrency += record.get("product_id").get -> numberFormat.format(record.get("ganancia").get.toDouble)
      }
      val imageData = chart.generateDualAxisCategoryChart(ventas, "Ventas", ganancia, "Ganancia", items, "Items", s"Top 20 Ventas / Ganancia $familia - $mes $periodo", "Productos", "", "No. Items")
      Ok(views.html.ventas.detalleFamiliaMatrizNoNavbar(familia, imageData, matriz))
    } catch {
      case e: Exception => BadRequest("No se pudo generar la consulta, " + e.getMessage)
    }
  }

}
