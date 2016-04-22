package controllers

import java.awt.Color
import java.io.ByteArrayOutputStream
import javax.inject.Inject

import org.jfree.chart._
import org.jfree.chart.axis.CategoryLabelPositions
import org.jfree.chart.plot.{PiePlot3D, PlotOrientation}
import org.jfree.data.DefaultKeyedValues
import org.jfree.data.category.DefaultCategoryDataset
import org.jfree.data.general.DefaultPieDataset
import play.api.db._
import play.api.mvc._

import scala.collection.immutable.ListMap
import scala.collection.mutable.ArrayBuffer

/**
  * A Demo to render a dynamically generated chart.
  */

class JFreeChartDemo @Inject()(db: Database) extends Controller {

  def show = Action{
    implicit request =>
      Ok(views.html.jFreeChartDemo())
  }

  def chart = Action { request=>
    println("REQUEST: " + request.queryString.map { case (k,v) => k -> v.mkString })
    val params = request.queryString.map { case (k,v) => k -> v.mkString }

    val familia: String = params.get("familia").get
    val data = getData(familia)
    val matriz = getMatrixData(familia)

    // val data = Map("uno" -> "dos")
    val MimeType = "image/png"
    try {
      val level = 1
      val list = Seq("uno", "dos")
      // val imageData = generateBarChart(familia)
      Ok(views.html.reporte(familia, data, matriz))
    } catch {
      case e: Exception =>
        BadRequest("Couldn’t generate chart. Error: " + e.getMessage)
    }
  }

  def getChart(familia: String) = Action { request=>
    // println("REQUEST: " + request.queryString.map { case (k,v) => k -> v.mkString })
    // val params = request.queryString.map { case (k,v) => k -> v.mkString }
    // val familia: String = params.get("familia").get
    val MimeType = "image/png"
    try {
      // val level = 1
      // val list = Seq("uno", "dos")
      val data = getData(familia)
      val imageData = generateBarChart(data)
      Ok(imageData).as(MimeType)
    } catch {
      case e: Exception =>
        BadRequest("Couldn’t generate chart. Error: " + e.getMessage)
    }
  }

  def matchMonths(month: Int) = {
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

  def demo = Action { request=>
    val MimeType = "image/png"
    try {
      val dataDB = new Data(db)
      val data = dataDB.getVentasAllColumns("""select 	
                                                 extract(year from invoice.invoice_date) as year, 
                                                 extract(month from invoice.invoice_date) as month, 
                                                 cast(coalesce( sum(invoice_item.quantity),0) as int) as items,
                                                 count(distinct invoice.invoice_id) as facturas
                                              into temp total_ventas_facturas
                                              from product, invoice_item, invoice
                                              where 1 = 1
                                                  and invoice.invoice_id = invoice_item.invoice_id
                                                  and product.product_id = invoice_item.product_id
                                                  and invoice.invoice_type_id = 'SALES_INVOICE'
                                                  and invoice.invoice_fis <> 'HISTORICA'
                                                  and invoice.status_id in ( 'INVOICE_READY', 'INVOICE_PAID', 'INVOICE_IN_PROCESS')
                                              group by 1,2
                                              order by 2;""", "select * from total_ventas_facturas where year = '2015'", "total_ventas_facturas")

      var primaryData: ListMap[String, Int] = ListMap()
      var secondaryData: ListMap[String, Int] = ListMap()

      data.foreach { record =>
          primaryData += matchMonths(record.get("month").get.toInt) -> record.get("items").get.toInt
          secondaryData += matchMonths(record.get("month").get.toInt) -> record.get("facturas").get.toInt
      }

//      primaryData = ListMap(primaryData.toList.sortWith(_._1.toInt < _._1.toInt):_*)
//      secondaryData = ListMap(secondaryData.toList.sortWith(_._1.toInt < _._1.toInt):_*)
      println(primaryData.getClass)

      val charter = new Chart()
      val imageData = charter.generateCombinedChart(primaryData, "Items", secondaryData, "Facturas", "Meses", "No. Items", "No. Facturas", "Ventas totales / No. Facturas")
      Ok(imageData).as(MimeType)
    } catch {
      case e: Exception =>
        BadRequest("Couldn’t generate chart. Error: " + e.getMessage)
    }
  }

  private def generateChart():Array[Byte] = {
    val values = new DefaultKeyedValues()
    values.addValue("Firefox", 39)
    values.addValue("Internet Explorer", 20)
    values.addValue("Chrome", 10)
    values.addValue("Safari", 15)
    val pieDataSet = new DefaultPieDataset(values)

    val width = 600
    val height = 400

    val chart = ChartFactory.createPieChart3D(
      "Pie Chart Demo", // chart title
      pieDataSet,
      true, // include legend
      true, // tooltips
      false // urls
    );

    val plot: PiePlot3D = chart.getPlot().asInstanceOf[PiePlot3D]
    //plot.setBackgroundPaint(Color.white)
    plot.setBackgroundAlpha(0.2f)
    plot.setForegroundAlpha(0.5f)
    plot.setDepthFactor(0.1);
    plot.setLabelPaint(Color.BLUE)
    plot.setLabelBackgroundPaint(Color.orange)

    val image = chart.createBufferedImage(width, height);
    val byteArray = new ByteArrayOutputStream();
    ChartUtilities.writeBufferedImageAsPNG(byteArray, image);
    return byteArray.toByteArray()
  }


  private def generateBarChart(data: Map[String, Int]):Array[Byte] = {

    val values = new DefaultCategoryDataset()

    var width = 0
    val height = 400

    var top = 0
    data.foreach { case (k, v) =>
      if (top < 20) {
        val productId = k
        val venta = v
        values.addValue(venta, "", productId)
        width += 50
        top = top + 1
      }

    }

    //   while (resultSet.next()) {
    //     val productId = resultSet.getString("product_id")
    //     val venta = resultSet.getInt("venta")
    //     values.addValue(venta, "", productId)
    //     width += 50
    //   }
    // }

    val chart = ChartFactory.createBarChart(
      s"Top 20 Venta Familia familia Enero 2016",
      "Productos",
      "Venta",
      values,
      PlotOrientation.VERTICAL,
      false, false, false
    )

    val plot = chart.getCategoryPlot()
    val axis = plot.getDomainAxis()
    axis.setCategoryLabelPositions(CategoryLabelPositions.UP_90)

    val image = chart.createBufferedImage(width, height);
    val byteArray = new ByteArrayOutputStream();
    ChartUtilities.writeBufferedImageAsPNG(byteArray, image);

    return byteArray.toByteArray()
  }

  def getData(familia: String): Map[String, Int] = {
    var data: Map[String, Int] = Map()
    db.withConnection { connection =>
      val statement = connection.createStatement();
      statement.execute("DROP TABLE IF EXISTS mytable")
      statement.execute( s"""SELECT product.product_id, product.primary_product_category_id, coalesce( sum(invoice_item.quantity),0) AS cantidad
                             INTO TEMP mytable
                               FROM product, invoice_item, invoice
                               WHERE 1 = 1
                                 AND invoice.invoice_id = invoice_item.invoice_id
                                 AND product.product_id = invoice_item.product_id
                                 AND invoice.invoice_type_id = 'SALES_INVOICE'
                                 AND invoice.invoice_fis <> 'HISTORICA'
                                 AND invoice.status_id in ( 'INVOICE_READY', 'INVOICE_PAID', 'INVOICE_IN_PROCESS')
                                 AND invoice.invoice_date >= '2016-01-01'
                                 AND invoice.invoice_date <=  '2016-01-30'
                                 AND product.primary_product_category_id = '${familia}'
                               GROUP BY 1, 2
                               ORDER BY 1,2;""")

      val resultSet = statement.executeQuery( s"""SELECT product.product_id, coalesce( mytable.cantidad, 0) as venta
                                                  FROM product left outer join mytable on product.product_id = mytable.product_id
                                                  WHERE  1=1
                                                    AND product.primary_product_category_id = '${familia}'
                                                  ORDER BY 2 DESC;""");

      while (resultSet.next()) {
        val productId = resultSet.getString("product_id")
        val venta = resultSet.getInt("venta")
        data += productId -> venta
      }
    }
    data = ListMap(data.toSeq.sortWith(_._2 > _._2):_*)

    println(data)

    return  data
  }


  def getMatrixData(familia: String): (Array[Array[String]]) = {
    var matriz: Map[String, ArrayBuffer[String]] = Map()
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
      val prod = statement.executeQuery(
        s"""select product.product_id, product.posicion_matriz as pos,
                                           to_char(coalesce( mytable.cantidad, 0), '999G999D') as venta
                                           from product left outer join mytable on product.product_id = mytable.product_id
                                           where 1=1
                                           	and product.primary_product_category_id = '${familia}'
                                           order by 2""")
      while(prod.next()){
        val pos = prod.getInt("pos")
        val venta = prod.getString("venta")
        prods +=  pos -> venta
      }
    }
    prods = ListMap(prods.toSeq.sortBy(_._1):_*)

    matriz += ("ejeX" -> ejeX)
    matriz += "ejeY" -> ejeY

    val numRenglones = ejeY.length
    val numColumnas =   ejeX.length

    var mat = Array.ofDim[String](numRenglones + 1, numColumnas + 1)

    var i = 1
    var j = 1
    for(value <- ejeX){
      mat(0)(i) = value
      i = i + 1
    }
    for (valueY <- ejeY) {
      mat(j)(0) = valueY
      j = j + 1
    }

    var maxX = numColumnas

    val maxSize = ejeX.length
    var renglonb = 1

    var newIdx = 1
    prods.foreach { case (k, v) =>
      if (renglonb <= numRenglones) {
        if (k <= maxSize) {
          mat(1)(k) = v
        } else {
          if (newIdx <= maxSize) {
            mat(renglonb)(newIdx) = v
          } else {
            newIdx = 1
            renglonb = renglonb + 1
            mat(renglonb)(newIdx) = v
          }
        }
        newIdx = newIdx + 1
      }
    }

    print(mat.map(_.mkString).mkString(" \n "))

    return mat
  }

}
