package controllers

import java.io.ByteArrayOutputStream

import org.jfree.chart._
import org.jfree.chart.plot.{PiePlot3D, PlotOrientation}
import org.jfree.data.DefaultKeyedValues
import org.jfree.data.general.DefaultPieDataset
import play.api._
import play.api.mvc._
import java.awt.Color
import java.io.ByteArrayInputStream

import org.jfree.chart.axis.CategoryLabelPositions
import org.jfree.chart.renderer.category.{BarPainter, BarRenderer, StandardBarPainter}
import org.jfree.data.category.{CategoryDataset, DefaultCategoryDataset}
import org.jfree.data.xy.DefaultXYDataset
import play.api.db.Databases

/**
  * A Demo to render a dynamically generated chart.
  */
class JFreeChartDemo extends Controller {

  var dataSource = Databases(
    driver = "org.postgresql.Driver",
    url = "jdbc:postgresql://192.168.1.94/ofbiz13prod",
    name = "ofbiz13prod",
    config = Map(
      "username" -> "ofbiz",
      "password" -> "Duke.2244"
    )
  )


  def show = Action(
    implicit request =>
      Ok(views.html.jFreeChartDemo()))


  def chart = Action {

    val MimeType = "image/png"
    try {
      val imageData = generateBarChart()

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


  private def generateBarChart():Array[Byte] = {

    /* var DBConn = database.getConnection()
    var statement = DBConn.createStatement()
    val test = statement.execute("DROP TABLE IF EXISTS mytable")
    val createTempTable = statement.execute("""select product.product_id, product.primary_product_category_id, coalesce( sum(invoice_item.quantity),0) as cantidad
                               into temp mytable
                               from product, invoice_item, invoice
                              	where 1 = 1
                                  and invoice.invoice_id = invoice_item.invoice_id
                                  and product.product_id = invoice_item.product_id
                                  and invoice.invoice_type_id = 'SALES_INVOICE'
                                  and invoice.invoice_fis <> 'HISTORICA'
                                  and invoice.status_id in ( 'INVOICE_READY', 'INVOICE_PAID', 'INVOICE_IN_PROCESS')
                                  and invoice.invoice_date >= '2016-01-01'
                                  and invoice.invoice_date <=  '2016-01-30'
                                  and product.primary_product_category_id = 'ILT-142'
                              group by 1, 2
                              order by 1,2;""")
    //DBConn.prepareStatement(.execute()


    var ventaPeriodo = statement.executeQuery(
      """select product.product_id, coalesce( mytable.cantidad, 0) as venta
                                                             from product left outer join mytable on product.product_id = mytable.product_id
                                                             where 1=1
                                                             	and product.primary_product_category_id = 'ILT-142'
                                                             order by 2 DESC limit 20;""")

    val values = new DefaultCategoryDataset()

    var width = 0
    val height = 400

    while (ventaPeriodo.next()) {
      val productId = ventaPeriodo.getString("product_id")
      val venta = ventaPeriodo.getInt("venta")
      values.addValue(venta, "", productId)
      width += 50
    }

    ventaPeriodo.close()
    statement.close()
    DBConn.close()
    */

    val connection = dataSource.getConnection();
    val values = new DefaultCategoryDataset()

    var width = 0
    val height = 400

    try {
      val statement = connection.createStatement();

      try {
        statement.execute("DROP TABLE IF EXISTS mytable")
        statement.execute("""SELECT product.product_id, product.primary_product_category_id, coalesce( sum(invoice_item.quantity),0) AS cantidad
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
                                 AND product.primary_product_category_id = 'ILT-142'
                               GROUP BY 1, 2
                               ORDER BY 1,2;""")

        val resultSet = statement.executeQuery("""SELECT product.product_id, coalesce( mytable.cantidad, 0) as venta
                                                  FROM product left outer join mytable on product.product_id = mytable.product_id
                                                  WHERE  1=1
                                                    AND product.primary_product_category_id = 'ILT-142'
                                                  ORDER BY 2 DESC LIMIT 20;""");

        try {
          // Do stuff with the result set.
          while (resultSet.next()) {
            val productId = resultSet.getString("product_id")
            val venta = resultSet.getInt("venta")
            values.addValue(venta, "", productId)
            width += 50
          }

        } finally {
          resultSet.close();
        }
      } finally {
        statement.close();
      }
    } finally {
      connection.close();
    }

    val chart = ChartFactory.createBarChart(
      "Top 20 Venta por Familia Enero 2016",
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

}
