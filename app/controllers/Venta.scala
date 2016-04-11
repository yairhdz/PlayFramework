package controllers

import java.io.ByteArrayOutputStream
import javax.inject.Inject

import org.jfree.chart.{ChartFactory, ChartUtilities}
import org.jfree.chart.axis.CategoryLabelPositions
import org.jfree.chart.plot.PlotOrientation
import org.jfree.data.category.DefaultCategoryDataset
import play.api.db.Database
import play.api.mvc._

import scala.collection.immutable.ListMap

/**
  * Created by yair on 11/04/16.
  */
 class Venta @Inject()(db: Database) extends Controller {

  def showVentaFamilias() = Action {
    val data = getData()
    Ok(views.html.ventas(data))
   }

  def getData(): Map[String, Int] = {
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

  def createChart() = Action {
    val MimeType = "image/png"
    try {
      val data = getData()
      val imageData = generateBarChart(data)
      //Redirect(routes.Venta.showVentaFamilias(imageData))
      Ok(imageData).as(MimeType)
    } catch {
      case e: Exception =>
      BadRequest("Couldn’t generate chart. Error: " + e.getMessage)
    }
  }

  def generateBarChart(data: Map[String, Int]): Array[Byte] = {
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
}
