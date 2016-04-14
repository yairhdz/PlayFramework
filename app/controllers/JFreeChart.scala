package controllers

import java.io.ByteArrayOutputStream

import org.jfree.chart.{ChartFactory, ChartUtilities}
import org.jfree.chart.axis.CategoryLabelPositions
import org.jfree.chart.plot.PlotOrientation
import org.jfree.data.category.DefaultCategoryDataset

/**
  * Created by yair on 13/04/16.
  */
class JFreeChart {

  def generateBarChart(data: Map[String, Int], title: String, titleX: String, titleY: String): Array[Byte] = {
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
        top += 1
      }
    }

    if (data.size < 5) {
      width = 300
    }

    val chart = ChartFactory.createBarChart(
      title,
      titleX,
      titleY,
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

    byteArray.toByteArray()
  }

}
