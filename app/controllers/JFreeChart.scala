package controllers

import java.awt.{BasicStroke, Color}
import java.io.ByteArrayOutputStream

import org.jfree.chart.{ChartFactory, ChartUtilities}
import org.jfree.chart.axis.{CategoryLabelPositions, NumberAxis}
import org.jfree.chart.block.{BlockBorder, BlockContainer, BorderArrangement, EmptyBlock}
import org.jfree.chart.labels.StandardXYToolTipGenerator
import org.jfree.chart.plot.PlotOrientation
import org.jfree.chart.renderer.xy.{StandardXYItemRenderer, XYItemRenderer, XYLineAndShapeRenderer}
import org.jfree.chart.title.{CompositeTitle, LegendTitle}
import org.jfree.data.category.DefaultCategoryDataset
import org.jfree.data.xy.{XYSeries, XYSeriesCollection}
import org.jfree.ui.{RectangleEdge, RectangleInsets}

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

    val image = chart.createBufferedImage(width, height)
    val byteArray = new ByteArrayOutputStream()
    ChartUtilities.writeBufferedImageAsPNG(byteArray, image)

    byteArray.toByteArray()
  }

  def generateXYChart = {
    val firefox = new XYSeries( "Firefox" )
    firefox.add( 1.0 , 1.0 )
    firefox.add( 2.0 , 4.0 )
    firefox.add( 3.0 , 3.0 )

    val chrome = new XYSeries( "Chrome" )
    chrome.add( 1.0 , 4.0 )
    chrome.add( 2.0 , 5.0 )
    chrome.add( 3.0 , 6.0 )

    val iexplorer = new XYSeries( "InternetExplorer" )
    iexplorer.add( 2.0 , 4.0 )
    iexplorer.add( 3.0 , 5.0 )
    iexplorer.add( 4.0 , 2.0 )
    iexplorer.add( 5.0 , 8.0 )
    iexplorer.add( 6.0 , 3.0 )
    iexplorer.add( 7.0 , 4.0 )

    val dataset = new XYSeriesCollection( )
    dataset.addSeries( firefox )
    dataset.addSeries( chrome )

    val dataset2 = new XYSeriesCollection( )
    dataset2.addSeries( iexplorer )

    val xylineChart = ChartFactory.createXYLineChart(
      "Browser usage statastics",
      "Category",
      "Score",
      dataset,
      PlotOrientation.VERTICAL,
      false, true, false)

    val width = 640; /* Width of the image */
    val height = 480; /* Height of the image */

    val plot = xylineChart.getXYPlot()
    plot.setBackgroundPaint(Color.white)
    plot.setDomainGridlinePaint(Color.white)
    plot.setRangeGridlinePaint(Color.white)
    plot.setOutlineVisible(false)

    val secondaryAxisRange = new NumberAxis("Secondary")
    secondaryAxisRange.setAutoRangeIncludesZero(false)

    plot.setRangeAxis(1, secondaryAxisRange)
    plot.setDataset(1, dataset2)
    plot.mapDatasetToRangeAxis(1, 1)

    val primaryRenderer = plot.getRenderer()
    primaryRenderer.setSeriesPaint(1, Color.CYAN)

    val secondaryRenderer = new XYLineAndShapeRenderer();
    secondaryRenderer.setSeriesPaint(0, Color.black)
    secondaryRenderer.setBaseShapesVisible(true)
    plot.setRenderer(1, secondaryRenderer)

    val primaryLegendTitle = new LegendTitle(primaryRenderer);
    val secondaryLegendTitle = new LegendTitle(secondaryRenderer);
    primaryLegendTitle.setPosition(RectangleEdge.RIGHT)
    secondaryLegendTitle.setPosition(RectangleEdge.RIGHT)

    val localBlockContainer = new BlockContainer(new BorderArrangement());
    localBlockContainer.add(primaryLegendTitle, RectangleEdge.LEFT);
    localBlockContainer.add(secondaryLegendTitle, RectangleEdge.RIGHT);
    localBlockContainer.add(new EmptyBlock(width.toDouble - 250, 0.0D));
    localBlockContainer.setPadding(0.0, 0.0, 0.0, -50.0)

    val localCompositeTitle = new CompositeTitle(localBlockContainer);
    localCompositeTitle.setPosition(RectangleEdge.BOTTOM);

    xylineChart.addSubtitle(localCompositeTitle);

    for (j <- 0 until plot.getRendererCount; i <- 0 until  plot.getSeriesCount) {
      val renderer = plot.getRenderer(j)
      renderer.setSeriesStroke(i, new BasicStroke(2))
    }

    val image = xylineChart.createBufferedImage(width, height)
    val byteArray = new ByteArrayOutputStream()
    ChartUtilities.writeBufferedImageAsPNG(byteArray, image)
    byteArray.toByteArray()
  }

}
