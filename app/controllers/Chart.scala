package controllers

import java.awt.{BasicStroke, Color, Font}
import java.io.ByteArrayOutputStream

import com.google.common.io.BaseEncoding
import org.jfree.chart.{ChartFactory, ChartUtilities, JFreeChart}
import org.jfree.chart.axis.{CategoryAxis, CategoryLabelPositions, NumberAxis}
import org.jfree.chart.block.{BlockBorder, BlockContainer, BorderArrangement, EmptyBlock}
import org.jfree.chart.labels._
import org.jfree.chart.plot.{CategoryPlot, CombinedDomainCategoryPlot, PlotOrientation}
import org.jfree.chart.renderer.category.{BarRenderer, LineAndShapeRenderer, StandardBarPainter}
import org.jfree.chart.renderer.xy.{StandardXYItemRenderer, XYItemRenderer, XYLineAndShapeRenderer}
import org.jfree.chart.title.{CompositeTitle, LegendTitle}
import org.jfree.data.category.DefaultCategoryDataset
import org.jfree.data.xy.{XYSeries, XYSeriesCollection}
import org.jfree.ui._

import scala.collection.immutable.ListMap
import scala.collection.mutable.ArrayBuffer

/**
  * Created by yair on 13/04/16.
  */
class Chart {

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

    val secondaryRenderer = new XYLineAndShapeRenderer()
    secondaryRenderer.setSeriesPaint(0, Color.black)
    secondaryRenderer.setBaseShapesVisible(true)
    plot.setRenderer(1, secondaryRenderer)

    val primaryLegendTitle = new LegendTitle(primaryRenderer)
    val secondaryLegendTitle = new LegendTitle(secondaryRenderer)
    primaryLegendTitle.setPosition(RectangleEdge.RIGHT)
    secondaryLegendTitle.setPosition(RectangleEdge.RIGHT)

    val localBlockContainer = new BlockContainer(new BorderArrangement())
    localBlockContainer.add(primaryLegendTitle, RectangleEdge.LEFT)
    localBlockContainer.add(secondaryLegendTitle, RectangleEdge.RIGHT)
    localBlockContainer.add(new EmptyBlock(width.toDouble - 250, 0.0D))
    localBlockContainer.setPadding(0.0, 0.0, 0.0, -50.0)

    val localCompositeTitle = new CompositeTitle(localBlockContainer)
    localCompositeTitle.setPosition(RectangleEdge.BOTTOM)

    xylineChart.addSubtitle(localCompositeTitle)

    for (j <- 0 until plot.getRendererCount; i <- 0 until  plot.getSeriesCount) {
      val renderer = plot.getRenderer(j)
      renderer.setSeriesStroke(i, new BasicStroke(2))
    }

    val image = xylineChart.createBufferedImage(width, height)
    val byteArray = new ByteArrayOutputStream()
    ChartUtilities.writeBufferedImageAsPNG(byteArray, image)
    byteArray.toByteArray()
  }

  def generateCombinedChartDemo = {
    val primaryDataset = new DefaultCategoryDataset()
    primaryDataset.addValue( 1.0 , "Ventas", "Enero" )
    primaryDataset.addValue( 2.0 , "Ventas",  "Febrero")
    primaryDataset.addValue( 3.0 , "Ventas",  "Marzo")

    val primaryRangeAxis = new NumberAxis("Venta items")
    primaryRangeAxis.setStandardTickUnits(NumberAxis.createIntegerTickUnits())
    val primaryRenderer = new LineAndShapeRenderer()
    primaryRenderer.setBaseToolTipGenerator(new StandardCategoryToolTipGenerator())
    primaryRenderer.setBaseItemLabelGenerator(new StandardCategoryItemLabelGenerator())
    primaryRenderer.setBaseItemLabelsVisible(true)
    val primarySubPlot = new CategoryPlot(primaryDataset, null, primaryRangeAxis, primaryRenderer)
    primarySubPlot.setDomainGridlinesVisible(true)

    val secondaryDataset = new DefaultCategoryDataset()
    secondaryDataset.addValue( 1.0 , "Ordenes", "Enero" )
    secondaryDataset.addValue( 2.0 , "Ordenes",  "Febrero")
    secondaryDataset.addValue( 3.0 , "Ordenes",  "Marzo")

    val secondaryRangeAxis = new NumberAxis("No. Ordenes")
    secondaryRangeAxis.setStandardTickUnits(NumberAxis.createIntegerTickUnits())
    val secondaryRenderer = new BarRenderer()
    secondaryRenderer.setBaseToolTipGenerator(new StandardCategoryToolTipGenerator())
    secondaryRenderer.setBaseItemLabelGenerator(new StandardCategoryItemLabelGenerator())
    secondaryRenderer.setBaseItemLabelsVisible(true)
    secondaryRenderer.setBarPainter(new StandardBarPainter)
    secondaryRenderer.setSeriesPaint(0, new Color(85, 177, 69))
    secondaryRenderer.setShadowPaint(new Color(.3f, .5f, .2f, 0.4f))
    val secondarySubPlot = new CategoryPlot(secondaryDataset, null, secondaryRangeAxis, secondaryRenderer)
    secondarySubPlot.setDomainGridlinesVisible(true)

    val domainAxis = new CategoryAxis("Meses")
    val plot = new CombinedDomainCategoryPlot(domainAxis)

    plot.add(primarySubPlot)
    plot.add(secondarySubPlot)

    val combinedChart = new JFreeChart(
      "Combined Domain Category Plot Demo",
      new Font("SansSerif", Font.BOLD, 12),
      plot,
      true
    )

    combinedChart.setBackgroundPaint(new Color(0, 0, 0, 0))

    val legend = combinedChart.getLegend
    legend.setPosition(RectangleEdge.RIGHT)
    legend.setFrame(BlockBorder.NONE)
    legend.setBackgroundPaint(new Color(0, 0, 0, 0))

    val width = 640; /* Width of the image */
    val height = 480; /* Height of the image */

    val image = combinedChart.createBufferedImage(width, height)
    val byteArray = new ByteArrayOutputStream()
    ChartUtilities.writeBufferedImageAsPNG(byteArray, image)
    byteArray.toByteArray()
  }

  def generateCombinedChart(primaryData: ListMap[String, Int], primaryCategoryName: String,
                            secondaryData: ListMap[String, Int], secondaryCategoryName: String,
                            titleX: String, primaryTitleY: String, secondaryTitleY: String, chartTitle: String) = {

    var width = 0 /* Width of the image */
    val height = 660 /* Height of the image */

    var primaryRangeValues = new ArrayBuffer[Int]()
    val primaryDataset = new DefaultCategoryDataset()
    primaryData.foreach { case (k, v) =>
      primaryDataset.addValue(v, primaryCategoryName, k)
        primaryRangeValues.insert(primaryRangeValues.length, v)
        width += 100
    }

    primaryRangeValues = primaryRangeValues.sortWith(_ < _)

    val primaryRangeAxis = new NumberAxis(primaryTitleY)
    primaryRangeAxis.setStandardTickUnits(NumberAxis.createIntegerTickUnits())
    var addRangeVal = 0
    if (primaryRangeValues.size > 0) {
      addRangeVal = math.abs(primaryRangeValues(primaryRangeValues.size - 1) - primaryRangeValues(primaryRangeValues.size - 2))
      if (addRangeVal < 1000) addRangeVal = 1000
      primaryRangeAxis.setRange(0.0D, primaryRangeValues.max + addRangeVal)
    }
    val primaryRenderer = new LineAndShapeRenderer()
    primaryRenderer.setBaseToolTipGenerator(new StandardCategoryToolTipGenerator())
    primaryRenderer.setBaseItemLabelGenerator(new StandardCategoryItemLabelGenerator())
    primaryRenderer.setBaseItemLabelsVisible(true)
//    primaryRenderer.setBasePositiveItemLabelPosition(new ItemLabelPosition(ItemLabelAnchor.INSIDE1, TextAnchor.TOP_CENTER, TextAnchor.BASELINE_RIGHT, 100))
    val primarySubPlot = new CategoryPlot(primaryDataset, null, primaryRangeAxis, primaryRenderer)
    primarySubPlot.setDomainGridlinesVisible(true)

    val secondaryDataset = new DefaultCategoryDataset()
    secondaryData.foreach { case (k, v) =>
      secondaryDataset.addValue(v, secondaryCategoryName, k)
    }

    val secondaryRangeAxis = new NumberAxis(secondaryTitleY)
    secondaryRangeAxis.setStandardTickUnits(NumberAxis.createIntegerTickUnits())
    val secondaryRenderer = new BarRenderer()
    secondaryRenderer.setBaseToolTipGenerator(new StandardCategoryToolTipGenerator())
    secondaryRenderer.setBaseItemLabelGenerator(new StandardCategoryItemLabelGenerator())
    secondaryRenderer.setBaseItemLabelsVisible(true)
    secondaryRenderer.setBasePositiveItemLabelPosition(new ItemLabelPosition(ItemLabelAnchor.CENTER,TextAnchor.CENTER))
    secondaryRenderer.setBarPainter(new StandardBarPainter)
    secondaryRenderer.setSeriesPaint(0, new Color(85, 177, 69))
    secondaryRenderer.setShadowPaint(new Color(.3f, .5f, .2f, 0.4f))
    val secondarySubPlot = new CategoryPlot(secondaryDataset, null, secondaryRangeAxis, secondaryRenderer)
    secondarySubPlot.setDomainGridlinesVisible(false)
    secondarySubPlot.setRangeGridlinesVisible(false)

    val domainAxis = new CategoryAxis(titleX)
    val plot = new CombinedDomainCategoryPlot(domainAxis)

    plot.add(primarySubPlot)
    plot.add(secondarySubPlot)

    val combinedChart = new JFreeChart(
      chartTitle,
      new Font("SansSerif", Font.BOLD, 30),
      plot,
      true
    )

    combinedChart.setBackgroundPaint(new Color(0, 0, 0, 0))

    val legend = combinedChart.getLegend
    legend.setPosition(RectangleEdge.RIGHT)
    legend.setFrame(BlockBorder.NONE)
    legend.setBackgroundPaint(new Color(0, 0, 0, 0))

    if (primaryData.size <= 12) width = 1200
    val image = combinedChart.createBufferedImage(width, height)
    val byteArray = new ByteArrayOutputStream()
    ChartUtilities.writeBufferedImageAsPNG(byteArray, image)
    println(BaseEncoding.base64().encode(byteArray.toByteArray))
//    byteArray.toByteArray()
    BaseEncoding.base64().encode(byteArray.toByteArray)
  }

}
