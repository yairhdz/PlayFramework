package controllers

import javax.inject.Inject
import play.api.mvc._
import play.api.db._
import scala.collection.mutable

/**
  * Created by yair on 30/03/16.
  */

class Application @Inject()(db: Database) extends Controller {

  def index = Action {
    val level = "1"
    val list = Seq("uno", "dos", "tres")
    println("INDEX")

    val prods = new mutable.HashMap[String, String]

    db.withConnection{ conn =>
      val statement = conn.createStatement()
      val query     = "select * from product limit 10"
      val resultSet = statement.executeQuery(query)

      while( resultSet.next() ) {
        val productDescription = resultSet.getString("description")
        val productId          = resultSet.getString("product_id")

        println(productId)

        prods.put(productId, productDescription)
      }
    }
    Ok(views.html.index("HOLA"))
  }

}

