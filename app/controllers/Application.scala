package controllers

import javax.inject.Inject

import play.api.Play.current
import play.api.mvc._
import play.api.db._
import scala.collection.mutable
/**
  * Created by yair on 30/03/16.
  */
class Application @Inject()(db: Database) extends Controller {
  // val dataSource = Databases(
     // driver = "org.postgresql.Driver",
     // url    = "jdbc:postgresql://192.168.1.94/ofbiz13prod",
     // name   = "ofbiz13prod",
     // config = Map(
       // "username" -> "ofbiz",
       // "password" -> "Duke.2244"
     // )
   // )

  def index = Action {
    println("INDEX  ")

    val prods      = new mutable.HashMap[String, String]
    db.withConnection{ conn =>
      val statement = conn.createStatement();
      val query = "select * from product limit 10"
      val resultSet = statement.executeQuery(query);
      while( resultSet.next() ) {
        val productDescription = resultSet.getString("description")
        val productId          = resultSet.getString("product_id")

        println(productId)

        prods.put(productId, productDescription)
      }

    }





    /*
    val connection = db.getConnection();
    val prods      = new mutable.HashMap[String, String]

    try {
      val statement = connection.createStatement();

      try {
        val query = "select * from product limit 10"
        val resultSet = statement.executeQuery(query);

        try {
          // Do stuff with the result set.
          while( resultSet.next() ) {
            val productDescription = resultSet.getString("description")
            val productId          = resultSet.getString("product_id")

            println(productId)

            prods.put(productId, productDescription)
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
    */


    /*
    val connection = database.getConnection()
    val statement  = connection.createStatement()
    val products   = statement.executeQuery("select * from product limit 10")
    val prods      = new mutable.HashMap[String, String]

    while( products.next() ) {
      val productDescription = products.getString("description")
      val productId          = products.getString("product_id")

      println(productId)

      prods.put(productId, productDescription)
    }

    println("PRODUCTOS: " + prods)

    products.close()
    statement.close()
    connection.close()
    */

    Ok(views.html.index(prods.toString()))
  }
}

