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
    Ok(views.html.index("Index"))
  }

  def showVentas = TODO
  def showInventario = TODO
}

