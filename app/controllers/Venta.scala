package controllers

import play.api.mvc._

/**
  * Created by yair on 11/04/16.
  */
 class Venta extends Controller {

   def showVentaFamilias = Action {
     Ok(views.html.ventas())

   }

}
