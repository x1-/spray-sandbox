package com.inkenkun.x1

import akka.actor.Actor
import spray.routing._
import spray.http._
import MediaTypes._

import scala.xml.Elem

// we don't implement our route structure directly in the service actor because
// we want to be able to test it independently, without having to spin up an actor
class MyServiceActor extends Actor with MyService {

  // the HttpService trait defines only one abstract member, which
  // connects the services environment to the enclosing actor or test
  def actorRefFactory = context

  // this actor only runs our route, but you could add
  // other things here, like request stream processing
  // or timeout handling
  def receive = runRoute( myRoute )
}


// this trait defines our service behavior independently from the service actor
trait MyService extends HttpService {

  val myRoute =
    path( "" ) {
      get {
        respondWithMediaType( `text/html` ) { // XML is marshalled to `text/xml` by default, so we simply override here
          complete ( index )
        }
      }
    } ~
    path( "add" ) {
      get {
        addPage
      } ~
      post {
        formFields( 'todo ) { todo =>
          Todos.add( todo )
          addPage
        }
      }
    }

  def addPage: Route = {
    respondWithMediaType( `text/html` ) {
      complete( add( Todos.all ) )
    }
  }

  lazy val index =
    <html>
      <body>
        <h1>TODO管理 |дﾟ)ﾁﾗｯ</h1>
        <ul>
          <li><a href="/add" title="TODO登録">TODO登録</a></li>
        </ul>
      </body>
    </html>

  def add( items: Seq[String] = Seq.empty[String] ): Elem =
    <html>
      <body>
        <h2>TODO登録ヾ(*´∀｀*)ﾉ</h2>
        <form name="form1" method="POST" action="add">
          <div>
            <span><input type="text" name="todo" value="" style="width:60%;" /></span>
            <span><input type="submit" name="submit" value="登録" /></span>
          </div>
        </form>
        <div>
          <ul>
            { for { item <- items } yield <li>{item}</li> }
          </ul>
        </div>
      </body>
    </html>

  def input( value: String ) = s"""<span><input type="text" name="todo" value="$value" style="width:60%;" /></span>"""
}

object Todos {
  private var items: Seq[String] = Seq()
  def add( item: String ): Unit = synchronized {
    items = items :+ item
  }
  def remove( item: String ): Unit = {
    val ( head, tail ) = items.span( _ != item )
    synchronized {
      if ( tail.size == 0 )
        head
      else
        head ++ tail.tail
    }
  }
  def all: Seq[String] = items
}