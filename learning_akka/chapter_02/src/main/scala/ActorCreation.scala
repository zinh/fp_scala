import akka.actor.{ ActorRef, ActorSystem, Props, Actor }
import MusicController._
import MusicPlayer._
object MusicController{
  sealed trait ControllerMsg
  case object Play extends ControllerMsg
  case object Stop extends ControllerMsg
  def props = Props[MusicController]
}

class MusicController extends Actor{
  def receive = {
    case Play => println("Music played...")
    case Stop => println("Music stopped")
    case _ => println("Unknown message")
  }
}

object MusicPlayer{
  sealed trait PlayMsg
  case object StopMusic extends PlayMsg
  case object PlayMusic extends PlayMsg
}

class MusicPlayer extends Actor{
  def receive = {
    case StopMusic => {
      println("Don't wanna stop")
    }
    case PlayMusic => {
      val controller = context.actorOf(MusicController.props, "controller")
      controller ! Play
    }
    case _ => println("Unknown message")
  }
}

object Main extends App{
  val system = ActorSystem("creation")
  val player = system.actorOf(Props[MusicPlayer], "player")
  player ! PlayMusic
  player ! StopMusic
  system.terminate
}
