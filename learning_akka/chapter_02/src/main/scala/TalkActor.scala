package zinh
import scala.language.postfixOps
import akka.pattern.ask
import akka.actor.{ ActorRef, ActorSystem, Props, Actor }
import Recorder._
import Checker._
import Storage._

case class User(username: String, email: String)

object Recorder{
  sealed trait RecorderMsg
  case class NewUser(user: User) extends RecorderMsg

  def props(checker: ActorRef, storage: ActorRef) = 
    Props(new Recorder(checker, storage))
}

object Checker{
  sealed trait CheckerMsg
  case class CheckUser(user: User) extends CheckerMsg

  sealed trait CheckerResponse
  case class BlackUser(user: User) extends CheckerResponse
  case class WhiteUser(user: User) extends CheckerResponse
}

object Storage{
  sealed trait StorageMsg
  case class addUser(user: User) extends StorageMsg
}

class Storage extends Actor{
  var users = List.empty[User]
  def receive = {
    case addUser(user) => {
      println(s"Storage: $user added")
      users = user :: users
    }
  }
}

class Checker extends Actor{
  val blacklist = List(User("Victor", "victor@gmail.com"))
  def receive = {
    case CheckUser(user) if blacklist.contains(user) =>
      println(s"Checker: $user blacklisted")
      sender() ! BlackUser(user)
    case CheckUser(user) =>
      println(s"Checker: $user whitelist")
      sender() ! WhiteUser(user)
  }
}

class Recorder(checker: ActorRef, storage: ActorRef) extends App{
  def receive = {
    case NewUser(user) =>
      checker ? CheckUser(user) map {
        case WhiteUser(user) =>
          storage !  addUser(user)
        case BlackUser(user) =>
          println(s"Recorder: $user is blacklisted")
      }
  }
}

object ActorTalk extends App{
  val system = ActorSystem("talk-to-actor")
  val checker = system.actorOf(Props[Checker], "checker")
  val storage = system.actorOf(Props[Storage], "storage")
  val recorder = system.actorOf(Recorder.props(checker, storage), "recorder")
  recorder ! Recorder.NewUser(User("John", "john@live.com"))
  Thread.sleep(100)
  system.terminate
}
