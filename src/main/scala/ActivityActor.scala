package com.kthcorp.h3.scala.actor

import akka.actor._
import akka.pattern._
import akka.util.Timeout
import akka.util.duration._

object Activities {
  trait Activity
  trait Noun

  case class write(sub: Noun, obj: Noun) extends Activity
  case class like(sub: Noun, obj: Noun) extends Activity

  //정상적인 follwing 처리를 위해 follow 모델 작성
  case class follow(sub: Noun, obj: Noun) extends Activity

  case class Sub(n: Noun) {
    def like(obj: Noun) = Activities.like(n, obj)
    def write(obj: Noun) = Activities.write(n, obj)
    def follow(obj: Noun) = Activities.follow(n, obj)
  }
  
  implicit def nounToSub(n: Noun) = Sub(n)

  case class Person(name: String, age: Int) extends Noun

  case class Post(author: Person, text: String) extends Noun

  /** dao 로 부터 follwer 목록을 얻기 위한 메시지 객체 */
  case class FollwersOf(who: Person)

  /** push, email 등을 날리기 위한 메시지 객체 */
  case class Message(to: Person, msg: String)
}


import Activities._

/**
 * 좀 더 그럴싸한 코드를 위해 DAO 개념의 Actor 추가
 * 간단한 구현을 위해 following 정보는 Map 에 저장된다
 */
class DAOActor extends Actor with ActorLogging{

  //격리된 var 변수로 이 액터의 인스턴스외에은 접근이 불가능하므로 안전
  var followers = Map[Person, List[Person]]()

  def receive = {
    case (who: Person) follow (whom: Person) => {
      log.info("%s follows %s".format(who, whom))
      //기존 리스트가 존재하면 새로운 follower 를 prepend 하고 map 을 변경
      //간단한 코드를 위해 매번 새로운 map 이 생성되는 방식, mutable.Map 을 사용해도 무방
      followers += whom -> (who :: followers.get(whom).getOrElse(Nil))
    }
    case FollwersOf(person) => sender ! followers(person)
  }
}

class PushActor extends Actor with ActorLogging {
  def receive = {
    case Message(to, msg) => log.info("Send Push [To: %s][Msg: %s]".format(to, msg))
  }
}

class EmailActor extends Actor with ActorLogging {
  def receive = {
    case Message(to, msg) => log.info("Send Email [To: %s][Msg: %s]".format(to, msg))
  }
}

/**
 * follow 개념이 들어가면서 notifcation actor 대신 activity 로 이름 변경
 */
class ActivityActor extends Actor with ActorLogging{
  implicit val timeout = Timeout(5 seconds)

  val daoActor = context.actorOf(Props[DAOActor], name = "dao")
  val pushActor = context.actorOf(Props[PushActor], name = "push")
  val emailActor = context.actorOf(Props[EmailActor], name = "email")

  def receive = {
    //@ 표현을 이용해 뒤 패턴전체를 alias 할 수 있음
    case f@(_ follow _) =>
      daoActor ! f
    
    case Person(name, _) like Post(author, _) =>
      sendPush(author, "%s likes your post".format(name))
    
    case (author:Person) write _ =>
      //follwer 목록이 담기게 될 future
      val future = daoActor ? FollwersOf(author)

      //functional future with for comprehension
      for(
        //future에 결과가 오면 그 내용을 List[Person]으로 캐스팅하고
        //그 결과를 follwers 변수에 담자
        followers <- future.mapTo[List[Person]];
        follower <- followers // 리스트 아이템 하나씩을 follwer 로 
      ) {
        sendEmail(follower, "Your friend(%s) write new post".format(author.name))
      }
  }

  def sendPush(to: Person, msg: String) {
    pushActor ! Message(to, msg)
  }

  def sendEmail(to: Person, msg: String) {
    emailActor ! Message(to, msg)
  }
}


object Main {

  implicit val system = ActorSystem("h3")
  val activityActor = system.actorOf(Props[ActivityActor], name = "activity")

  def main(args: Array[String]) {
    val jane = Person("jane", 59)
    val charles = Person("charles", 50)
    val gne = Person("G.Ne", 60)

    activityActor ! (jane follow charles)
    activityActor ! (charles follow jane)
    activityActor ! (gne follow charles)
    activityActor ! (gne follow jane)

    val post = Post(jane, "hello, world")

    activityActor ! (jane write post)
    activityActor ! (charles like post)

    //functional future
    gracefulStop(activityActor, 5 seconds) map {
      isSuccess => system.shutdown
    }
  }
}

