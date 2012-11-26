package com.kthcorp.h3.scala

object Activities {
  /**
   * 자바의 인터페이스와 유사한 trait 을 이용해 수퍼타입 선언
   *  내용이 없는 경우 중괄호 생략 가능
   */
  trait Activity

  /**
   * 모든 명사 타입 객체, 즉 리소스나 모델들을 위한 최상위 클래스
   * 주어와 목적어 자리에 들어갈 수 있다
   */
  trait Noun

  /**
   * write activity 로 소문자로 쓴것은, 
   * 밑에 있는 Sub.write 함수와 모양을 일치 시켜 더 멋진 코드를 만들기 위함 :)
   */
  case class write(sub: Noun, obj: Noun) extends Activity

  //역시, Sub.like 함수의 이름을 일치 시킨다
  case class like(sub: Noun, obj: Noun) extends Activity

  /**
   * 명사를 wrapping 해서 주어 역할을 할 수 있도록 한다
   */
  case class Sub(n: Noun) {
    def like(obj: Noun) = Activities.like(n, obj) //재귀가 아니라 생성자를 부르도록 풀네임 명시
    def write(obj: Noun) = Activities.write(n, obj)
  }

  /**
   * 명사를 언제든지 암묵적으로 주어로 바꿔준다
   * Noun 객체 뒤에 like 나 write 등의 함수를 부르면 자동으로 이 함수가 불려 타입 변환
   */
  implicit def nounToSub(n: Noun) = Sub(n)

  /** 한명의 사람을 나타내는 모델 클래스 */
  case class Person(name: String, age: Int) extends Noun {
    def followers = List(Person("wangtao", 35), Person("taokim", 35))
  }

  /** 
   * 현재는 나이뿐이라 비효율적이지만, mutable 상태를 가지는 여러 인자로 구성하면 StringBuilder
   * 와 같은 형태로 효율적으로 사용 가능
   */
  case class PersonBuilder(name : String, age: Int = 0) {
    def is(age: Int) = copy(age = age)

    def old = Person(name, age)
  }

  implicit def strToPersonBuilder(name: String) = new PersonBuilder(name)


  /** 글 하나를 나타내는 모델 */
  case class Post(author: Person, text: String) extends Noun
}


object Notification {
  import Activities._
  
  def sendNoti(act: Activity) {
    act match {
      //패턴자리에 일반적으로 함수를 쓸수는 없다. 
      //다음 경우는 생성자로 인셕되며 constructor pattern 이라 불린다.
      //like(Person, Post) 처럼 중앙에 있는 표현식이 
      //클래스의 생성자이며 좌,우 값이 생성자의 인자여야 한다
      case Person(name, _) like Post(author, _) =>
        sendPush(author, "%s likes your post".format(name))

      //write(someone, _)
      case (someone: Person) write _ =>
        someone.followers.foreach{
          p =>
            sendEmail(p, "Your friend(%s) write new post".format(someone.name))
        }
    }
  }

  def sendPush(to: Person, msg: String) {
    println("Send Push [To: %s][Msg: %s]".format(to, msg))
  }

  def sendEmail(to: Person, msg: String) {
    println("Send Email [To: %s][Msg: %s]".format(to, msg))
  }
}

object Main {

  import Activities._
  import Notification._

  def main(args: Array[String]) {
    val jane = "jane" is 59 old

    val charles = "charles" is 50 old

    val post = Post(jane, "hello, world")

    val a1 = jane write post //자동으로 jane이 Sub 에 둘러싸여 write 함수를 부름
    val a2 = charles like post //like 함수가 like(charles, post) 객체를 생성해 리턴

    println(a1)
    println(a2)

    sendNoti(a1)
    sendNoti(a2)
  }
}

