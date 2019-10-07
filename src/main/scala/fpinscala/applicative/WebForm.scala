package fpinscala.applicative

import java.text.SimpleDateFormat
import java.util.Date

import scala.util.Try
import scala.util.{Success => ScalaSuccess, Failure => ScalaFailure }

sealed trait Validation[+E, +A]
case class Success[A](a: A) extends Validation[Nothing, A]
case class Failure[E](head: E, tail: Vector[E] = Vector.empty) extends Validation[E, Nothing]
case class WebForm(name: String, birthdate: Date, phoneNumber: String)

object WebForm {
  type Validate[A] = Validation[String, A]

  def validName(name: String): Validation[String, String] =
    if (name != "") Success(name)
    else Failure("Name cannot be empty")

  def validBirthdate(birthdate: String): Validation[String, Date] =
    Try(new SimpleDateFormat("yyyy-MM-dd").parse(birthdate)) match {
      case ScalaSuccess(value) => Success(value)
      case ScalaFailure(_) => Failure("Birthdate must be in the form yyyy-MM-dd")
    }
  def validPhone(phoneNumber: String): Validation[String, String] =
    if (phoneNumber.matches("[0-9]{10}"))
      Success(phoneNumber)
    else Failure("Phone number must be 10 digits")

  def validWebForm(name: String, birthdate: String, phone: String)
                  (implicit A: Applicative[Validate]): Validate[WebForm] = {
    A.map3(
      validName(name),
      validBirthdate(birthdate),
      validPhone(phone))(WebForm.apply)
  }
}
