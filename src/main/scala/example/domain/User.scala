package example.domain

case class UserId(value: Long) extends AnyVal
case class UserName(value: String)

sealed trait UserType
object UserType {
  case object Normal extends UserType
  case object Premium extends UserType
}

case class User(
  id: UserId,
  name: UserName,
  tpe: UserType
)
