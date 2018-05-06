package example

import example.domain.{User, UserId, UserName, UserType}
import example.dump.MapDump

object Main extends App {
  val u1 = User(UserId(1L), UserName("sato"), UserType.Normal)

//  if derivation failed, then output
//  Main.scala:11:22: magnolia: could not find MapDump.Typeclass for type Long
//       in parameter 'value' of product type example.domain.UserId
//       in parameter 'id' of product type example.domain.User
  println(MapDump.gen[User].dumpAsMap(u1))

//  same as above. but no debug output
//  (User,Map(id -> (UserId,UserId(1)), name -> (UserName,Map(value -> (string,sato))), tpe -> (Normal,Normal)))
  println(implicitly[MapDump[User]].dumpAsMap(u1))
}
