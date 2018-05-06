package example.dump

import magnolia._

import scala.language.experimental.macros
import scala.util.control.NonFatal

trait FromDumpedMap[A] {
  def constructFrom(value: (String, Any)): A
}

object FromDumpedMap extends GenericFromDumpedMap {
  def apply[A](f: (String, Any) => A): FromDumpedMap[A] =
    new FromDumpedMap[A] {
      override def constructFrom(value: (String, Any)): A = f(value._1, value._2)
    }

  implicit val stringDump: FromDumpedMap[String] = FromDumpedMap[String] {
    case (clazz, param: String) if clazz == "string" =>
      param
    case arg =>
      throw new IllegalArgumentException(s"failed to decode. $arg")
  }

  implicit val longDump: FromDumpedMap[Long] = FromDumpedMap[Long] {
    case (clazz, param: Long) if clazz == "long" =>
      param
    case arg =>
      throw new IllegalArgumentException(s"failed to decode. $arg")
  }
}

trait GenericFromDumpedMap {
  type Typeclass[T] = FromDumpedMap[T]

  def combine[T](ctx: CaseClass[Typeclass, T]): Typeclass[T] = new Typeclass[T] {
    override def constructFrom(value: (String, Any)): T = {
      if (ctx.isValueClass || ctx.isObject) {
        try {
          value._2.asInstanceOf[T]
        } catch {
          case NonFatal(e) => throw new IllegalArgumentException(s"failed to decode. $ctx $value", e)
        }
      } else {
        ctx.construct { p =>
          val paramMap = try {
            value._2.asInstanceOf[Map[String, (String, Any)]]
          } catch {
            case NonFatal(e) => throw new IllegalArgumentException(s"failed to decode. $ctx $value", e)
          }

          val param = if (paramMap.contains(p.label)) {
            paramMap(p.label)
          } else {
            throw new IllegalArgumentException(s"failed to decode. $ctx $p $value")
          }
          p.typeclass.constructFrom(param)
        }
      }
    }
  }

  def dispatch[T](ctx: SealedTrait[Typeclass, T]): Typeclass[T] = new Typeclass[T] {
    override def constructFrom(value: (String, Any)): T = {
      val (subtype, subTypeValue) = ctx.subtypes.find(_.typeName.short == value._1) match {
        case Some(sub) =>
          try {
            (sub, value._2.asInstanceOf[sub.SType])
          } catch {
            case NonFatal(e) => throw new IllegalArgumentException(s"failed to decode. $ctx $value", e)
          }
        case _ => throw new IllegalArgumentException(s"failed to decode. $ctx $value")
      }
      subtype.typeclass.constructFrom((value._1, subTypeValue))
    }
  }

  implicit def gen[T]: Typeclass[T] = macro Magnolia.gen[T]
}
