package example.dump

import magnolia._

import scala.language.experimental.macros

trait MapDump[A] {
  /** return
    * (className, Map(param1 -> value1, param2 -> value2))
    * or
    * (className, value1)
    * when value class or object
    */
  def dumpAsMap(value: A): (String, Any)
}

object MapDump extends GenericMapDump {
  def apply[A](f: A => (String, Any)): MapDump[A] = new MapDump[A] {
    override def dumpAsMap(value: A): (String, Any) = f(value)
  }

  implicit val stringDump: MapDump[String] = MapDump[String] { value =>
    ("string", value)
  }

  implicit val longDump: MapDump[Long] = MapDump[Long] { value =>
    ("long", value)
  }
}

trait GenericMapDump {
  type Typeclass[T] = MapDump[T]

  def combine[T](ctx: CaseClass[Typeclass, T]): Typeclass[T] = new Typeclass[T] {
    override def dumpAsMap(value: T): (String, Any) = {
      val valueOrMap = if (ctx.isValueClass || ctx.isObject) {
        value
      } else {
        ctx.parameters.map { p =>
          p.label -> p.typeclass.dumpAsMap(p.dereference(value))
        }.toMap
      }
      (ctx.typeName.short, valueOrMap)
    }
  }

  def dispatch[T](ctx: SealedTrait[Typeclass, T]): Typeclass[T] = new Typeclass[T] {
    override def dumpAsMap(value: T): (String, Any) = ctx.dispatch(value) { sub =>
      sub.typeclass.dumpAsMap(sub.cast(value))
    }
  }

  implicit def gen[T]: Typeclass[T] = macro Magnolia.gen[T]
}
