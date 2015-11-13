package companionobject

import scala.reflect.macros.whitebox


@macrocompat.bundle
class ClassWithoutCompanionObject(val c: whitebox.Context)


@macrocompat.bundle
class ClassWithCompanionObject(val c: whitebox.Context)

object ClassWithCompanionObject {
  def foo = 1
}
