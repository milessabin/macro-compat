/*
 * Copyright (c) 2015 Miles Sabin
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package macrocompat

class ContextReporter[
  C <: tools.nsc.typechecker.Contexts#Context
](val c: C) {
  def hasErrors = c.hasErrors
  def isBuffering = c.bufferErrors
  def isThrowing = c.throwErrors
  def errors = c.errBuffer.toVector
  def warnings = c.warningsBuffer.toVector
  def firstError = errors.headOption
  def clearAll(): Unit = {
    c.errBuffer.clear
    c.warningsBuffer.clear
  }
  def clearAllErrors(): Unit = c.errBuffer.clear
}
class TypecheckerContextExtensions[
  C <: tools.nsc.typechecker.Contexts#Context
](val c: C) {
  def reporter = new ContextReporter[C](c)
}

object TypecheckerContextExtensions {
  implicit def contextExtensions[C <: tools.nsc.typechecker.Contexts#Context](c: C) =
    new TypecheckerContextExtensions[C](c)
}
