/*
 * Copyright 2016-2017 47 Degrees, LLC. <http://www.47deg.com>
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

package com.fortysevendeg.scalacheck.combinators

import org.scalacheck.{Arbitrary, Gen}

object Config extends App {

  /*

  server = {
    name = "Server name"
    credentials = {
      username = "admin"
      password = "root"
    }
  }

   */

  sealed trait Prop

  case class PropMap(props: List[(String, Prop)]) extends Prop

  object PropMap {
    def apply(prop: (String, Prop)*): PropMap = PropMap(prop.toList)
  }

  trait PropValue extends Prop {
    type Type
    val gen: Gen[Type]
    val printer: Printer[Type]
  }

  object PropValue {
    def apply[T](implicit A: Arbitrary[T], P: Printer[T]): PropValue = new PropValue {
      override type Type = T
      override val gen: Gen[T]         = A.arbitrary
      override val printer: Printer[T] = P
    }
  }

  trait Printer[T] {
    def print(t: T): String
  }

  implicit val matPropMapPrinter: Printer[MatPropMap] = new Printer[MatPropMap] {
    override def print(t: MatPropMap): String =
      t.props
        .map {
          case (l, p: MatPropValue) => s"$l = ${p.print}"
          case (l, p: MatPropMap)   => s"$l = {\n${print(p)}\n}"
        }
        .mkString("\n")

  }

  implicit val stringPrinter: Printer[String] = new Printer[String] {
    override def print(s: String): String = s""""$s""""
  }

  sealed trait MatProp

  case class MatPropMap(props: List[(String, MatProp)]) extends MatProp

  object MatPropMap {
    def apply(prop: (String, MatProp)*): MatPropMap = MatPropMap(prop.toList)
  }

  trait MatPropValue extends MatProp {
    type T
    val t: T
    val p: Printer[T]
    def print: String = p.print(t)
  }

  object MatPropValue {
    def apply[Type](value: Type)(implicit P: Printer[Type]): MatPropValue = new MatPropValue {
      override type T = Type
      override val t: T          = value
      override val p: Printer[T] = P
    }
  }

  def toMap(prop: Prop): Gen[MatProp] = prop match {
    case p: PropValue => p.gen.map(MatPropValue(_)(p.printer))
    case PropMap(Nil) => Gen.const(MatPropMap())
    case PropMap(head :: tail) =>
      val base = toMap(head._2).map(p => MatPropMap(head._1 -> p))
      tail.foldLeft(base) {
        case (matPropMat, (l, p)) =>
          for {
            map1 <- matPropMat
            map2 <- toMap(p).map(matProp => MatPropMap(l -> matProp))
          } yield MatPropMap(map1.props ++ map2.props)
      }

  }

  val config = MatPropMap(
    "server" -> MatPropMap(
      "name" -> MatPropValue("Server Name"),
      "credentials" -> MatPropMap(
        "username" -> MatPropValue("admin"),
        "password" -> MatPropValue("root")
      )
    )
  )

  val configGen = PropMap(
    "server" -> PropMap(
      "name" -> PropValue[String](Arbitrary(Gen.identifier), stringPrinter),
      "credentials" -> PropMap(
        "username" -> PropValue[String](Arbitrary(Gen.identifier), stringPrinter),
        "password" -> PropValue[String](Arbitrary(Gen.identifier), stringPrinter)
      )
    )
  )

  val c = toMap(configGen)
  println(
    c.sample
      .map {
        case p: MatPropMap =>
          matPropMapPrinter.print(p)
        case _ => "PropValue"
      }
      .getOrElse("Empty"))

}
