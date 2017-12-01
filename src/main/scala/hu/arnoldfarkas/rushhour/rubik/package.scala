package hu.arnoldfarkas.rushhour

import hu.arnoldfarkas.rushhour.rubik._

package object rubik {
  type Color = Char

}

case class RubikPage(tiles: Seq[Seq[Color]]) {

  lazy val isReady = tiles.flatten.forall(_ == tiles.head.head)

  def rotate(rotation: Rotation): RubikPage = rotation match {
    case CW => ???
    case CCW => ???
  }
}
