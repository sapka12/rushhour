package hu.arnoldfarkas.rushhour

object Game {
  case class Pos(x: Int, y: Int)

  type Field = Pos => Boolean

  case class Car(val positions: Set[Pos], val sign: Char) {

    def validMoves: Set[Move] = positions match {
      case positions if positions.map(_.x).forall(_ == positions.head.x) => {
        Set(
          Move(Car(positions.map(pos => Pos(pos.x, pos.y - 1)), sign), Up),
          Move(Car(positions.map(pos => Pos(pos.x, pos.y + 1)), sign), Down)
        )
      }
      case positions if positions.map(_.y).forall(_ == positions.head.y) => {
        Set(
          Move(Car(positions.map(pos => Pos(pos.x - 1, pos.y)), sign),
            Left),
          Move(Car(positions.map(pos => Pos(pos.x + 1, pos.y)), sign),
            Right)
        )
      }
    }

    def validSteps: Set[Step] = {
      val pos = this.positions.toList(0)
      if (this.positions.forall(_.x == pos.x)) {
        Set(Up, Down)
      } else {
        Set(Left, Right)
      }
    }

    def reposition(fp: Pos => Pos): Car = Car(positions.map(fp), sign)

    def goBy(step: Step): Option[Car] =
      if (validSteps.contains(step)) {
        Some(step match {
          case Up => reposition(p => Pos(p.x, p.y - 1))
          case Down => reposition(p => Pos(p.x, p.y + 1))
          case Left => reposition(p => Pos(p.x - 1, p.y))
          case Right => reposition(p => Pos(p.x + 1, p.y))
        })
      } else {
        None
      }
  }

  case class State(val field: Field, val cars: Set[Car]) {

    def validMoves: Set[(Move, State)] = {
      val allValidSteps = for {
        car <- cars
        move <- car.validMoves
      } yield move

      allValidSteps.filter(_.car.positions.forall(field))
        .map(move => (move, goBy(move)))
        .filterNot(_._2 == None)
        .map(tuple => (tuple._1, tuple._2.get))
    }

    def goBy(move: Move): Option[State] = {
      val car = move.car
      val steps = car.validSteps
      if (steps.contains(move.step)) {
        car goBy move.step match {
          case Some(c) => {
            val newCars: Set[Car] =
              cars.filter(_.sign == car.sign) + c
            Some(State(field, newCars))
          }
          case None => None
        }
      } else {
        None
      }
    }
  }

  def finalState(state: State, car: Car): Boolean =
    state.cars.contains(car)

  sealed trait Step
  case object Up extends Step
  case object Down extends Step
  case object Left extends Step
  case object Right extends Step

  case class Move(val car: Car, val step: Step) {
//    override def toString: String =
//      "[" + car.sign.toString + {
//        step match {
//          case Up => "^"
//          case Down => "v"
//          case Left => "<"
//          case Right => ">"
//        }
//      } + "]"
  }
  type Path = List[Move]

}
