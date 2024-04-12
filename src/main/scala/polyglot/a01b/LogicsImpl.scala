package polyglot.a01b

import polyglot.OptionToOptional
import util.Optionals.Optional as ScalaOptional
import polyglot.a01b.Logics

import scala.jdk.javaapi.OptionConverters

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a01b/sol2/ */
trait Position:
  def x: Int
  def y: Int
object Position:
  def apply(x: Int, y: Int): Position = PositionImpl(x: Int, y: Int)
  private case class PositionImpl(x: Int, y: Int) extends Position

class LogicsImpl(private val size: Int, private val mines: Int) extends Logics:

  private val _gameSize: Int = size
  private val _nMines: Int = mines
  import util.Sequences.*
  private var _mines: Sequence[Position] = Sequence()
  private var _cells: Sequence[Position] = Sequence()

  for i <- 0 until _gameSize do
    for j <- 0 until _gameSize do
      _cells = _cells.add(Position(i,j))

  for i <- 0 until _nMines do
    _mines = _mines.add(Position(i,i)) //bombe sulla diagonale momentaneamente

  def hit(x: Int, y: Int): java.util.Optional[Integer] = _mines match
    case _mines if _mines.contains(Position(x, y)) => OptionToOptional(ScalaOptional.Empty())
    case _ =>
      _cells = _cells.filter(_!=Position(x,y))
      OptionToOptional(ScalaOptional.Just(_cells.filter(pos => pos.x >= 0 && pos.y >= 0 && pos.x < _gameSize && pos.y < _gameSize)
      .filter(pos => pos.x >= x-1 && pos.y >= y-1 && pos.x <= x+1 && pos.y <= y+1)
      .filter(pos => _mines.contains(pos)).size(0)))

  def won = _cells.size(0) == _mines.size(0)
