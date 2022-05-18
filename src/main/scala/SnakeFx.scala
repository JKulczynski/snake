import scalafx.application.{JFXApp3, Platform}
import scalafx.beans.property.{IntegerProperty, ObjectProperty}
import scalafx.scene.Scene
import scalafx.scene.paint.Color
import scalafx.scene.paint.Color._
import scalafx.scene.shape.Rectangle

import scala.concurrent.Future
import scala.util.Random

object SnakeFx extends JFXApp3 {

  //Drawing first 3-square snake in game
  val initialSnake: List[(Double, Double)] = List(
    (250, 200),
    (225, 200),
    (200, 200),
  )

  import scala.concurrent.ExecutionContext.Implicits.global
  def gameLoop(update: () => Unit): Unit =
    Future {
      update()
      Thread.sleep(80)
    }.flatMap(_ => Future(gameLoop(update)))

  //State of game
  //Seting directions for snake
  case class State(snake: List[(Double, Double)], food: (Double, Double)) {
    //Updating State of snake
    def newState(dir: Int): State = {
      val (x, y) = snake.head
      val (newx, newy) = dir match {
        case 1 => (x, y - 25) //up
        case 2 => (x, y + 25) //down
        case 3 => (x - 25, y) //left
        case 4 => (x + 25, y) //right
        case _ => (x, y)
      }

      //Creating new Snake:
      //1.After crashing into the scene boundaries
      val newSnake: List[(Double, Double)] =
      if (newx < 0 || newx >= 600 || newy < 0 || newy >= 600 || snake.tail.contains((newx, newy)))
        initialSnake
      //After eating Food
      else if (food == (newx, newy))
        food :: snake
      else
        (newx, newy) :: snake.init

      //Creating new food on scene
      val newFood =
        if (food == (newx, newy))
          randomFood()
        else
          food

      //updating scene with new snake and new food
      State(newSnake, newFood)
    }

    //Display new state on the screen
    def rectangles: List[Rectangle] = square(food._1, food._2, Red) :: snake.map {
      case (x, y) => square(x, y, Green)
    }
  }

  //Placing food on scene
  def randomFood(): (Double, Double) =
    (Random.nextInt(24) * 25, Random.nextInt(24) * 25)

  //Drawing new square of snake
  def square(xr: Double, yr: Double, color: Color) = new Rectangle {
    x = xr
    y = yr
    width = 25
    height = 25
    fill = color
  }

  //Drawing stage of game
  override def start(): Unit = {

    //Property describing the current state of the game as an instance of State
    val state = ObjectProperty(State(initialSnake, randomFood()))
    //Property that keeps the current frame, updating every X milliseconds
    val frame = IntegerProperty(0)
    //Property that keeps track of the current direction, changeable by key presses
    val direction = IntegerProperty(4)

    //Updating the frame, automatically once the frame changes.
    frame.onChange {
      state.update(state.value.newState(direction.value))
    }

    stage = new JFXApp3.PrimaryStage {
      width = 600
      height = 600
      scene = new Scene {
        fill = White
        content = state.value.rectangles
        //update direction based on key presses
        onKeyPressed = key => key.getText match {
          case "w" => direction.value = 1
          case "s" => direction.value = 2
          case "a" => direction.value = 3
          case "d" => direction.value = 4
        }
        state.onChange(Platform.runLater{
          content = state.value.rectangles})
        }
      }

    gameLoop(() => frame.update(frame.value + 1))
    }

  }

