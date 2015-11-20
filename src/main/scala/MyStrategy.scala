import model.{Car, Game, Move, World}
import model.TileType.{RIGHT_BOTTOM_CORNER, LEFT_BOTTOM_CORNER, RIGHT_TOP_CORNER, LEFT_TOP_CORNER}
import scala.math.{hypot, Pi, abs}
import scala.language.implicitConversions
import java.io._

class Point(val xc: Double, val yc: Double) {
    var x: Double = xc
    var y: Double = yc
}

class MyStrategy extends Strategy {
    private val logWriter_ = new PrintWriter("car.log")

    implicit def bool2int(b:Boolean) = if (b) 1 else 0

    private def log(world: World, msg: String) {
        logWriter_.write("[%4d] %s\n".format(world.tick, msg))
    }

    private def getNextWaypoint(self: Car, world: World, game: Game): Point = {
        var nextWaypointX: Double = (self.nextWaypointX + 0.5D) * game.trackTileSize
        var nextWaypointY: Double = (self.nextWaypointY + 0.5D) * game.trackTileSize
        val cornerTileOffset: Double = 0.25D * game.trackTileSize

        world.tilesXY(self.nextWaypointX)(self.nextWaypointY) match {
        case LEFT_TOP_CORNER =>
            nextWaypointX += cornerTileOffset
            nextWaypointY += cornerTileOffset
        case RIGHT_TOP_CORNER =>
            nextWaypointX -= cornerTileOffset
            nextWaypointY += cornerTileOffset
        case LEFT_BOTTOM_CORNER =>
            nextWaypointX += cornerTileOffset
            nextWaypointY -= cornerTileOffset
        case RIGHT_BOTTOM_CORNER =>
            nextWaypointX -= cornerTileOffset
            nextWaypointY -= cornerTileOffset
        case _ =>
        }

        return new Point(nextWaypointX, nextWaypointY)
    }

    private def getNextPoint(self: Car, world: World, game: Game): Point = {
        return getNextWaypoint(self, world, game)
    }

    private def canSpeedUp(self: Car, world: World, game: Game, distToPoint: Double): Boolean = {
        val tileType = world.tilesXY(self.nextWaypointX)(self.nextWaypointY) 
        return tileType != LEFT_TOP_CORNER && tileType != RIGHT_TOP_CORNER && 
            tileType != LEFT_BOTTOM_CORNER && tileType != RIGHT_BOTTOM_CORNER &&
            distToPoint > 3*game.trackTileSize
    }

    def move(self: Car, world: World, game: Game, move: Move) {

        val nextPoint = getNextPoint(self, world, game)
        val distToPoint = self.distanceTo(nextPoint.x, nextPoint.y)
        val angleToPoint: Double = self.angleTo(nextPoint.x, nextPoint.y)

        move.enginePower = 0.75
        if (canSpeedUp(self, world, game, distToPoint)) {
            move.enginePower = 1
            move.useNitro = true
        }

        move.wheelTurn = angleToPoint * 32.0D / Pi
        val speedModule: Double = hypot(self.speedX, self.speedY)
        if (speedModule * speedModule * abs(angleToPoint) > 2.5D * 2.5D * Pi) {
            move.brake = true
        }

        log(world, "x=%07.2f y=%07.2f x_wp=%d y_wp=%d x_next=%07.2f y_next=%07.2f dist_next=%07.2f angle_next=%+.4f turn=%+08.4f power=%.2f brake=%d"
            .format(self.x, self.y, self.nextWaypointX, self.nextWaypointY, nextPoint.x, nextPoint.y,
                distToPoint, angleToPoint, move.wheelTurn, move.enginePower, move.brake:Int))
    }
}
