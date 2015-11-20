import model.{Car, Game, Move, World}
import model.TileType.{RIGHT_BOTTOM_CORNER, LEFT_BOTTOM_CORNER, RIGHT_TOP_CORNER, LEFT_TOP_CORNER}
import scala.math.{hypot, Pi, abs}
import java.io._


class MyStrategy extends Strategy {
    private val logWriter_ = new PrintWriter("car.log")

    private def log(world: World, msg: String) {
        logWriter_.write(s"[${world.tick}] $msg\n")
    }

    def move(self: Car, world: World, game: Game, move: Move) {
        var nextWaypointX: Double = (self.nextWaypointX + 0.5D) * game.trackTileSize
        var nextWaypointY: Double = (self.nextWaypointY + 0.5D) * game.trackTileSize

        val distToWaypoint = self.distanceTo(nextWaypointX, nextWaypointY)
        log(world, s"x=${self.x} y=${self.y} x_wp=${self.nextWaypointX} y_wp=${self.nextWaypointY}"
            ++ s" x_wp_next=$nextWaypointX y_wp_next=$nextWaypointY dist_to_wp=$distToWaypoint")

        val cornerTileOffset: Double = 0.25D * game.trackTileSize
        move.enginePower = 0.75
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
            move.enginePower = 1
            if (distToWaypoint > 2*game.trackTileSize) {
                move.useNitro = true
            }
            
        }

        val angleToWaypoint: Double = self.angleTo(nextWaypointX, nextWaypointY)
        move.wheelTurn = angleToWaypoint * 32.0D / Pi

        val speedModule: Double = hypot(self.speedX, self.speedY)
        if (speedModule * speedModule * abs(angleToWaypoint) > 2.5D * 2.5D * Pi) {
            move.brake = true
        }


    }
}
