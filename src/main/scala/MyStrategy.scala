import model.{Car, Game, Move, World}
import model.TileType.{RIGHT_BOTTOM_CORNER, LEFT_BOTTOM_CORNER, RIGHT_TOP_CORNER, LEFT_TOP_CORNER}
import scala.math.{hypot, Pi, abs}


class MyStrategy extends Strategy {
    def move(self: Car, world: World, game: Game, move: Move) {
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
        val angleToWaypoint: Double = self.angleTo(nextWaypointX, nextWaypointY)
        val speedModule: Double = hypot(self.speedX, self.speedY)
        move.wheelTurn = angleToWaypoint * 32.0D / Pi
        move.enginePower = 0.75D
        if (speedModule * speedModule * abs(angleToWaypoint) > 2.5D * 2.5D * Pi) {
          move.brake = true
        }
    }
}
