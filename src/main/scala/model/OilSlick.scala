package model

/**
  * Класс, определяющий лужу мазута. Содержит также все свойства круглого юнита.
  * @param id Возвращает уникальный идентификатор объекта.
  * @param mass Возвращает массу объекта в единицах массы.
  * @param x Возвращает X-координату центра объекта. Ось абсцисс направлена слева направо.
  * @param y Возвращает Y-координату центра объекта. Ось ординат направлена свеху вниз.
  * @param speedX Возвращает X-составляющую скорости объекта. Ось абсцисс направлена слева направо.
  * @param speedY Возвращает Y-составляющую скорости объекта. Ось ординат направлена свеху вниз.
  * @param angle Возвращает угол поворота объекта в радианах. Нулевой угол соответствует направлению оси абсцисс.
  *              Положительные значения соответствуют повороту по часовой стрелке.
  * @param angularSpeed Возвращает скорость вращения объекта.
  *                     Положительные значения соответствуют вращению по часовой стрелке.
  * @param radius Возвращает радиус объекта.
  * @param remainingLifetime Возвращает количество тиков, по прошествии которого лужа мазута полностью высохнет.
  */
class OilSlick(id: Long,
               mass: Double,
               x: Double,
               y: Double,
               speedX: Double,
               speedY: Double,
               angle: Double,
               angularSpeed: Double,
               radius: Double,
               val remainingLifetime: Int) extends CircularUnit(id, mass, x, y, speedX, speedY, angle, angularSpeed, radius)

object OilSlick extends CanBeEmpty[OilSlick]