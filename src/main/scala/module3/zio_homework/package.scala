package module3

import module3.zioConcurrency.{currentTime, printEffectRunningTime}
import module3.zio_homework.TimeToConsole.TimeToConsole
import zio.{Has, ULayer, ZIO, ZLayer}
import zio.clock.Clock
import zio.console._
import zio.duration.durationInt
import zio.random._

import java.io.IOException
import scala.language.postfixOps

package object zio_homework {
  /**
   * 1.
   * Используя сервисы Random и Console, напишите консольную ZIO программу которая будет предлагать пользователю угадать число от 1 до 3
   * и печатать в когнсоль угадал или нет. Подумайте, на какие наиболее простые эффекты ее можно декомпозировать.
   */

  def getGuessed: ZIO[Console, IOException, Int] = getStrLn
    .flatMap(str => ZIO.effect(str.toInt))
    .orElse(putStrLn("Enter value") zipRight getGuessed)

  def play(number: Int): ZIO[Console, Throwable, Unit] = getGuessed.flatMap {
    case value if (1 > value || value > 3) => putStrLn("Not correct value") zipRight play(number)
    case value if (value != number) => putStrLn("Not correct!") zipRight play(number)
    case _ => putStrLn("You are win!")
  }

  lazy val guessProgram: ZIO[Console with Random, Throwable, Unit] = for {
    number <- nextIntBetween(1, 4)
    _ <- putStrLn("Enter value between 1 and 3")
    _ <- play(number)
    } yield ()

  /**
   * 2. реализовать функцию doWhile (общего назначения), которая будет выполнять эффект до тех пор, пока его значение в условии не даст true
   *
   */

  def doWhile[R, E, A](condition: A => Boolean, effect: ZIO[R, E, A]): ZIO[R, E, A] =
    effect.repeatWhile(condition)

  /**
   * 3. Реализовать метод, который безопасно прочитает конфиг из файла, а в случае ошибки вернет дефолтный конфиг
   * и выведет его в консоль
   * Используйте эффект "load" из пакета config
   */


  def loadConfigOrDefault: ZIO[Console, IOException, config.AppConfig] =
    config.load.orElse(ZIO.succeed(config.AppConfig("host", "port")).tap(config => putStrLn(config.toString)))


  /**
   * 4. Следуйте инструкциям ниже для написания 2-х ZIO программ,
   * обратите внимание на сигнатуры эффектов, которые будут у вас получаться,
   * на изменение этих сигнатур
   */


  /**
   * 4.1 Создайте эффект, который будет возвращать случайеым образом выбранное число от 0 до 10 спустя 1 секунду
   * Используйте сервис zio Random
   */
  lazy val eff: ZIO[Random with Clock, Nothing, Int] =
    nextIntBetween(0, 11).delay(1.second)

  /**
   * 4.2 Создайте коллукцию из 10 выше описанных эффектов (eff)
   */
  lazy val effects: Seq[ZIO[Random with Clock, Nothing, Int]] =
    (1 to 10).map(_ => eff)


  /**
   * 4.3 Напишите программу которая вычислит сумму элементов коллекци "effects",
   * напечатает ее в консоль и вернет результат, а также залогирует затраченное время на выполнение,
   * можно использовать ф-цию printEffectRunningTime, которую мы разработали на занятиях
   */

  lazy val app: ZIO[Console with Clock with Random, Any, Int] =
    printEffectRunningTime(ZIO.collectAll(effects).map(_.sum)).tap(v => putStrLn(v.toString))


  /**
   * 4.4 Усовершенствуйте программу 4.3 так, чтобы минимизировать время ее выполнения
   */

  lazy val appSpeedUp: ZIO[Console with Clock with Random, Any, Int] =
    printEffectRunningTime(ZIO.collectAllPar(effects).map(_.sum)).tap(v => putStrLn(v.toString))


  /**
   * 5. Оформите ф-цию printEffectRunningTime разработанную на занятиях в отдельный сервис, так чтобы ее
   * молжно было использовать аналогично zio.console.putStrLn например
   */

  object TimeToConsole {
    type TimeToConsole = Has[TimeLoggerClass]
    class TimeLoggerClass {
      def printEffectRunningTime[R, E, A](zio: ZIO[R, E, A]): ZIO[Console with Clock with R, Any, A] = for {
        start <- currentTime
        r <- zio
        end <- currentTime
        _ <- putStrLn(s"Running time ${end - start}")
      } yield r
    }
    val play: ULayer[Has[TimeLoggerClass]] = ZLayer.succeed(new TimeLoggerClass)

    def printEffectRunningTime[R, E, A](zio: ZIO[R, E, A]): ZIO[TimeToConsole with Console with Clock with R, Any, A] =
      ZIO.accessM[TimeToConsole with Console with Clock with R](_.get.printEffectRunningTime(zio))
  }


  /**
   * 6.
   * Воспользуйтесь написанным сервисом, чтобы созадть эффект, который будет логировать время выполнения прогаммы из пункта 4.3
   *
   *
   */

  lazy val appWithTimeLogg: ZIO[TimeToConsole with Console with Clock with Random, Any, Int] =
    TimeToConsole.printEffectRunningTime(ZIO.collectAllPar(effects).map(_.sum)).tap(v => putStrLn(v.toString))

  /**
   *
   * Подготовьте его к запуску и затем запустите воспользовавшись ZioHomeWorkApp
   */

  lazy val runApp: ZIO[Console with Random with Clock, Any, Int] =
    appWithTimeLogg.provideSomeLayer[Console with Random with Clock](TimeToConsole.play)

}
