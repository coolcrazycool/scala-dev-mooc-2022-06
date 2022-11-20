package module3.cats_effect_homework

import cats.effect.{IO, IOApp, Spawn}
import cats.implicits._
import module3.cats_effect_homework.Wallet.WalletId

import scala.concurrent.duration.{DurationInt, FiniteDuration}

// Поиграемся с кошельками на файлах и файберами.

// Нужно написать программу где инициализируются три разных кошелька и для каждого из них работает фоновый процесс,
// который регулярно пополняет кошелек на 100 рублей раз в определенный промежуток времени. Промежуток надо сделать разный, чтобы легче было наблюдать разницу.
// Для определенности: первый кошелек пополняем раз в 100ms, второй каждые 500ms и третий каждые 2000ms.
// Помимо этих трёх фоновых процессов (подсказка - это файберы), нужен четвертый, который раз в одну секунду будет выводить балансы всех трех кошельков в консоль.
// Основной процесс программы должен просто ждать ввода пользователя (IO.readline) и завершить программу (включая все фоновые процессы) когда ввод будет получен.
// Итого у нас 5 процессов: 3 фоновых процесса регулярного пополнения кошельков, 1 фоновый процесс регулярного вывода балансов на экран и 1 основной процесс просто ждущий ввода пользователя.

// Можно делать всё на IO, tagless final тут не нужен.

// Подсказка: чтобы сделать бесконечный цикл на IO достаточно сделать рекурсивный вызов через flatMap:
// def loop(): IO[Unit] = IO.println("hello").flatMap(_ => loop())
object WalletFibersApp extends IOApp.Simple {

  def giveMeYourMoney(
                       wallet: Wallet[IO],
                       amount: BigDecimal,
                       period: FiniteDuration
                     ): IO[Unit] =
    wallet
      .topup(amount)
      .flatMap(_ => IO.sleep(period))
      .flatMap(_ => giveMeYourMoney(wallet, amount, period))

  def walletDescription(wallets: List[Wallet[IO]]): IO[Unit] = for {
    w1 <- wallets.head.balance
    w2 <- wallets(1).balance
    w3 <- wallets.last.balance

    _ <- IO.println(s"Balance 1: ${w1}\n Balance 2: ${w2}\n Balance 3: ${w3}\n") *> IO.sleep(1.seconds) *> walletDescription(wallets)
  } yield ()

  def run: IO[Unit] =
    for {
      _ <- IO.println("Press any key to stop...")
      wallet1 <- Wallet.fileWallet[IO]("1")
      wallet2 <- Wallet.fileWallet[IO]("2")
      wallet3 <- Wallet.fileWallet[IO]("3")
      // todo: запустить все файберы и ждать ввода от пользователя чтобы завершить работу
      _ <- Spawn[IO].start(giveMeYourMoney(wallet1, BigDecimal(100), 200.millisecond))
      _ <- Spawn[IO].start(giveMeYourMoney(wallet2, BigDecimal(100), 500.millisecond))
      _ <- Spawn[IO].start(giveMeYourMoney(wallet3, BigDecimal(100), 2000.millisecond))
      _ <- Spawn[IO].start(walletDescription(List(wallet1, wallet2, wallet3)))
      _ <- IO.readLine
    } yield ()

}
