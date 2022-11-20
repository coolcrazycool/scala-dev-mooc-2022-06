package module3.cats_effect_homework

import cats.effect.Sync
import cats.implicits._
import Wallet._

// DSL управления электронным кошельком
trait Wallet[F[_]] {
  // возвращает текущий баланс
  def balance: F[BigDecimal]

  // пополняет баланс на указанную сумму
  def topup(amount: BigDecimal): F[Unit]

  // списывает указанную сумму с баланса (ошибка если средств недостаточно)
  def withdraw(amount: BigDecimal): F[Either[WalletError, Unit]]
}

// Игрушечный кошелек который сохраняет свой баланс в файл
// todo: реализовать используя java.nio.file._
// Насчёт безопасного конкуррентного доступа и производительности не заморачиваемся, делаем максимально простую рабочую имплементацию. (Подсказка - можно читать и сохранять файл на каждую операцию).
// Важно аккуратно и правильно завернуть в IO все возможные побочные эффекты.
//
// функции которые пригодятся:
// - java.nio.file.Files.write
// - java.nio.file.Files.readString
// - java.nio.file.Files.exists
// - java.nio.file.Paths.get
final class FileWallet[F[_] : Sync](id: WalletId) extends Wallet[F] {
  def balance: F[BigDecimal] = for {
    path <- Sync[F].pure(java.nio.file.Paths.get("/Users/18672585/", s"wallet_$id.txt"))
    exists <- Sync[F].delay(java.nio.file.Files.exists(path))
    balance <- Sync[F].pure(if (exists) new String(java.nio.file.Files.readAllBytes(path)) match {
      case value if value.isEmpty => BigDecimal.valueOf(0)
      case value => BigDecimal.valueOf(value.toDouble)
    } else BigDecimal.valueOf(0))
  } yield balance

  def topup(amount: BigDecimal): F[Unit] = for {
    balance <- balance
    path <- Sync[F].pure(java.nio.file.Paths.get("/Users/18672585/", s"wallet_$id.txt"))
    _ <- Sync[F].delay(java.nio.file.Files.write(path, (balance + amount).toString.getBytes()))
  } yield ()

  def withdraw(amount: BigDecimal): F[Either[WalletError, Unit]] = for {
    balance <- balance
    path <- Sync[F].pure(java.nio.file.Paths.get("/Users/18672585/", s"wallet_$id.txt"))
    res <- Sync[F].pure(
      if (balance - amount < 0) {
        Left(BalanceTooLow)
      } else {
        java.nio.file.Files.write(path, (balance - amount).toString.getBytes())
        Right(())
      }
    )
  } yield res
}

object Wallet {

  // todo: реализовать конструктор
  // внимание на сигнатуру результата - инициализация кошелька имеет сайд-эффекты
  // Здесь нужно использовать обобщенную версию уже пройденного вами метода IO.delay,
  // вызывается она так: Sync[F].delay(...)
  // Тайпкласс Sync из cats-effect описывает возможность заворачивания сайд-эффектов
  def fileWallet[F[_] : Sync](id: WalletId): F[Wallet[F]] = Sync[F].delay(new FileWallet[F](id))

  type WalletId = String

  sealed trait WalletError

  case object BalanceTooLow extends WalletError
}
