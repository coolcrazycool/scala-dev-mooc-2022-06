package module4.homework.services

import zio.Has
import zio.Task
import module4.homework.dao.entity.User
import module4.homework.dao.entity.Role
import module4.homework.dao.repository.UserRepository
import zio.ZIO
import zio.RIO
import module4.homework.dao.entity.UserToRole
import zio.ZLayer
import zio.macros.accessible
import module4.homework.dao.entity.RoleCode
import module4.phoneBook.db

import java.sql.SQLException
import javax.sql.DataSource

@accessible
object UserService{
    type UserService = Has[Service]

    trait Service{
        def listUsers(): RIO[db.DataSource, List[User]]
        def listUsersDTO(): RIO[db.DataSource, List[UserDTO]]
        def addUserWithRole(user: User, roleCode: RoleCode): RIO[db.DataSource, UserDTO]
        def listUsersWithRole(roleCode: RoleCode): RIO[db.DataSource, List[UserDTO]]
    }

    class Impl(userRepo: UserRepository.Service) extends Service{
        val dc = db.Ctx
        import dc._

        def listUsers(): RIO[db.DataSource, List[User]] = userRepo.list()

        def userDTO(user: User): ZIO[Has[DataSource], SQLException, UserDTO] =
            for {
                userRoles <- userRepo.userRoles(user.typedId)
            } yield UserDTO(user, userRoles.toSet)

        def listUsersDTO(): RIO[db.DataSource,List[UserDTO]] =
            listUsers().flatMap {
                users => ZIO.foreach(users)(userDTO)
            }

        def addUserWithRole(user: User, roleCode: RoleCode): RIO[db.DataSource, UserDTO] = transaction(
            for {
                _ <- userRepo.createUser(user)
                _ <- userRepo.insertRoleToUser(roleCode, user.typedId)
                userDTO <- userDTO(user)
            } yield userDTO
        )

        def listUsersWithRole(roleCode: RoleCode): RIO[db.DataSource,List[UserDTO]] = for {
            users <- userRepo.listUsersWithRole(roleCode)
            result <- ZIO.foreach(users)(userDTO)
        } yield result

    }

    val live: ZLayer[UserRepository.UserRepository, Nothing, UserService] = ZLayer.fromService(r => new Impl(r))
}

case class UserDTO(user: User, roles: Set[Role])