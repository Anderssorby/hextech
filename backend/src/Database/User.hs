module Database.User where

import Control.Arrow (returnA)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Text (Text)
import Database.Base
import GHC.Int (Int64)
import Opaleye

-------------------------------------------------------------------------------
data UserT a b c d
  = User
      { userId :: a,
        userEmail :: b,
        userPasswordHash :: c,
        userName :: d
      }

$(makeAdaptorAndInstance "pUser" ''UserT)

type User =
  Entity
    ( UserT
        Int
        Text
        Text
        Text
    )

type UserWriteField =
  EntityWriteField
    ( UserT
        (Maybe (F SqlInt4)) -- use Maybe because we don't need to specify id when inserting
        (F SqlText)
        (F SqlText)
        (F SqlText)
    )

type UserField =
  EntityField
    ( UserT (F SqlInt4)
        (F SqlText)
        (F SqlText)
        (F SqlText)
    )

userTable :: Table UserWriteField UserField
userTable =
  table "users" . pEntity . withTimestampFields $
    pUser
      User
        { userId = tableField "id",
          userEmail = tableField "email",
          userPasswordHash = tableField "password_hash",
          userName = tableField "name"
        }
-------------------------------------------------------------------------------
type UserID = Int

-------------------------------------------------------------------------------
userSelect :: Select UserField
userSelect = selectTable userTable

-------------------------------------------------------------------------------
insertUser :: (Text, Text, Text) -> Insert Int64
insertUser (userEmail, userPasswordHash, userName) =
  Insert
    { iTable = userTable,
      iRows =
        withTimestamp
          [ User
              { userId = Nothing,
                userEmail = toFields userEmail,
                userPasswordHash = toFields userPasswordHash,
                userName = toFields userName
              }
          ],
      iReturning = rCount,
      iOnConflict = Nothing
    }

-------------------------------------------------------------------------------
findUserByEmail :: Text -> Select UserField
findUserByEmail email =
  proc () -> do
    user <- userSelect -< ()
    let userDetail = record user
    restrict -< userEmail userDetail .== toFields email
    returnA -< user

-------------------------------------------------------------------------------
findUserByID :: UserID -> Select UserField
findUserByID id =
  proc () -> do
    user <- userSelect -< ()
    let userDetail = record user
    restrict -< userId userDetail .== toFields id
    returnA -< user

-------------------------------------------------------------------------------
updateUserPassword :: UserID -> Text -> Update Int64
updateUserPassword id newPasswordHash =
  Update
    { uTable = userTable,
      uUpdateWith = updateRecord updatePasswordHash,
      uWhere = ((.==) (toFields id)) . userId . record,
      uReturning = rCount
    }
  where
    updatePasswordHash userData =
      userData {userPasswordHash = toFields newPasswordHash}
