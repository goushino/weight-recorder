{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TemplateHaskell #-}
{-# LANGUAGE DataKinds, DeriveGeneric #-}

module Model.User
  ( NewUser(NewUser, nuName, nuPassword)
  , nuName'
  , nuPassword'
  , insertUser
  , selectUser
  )
where

import GHC.Generics (Generic)
import Control.Exception (catch)
import Crypto.BCrypt ( hashPasswordUsingPolicy
                     , slowerBcryptHashingPolicy
                     , validatePassword
                     )
import qualified Data.ByteString as BS
import Data.Text (pack, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Database.HDBC ( IConnection
                     , SqlError
                     , withTransaction
                     )
import Database.HDBC.Query.TH (makeRelationalRecord)
import qualified Database.HDBC.Record as DHR
import qualified Database.Relational as HRR
import Data.Functor.ProductIsomorphic ((|$|), (|*|))
import qualified Entity.User as User
import System.IO (hPrint, hPutStrLn, stderr)

data NewUser = NewUser
  { nuName :: !String
  , nuPassword :: !String
  } deriving (Generic)

makeRelationalRecord ''NewUser

piNewUser :: HRR.Pi User.User NewUser
piNewUser = NewUser |$| User.name' |*| User.password'

insertUser :: IConnection c => NewUser -> c -> IO Integer
insertUser u conn = do
  mHashed <- hashPasswordUsingPolicy slowerBcryptHashingPolicy $ enc . nuPassword $ u
  case mHashed of
    Nothing -> do
      hPutStrLn stderr "Failed to hash password"
      return 0
    Just hashed -> do
      let ins = HRR.typedInsert' HRR.defaultConfig User.tableOfUser piNewUser
          u' = u { nuPassword = dec hashed }
      withTransaction conn $
        \conn' ->
          DHR.runInsert conn' ins u' `catch`
            \e -> do
              hPrint stderr (e :: SqlError)
              return 0

enc :: String -> BS.ByteString
enc = encodeUtf8 . pack

dec :: BS.ByteString -> String
dec = unpack . decodeUtf8

selectUser :: IConnection c => String -> String -> c -> IO (Maybe User.User)
selectUser name pass conn = do
  user <- DHR.runQuery conn q name >>= DHR.listToUnique
  return $ user >>= checkHash
  where
    q :: HRR.Query String User.User
    q = HRR.relationalQuery . HRR.relation' . HRR.placeholder $
      \ph -> do
        a <- HRR.query User.user
        HRR.wheres $ a HRR.! User.name' HRR..=. ph
        return a

    checkHash :: User.User -> Maybe User.User
    checkHash user
      | validated = Just user
      | otherwise = Nothing
      where
        hashed = User.password user
        validated = validatePassword (enc hashed) (enc pass)
