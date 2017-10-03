module App.Password where

import Control.Monad.IO.Class (MonadIO(liftIO))
import Crypto.Scrypt
       (EncryptedPass(EncryptedPass), Pass(Pass), encryptPassIO',
        getEncryptedPass, verifyPass')
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8With, encodeUtf8)
import Data.Text.Encoding.Error (lenientDecode)

import App.Db.Types (PasswordHash(..))

data PasswordCheck
  = PasswordIncorrect
  | PasswordCorrect
  deriving (Show)

foldPasswordCheck
  :: a -- ^ Value to use for incorrect password.
  -> a -- ^ Value to use for correct password.
  -> PasswordCheck
  -> a
foldPasswordCheck incorrectVal _ PasswordIncorrect = incorrectVal
foldPasswordCheck _ correctVal PasswordCorrect = correctVal

hashPassword
  :: MonadIO m
  => Text -> m PasswordHash
hashPassword password =
  let pass = passwordToPass password
  in encryptedPassToPasswordHash <$> liftIO (encryptPassIO' pass)

passwordToPass :: Text -> Pass
passwordToPass = Pass . encodeUtf8

encryptedPassToPasswordHash :: EncryptedPass -> PasswordHash
encryptedPassToPasswordHash =
  PasswordHash . decodeUtf8With lenientDecode . getEncryptedPass

passwordHashToEncryptedPass :: PasswordHash -> EncryptedPass
passwordHashToEncryptedPass = EncryptedPass . encodeUtf8 . unPasswordHash

checkPassword :: Text -> PasswordHash -> PasswordCheck
checkPassword password passwordHash =
  let res =
        verifyPass'
          (passwordToPass password)
          (passwordHashToEncryptedPass passwordHash)
  in case res of
       True -> PasswordCorrect
       False -> PasswordIncorrect
