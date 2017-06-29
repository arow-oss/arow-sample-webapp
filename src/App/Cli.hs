module App.Cli where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (runReaderT)
import Data.Text (Text, pack, unpack)
import Options.Applicative
       (InfoMod, Parser, ParserInfo, ReadM, argument, auto, command,
        execParser, fullDesc, header, helper, info, metavar, progDesc,
        str, subparser)
import Data.Semigroup ((<>))
import Servant (runHandler)
import System.Exit (ExitCode(ExitFailure), exitWith)

import App.Config (configFromEnv)
import App.Db
       (Key(..), SqlBackend, ToBackendKey, fromSqlKey, dbAddAdmin,
        dbAddCompanyUser, dbAddUser, runDb, toSqlKey)
import App.Monad (AppM)

data AddAdminOpts = AddAdminOpts
  { addAdminEmail :: Text
  , addAdminPassword :: Text
  , addAdminName :: Text
  } deriving (Eq, Read, Show)

data AddCompanyUserOpts = AddCompanyUserOpts
  { addCompanyUserEmail :: Text
  , addCompanyUserPassword :: Text
  , addCompanyUserName :: Text
  } deriving (Eq, Read, Show)

data AddUserOpts = AddUserOpts
  { addUserEmail :: Text
  , addUserPassword :: Text
  , addUserName :: Text
  } deriving (Eq, Read, Show)

-- | Sum-type to represent all possible commands.
data Command
  = AddAdmin AddAdminOpts
  | AddCompanyUser AddCompanyUserOpts
  | AddUser AddUserOpts
  deriving (Eq, Read, Show)

defaultMainCli :: IO ()
defaultMainCli = parseOptions >>= runCommand

sqlKeyReadM
  :: ToBackendKey SqlBackend record
  => ReadM (Key record)
sqlKeyReadM = toSqlKey <$> auto

-- | 'ReadM' parser for 'Text'.  Similar to 'str'.
txt :: ReadM Text
txt = pack <$> str

addAdminParserInfo :: ParserInfo Command
addAdminParserInfo =
  info
    (helper <*> fmap AddAdmin addAdminParser)
    (fullDesc `mappend` progDesc "Add a new admin.")
  where
    addAdminParser :: Parser AddAdminOpts
    addAdminParser =
      AddAdminOpts
        <$> argument txt (metavar "ADMIN_EMAIL")
        <*> argument txt (metavar "ADMIN_PASSWORD")
        <*> argument txt (metavar "ADMIN_NAME")

addCompanyUserParserInfo :: ParserInfo Command
addCompanyUserParserInfo =
  info
    (helper <*> fmap AddCompanyUser addCompanyUserParser)
    (fullDesc `mappend` progDesc "Add a new user.")
  where
    addCompanyUserParser :: Parser AddCompanyUserOpts
    addCompanyUserParser =
      AddCompanyUserOpts
        <$> argument txt (metavar "COMPANY_USER_EMAIL")
        <*> argument txt (metavar "COMPANY_USER_PASSWORD")
        <*> argument txt (metavar "COMPANY_USER_NAME")

addUserParserInfo :: ParserInfo Command
addUserParserInfo =
  info
    (helper <*> fmap AddUser addUserParser)
    (fullDesc `mappend` progDesc "Add a new user.")
  where
    addUserParser :: Parser AddUserOpts
    addUserParser =
      AddUserOpts
        <$> argument txt (metavar "USER_EMAIL")
        <*> argument txt (metavar "USER_PASSWORD")
        <*> argument txt (metavar "USER_NAME")

parseOptions :: IO Command
parseOptions = execParser topOpts
  where
    topOpts :: ParserInfo Command
    topOpts = info (helper <*> commandSubParser) topDesc

    headerMsg :: String
    headerMsg = "arow-sample-cli - add a new user"

    progDescMsg :: String
    progDescMsg = "Add a new user to the database."

    topDesc :: InfoMod Command
    topDesc = fullDesc `mappend` progDesc progDescMsg `mappend` header headerMsg

    commandSubParser :: Parser Command
    commandSubParser =
      subparser $
        command "add-admin" addAdminParserInfo `mappend`
        command "add-company-user" addCompanyUserParserInfo `mappend`
        command "add-user" addUserParserInfo

runCommand :: Command -> IO ()
runCommand cmd = do
  config <- configFromEnv
  eitherResult <- runHandler $ runReaderT (go cmd) config
  case eitherResult of
    Right () -> pure ()
    Left servanterr -> putStrLn $ "ERROR: " <> show servanterr
  where
    go :: Command -> AppM ()
    go (AddAdmin addAdminOpts) =
      runAddAdminCmd addAdminOpts
    go (AddCompanyUser addCompanyUserOpts) =
      runAddCompanyUserCmd addCompanyUserOpts
    go (AddUser addUserOpts) =
      runAddUserCmd addUserOpts

runAddAdminCmd :: AddAdminOpts -> AppM ()
runAddAdminCmd (AddAdminOpts email password name) = do
  maybeAdminKey <- runDb $ dbAddAdmin email password name
  case maybeAdminKey of
    Nothing ->
      die $ "Admin with email address " <> unpack email <> " already exists."
    Just adminKey ->
      liftIO . putStrLn $
        "Successfully added Admin.  ID: " <> show (fromSqlKey adminKey)

runAddUserCmd :: AddUserOpts -> AppM ()
runAddUserCmd (AddUserOpts email password name) = do
  maybeUserKey <- runDb $ dbAddUser email password name
  case maybeUserKey of
    Nothing ->
      die $ "User with email address " <> unpack email <> " already exists."
    Just userKey ->
      liftIO . putStrLn $
        "Successfully added User.  ID: " <> show (fromSqlKey userKey)

runAddCompanyUserCmd :: AddCompanyUserOpts -> AppM ()
runAddCompanyUserCmd (AddCompanyUserOpts email password name) = do
  maybeCompanyUserKey <- runDb $ dbAddCompanyUser email password name
  case maybeCompanyUserKey of
    Nothing ->
      die $
        "Company User with email address " <>
        unpack email <>
        " already exists."
    Just companyUserKey ->
      liftIO . putStrLn $
        "Successfully added Company User.  ID: " <>
        show (fromSqlKey companyUserKey)

die :: MonadIO m => String -> m a
die msg = do
  liftIO . putStrLn $ "ERROR: " <> msg
  liftIO . exitWith $ ExitFailure 1
