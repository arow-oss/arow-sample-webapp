module App.Cli where

import Control.Monad.Reader (runReaderT)
import Data.Text (Text, pack)
import Options.Applicative
       (InfoMod, Parser, ParserInfo, ReadM, argument, auto, command,
        execParser, fullDesc, header, helper, info, metavar, progDesc,
        str, subparser)
import Data.Semigroup ((<>))
import Servant (runHandler)

import App.Config (Config(..), configFromEnv)
import App.Db (Key, SqlBackend, ToBackendKey, toSqlKey)
import App.Monad (AppM)

data AddAdminOpts = AddAdminOpts
  { addAdminEmail :: Text
  , addAdminPassword :: Text
  , addAdminName :: Text
  } deriving (Eq, Read, Show)

data AddUserOpts = AddUserOpts
  { addUserEmail :: Text
  , addUserPassword :: Text
  , addUserName :: Text
  } deriving (Eq, Read, Show)

-- | Sum-type to represent all possible commands.
data Command
  = AddAdmin AddAdminOpts
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
    go (AddUser addUserOpts) =
      runAddUserCmd addUserOpts

runAddAdminCmd :: AddAdminOpts -> AppM ()
runAddAdminCmd (AddAdminOpts email password name) = do
  undefined
  -- (Entity companyUserId _) <- dbCreateCompanyUser companyId email name password
  -- putStrLn $
  --   "Created company user \"" <> name <> "\" with id: " <>
  --   tshow (fromSqlKey companyUserId)

runAddUserCmd :: AddUserOpts -> AppM ()
runAddUserCmd (AddUserOpts email password name) = do
  undefined
  -- (Entity companyUserId _) <- dbCreateCompanyUser companyId email name password
  -- putStrLn $
  --   "Created company user \"" <> name <> "\" with id: " <>
  --   tshow (fromSqlKey companyUserId)
