module HWM.CLI.Command.Add (addCommand, runAdd) where

import Options.Applicative
import HWM.Core.Options (globalOptionsParser)

addCommand :: Mod CommandFields (IO ())
addCommand = command "add" $ info (helper <*> addOptionsParser) 
  (progDesc "Add a package to the workspace")

addOptionsParser :: Parser (IO ())
addOptionsParser = runAdd <$> argument str (metavar "PACKAGE" <> help "Package name to add")

runAdd :: String -> IO ()
runAdd pkg = do
  putStrLn $ "Adding package: " ++ pkg
  -- TODO: Implement actual add logic
