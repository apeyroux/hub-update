{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE ScopedTypeVariables #-}

--
-- config.json sample :
--
-- {
--     "dockerbin":"/nix/store/4d6gyiiwk76i8gmpwvca3mdll88h2s2x-system-path/bin/docker",
--     "login":"admin",
--     "password":"admin",
--     "hub":"hub.mydomain.com",
--     "images":[
-- 	{
-- 	    "pull_repository":"debian",
-- 	    "tag":"latest",
-- 	    "push_repository":"41px/debian"
-- 	}, {
-- 	    "pull_repository":"ubuntu",
-- 	    "tag":"latest",
-- 	    "push_repository":"41px/ubuntu"
-- 	}
--     ]
-- }


module Main where

import           System.Environment (getArgs)
import           Control.Exception
import           Data.Aeson
import           Data.Maybe
import           Data.Monoid
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import           Data.Text.Encoding as E
import           GHC.Generics
import           Shelly
import           Data.Conduit
import           Data.Conduit.Binary
default (T.Text)

data ConfigurationImage = ConfigurationImage {
  ciPullRepository :: T.Text,
  ciTag :: T.Text,
  ciPushRepository :: T.Text
  } deriving (Generic, Show)

data Configuration = Configuration {
  cfgDockerBin :: T.Text,
  cfgLogin :: T.Text,
  cfgPassword :: T.Text,
  cfgHub :: T.Text,
  cfgImages :: [ConfigurationImage]
  } deriving (Generic, Show)

instance FromJSON Configuration where
  parseJSON (Object v) = Configuration <$>
    v .: "dockerbin" <*>
    v .: "login" <*>
    v .: "password" <*>
    v .: "hub" <*>
    v .: "images"

instance FromJSON ConfigurationImage where
  parseJSON (Object v) = ConfigurationImage <$>
    v .: "pull_repository" <*>
    v .: "tag" <*>
    v .: "push_repository"

dlogin :: Configuration -> Sh T.Text
dlogin cfg = run docker ["login",
                    hub,
                    "--username=" <> login,
                    "--password=" <> password]
  where
    docker = fromText $ cfgDockerBin cfg
    hub = cfgHub cfg
    login = cfgLogin cfg
    password = cfgPassword cfg

docker :: [T.Text] -> Configuration -> Sh T.Text
docker = flip $ run . fromText . cfgDockerBin

dpull :: ConfigurationImage -> Configuration -> Sh T.Text
dpull ci = docker ["pull", ciPullRepository ci  <> ":" <> ciTag ci]

dpush :: ConfigurationImage -> Configuration -> Sh T.Text
dpush ci cfg = docker ["push", (cfgHub cfg) <> "/" <> ciPushRepository ci] cfg

dtag :: ConfigurationImage -> Configuration -> Sh T.Text
dtag ci cfg = docker ["tag",
                ciPullRepository ci <> ":" <> ciTag ci,
                (cfgHub cfg) <> "/" <> ciPushRepository ci <> ":" <> ciTag ci] cfg

-- trap js in handler
-- and parse js in aeson and run cmd
dimgjs :: Configuration -> Sh T.Text
dimgjs = docker ["images",
                 "--format",
                 "{\"id\":\"{{.ID}}\",\"tag\":\"{{.Tag}}\",\"repo\":\"{{.Repository}}\"}"]

main :: IO ()
main = do
  args <- getArgs
  case args of
    [config,verbose] -> runWith config (read verbose::Bool)
    _                -> usage
  where
    usage = putStrLn "Usage\n\thub-update /path/config.json verbose (True||False)\n\nExemple:\n\thub-update /etc/hub.json True"
    runWith file verbose = do
      -- putStrLn "login ..."
      -- config file >>= sh . dlogin
      putStrLn "start pull action ..."
      config file >>= dockerWith dpull verbose
      putStrLn "start tag action ..."
      config file >>= dockerWith dtag verbose
      putStrLn "start push action ..."
      config file >>= dockerWith dpush verbose
      -- shelly $ runHandle (fromText "/run/current-system/sw/bin/docker") dopts (\h-> runResourceT $ CB.sourceHandle h $$ CB.sinkFile "/tmp/d.js")
      -- shelly $ runHandle (fromText "/run/current-system/sw/bin/docker") dopts (\h-> CB.sourceHandle h $$ sinkParser json)
      return ()
    -- sh = shelly . silently . print_stderr True
    sh verbose = if verbose then
           (shelly . print_stderr True)
         else
           (shelly . silently . print_stderr True)
    dockerWith f verbose = (\cfg -> (sh verbose) . mapM (\img -> processImgWith f img cfg) $ cfgImages cfg)
    showException (e::SomeException) = "Ooooups: " <> (T.pack $ show e)
    processImgWith f img cfg = catch_sh (f img cfg) (pure . showException)
    config file = BL.readFile file
               >>= pure . (\js -> decode js::Maybe Configuration)
               >>= pure . fromJust
