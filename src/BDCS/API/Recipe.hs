-- Copyright (C) 2017 Red Hat, Inc.
--
-- This file is part of bdcs-api.
--
-- bdcs-api is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- bdcs-api is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with bdcs-api.  If not, see <http://www.gnu.org/licenses/>.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module BDCS.API.Recipe(bumpVersion,
                       parseRecipe,
                       recipeTOML,
                       recipeTomlFilename,
                       recipeBumpVersion,
                       Recipe(..),
                       RecipeModule(..))
  where

import           BDCS.API.TOMLMediaType
import           Data.Aeson
import           Data.Aeson.Types(Result(..))
import           Data.Maybe(fromMaybe)
import qualified Data.SemVer as SV
import           Data.String.Conversions(cs)
import qualified Data.Text as T
import           Text.Printf(printf)
import           Text.Toml(parseTomlDoc)


{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

data Recipe =
    Recipe { rName              :: String
           , rVersion           :: Maybe String
           , rDescription       :: String
           , rPackages          :: [RecipeModule]
           , rModules           :: [RecipeModule]
    } deriving (Eq, Show)

instance FromJSON Recipe where
  parseJSON = withObject "recipe" $ \o -> do
      rName        <- o .:  "name"
      rVersion     <- o .:? "version"
      rDescription <- o .:  "description"
      rPackages    <- o .:? "packages" .!= []
      rModules     <- o .:? "modules" .!= []
      return Recipe{..}

instance ToJSON Recipe where
  toJSON Recipe{..} = object [
        "name"        .= rName
      , "version"     .= fromMaybe "" rVersion
      , "description" .= rDescription
      , "packages"    .= rPackages
      , "modules"    .= rModules ]

instance ToTOML Recipe where
    toTOML recipe = cs $ recipeTOML recipe

instance FromTOML Recipe where
    parseTOML toml = parseRecipe $ cs toml


data RecipeModule =
    RecipeModule { rmName         :: String
                 , rmVersion      :: String
    } deriving (Eq, Show)

instance FromJSON RecipeModule where
  parseJSON = withObject "recipe module" $ \o -> do
      rmName    <- o .: "name"
      rmVersion <- o .: "version"
      return RecipeModule{..}

instance ToJSON RecipeModule where
  toJSON RecipeModule{..} = object [
        "name"    .= rmName
      , "version" .= rmVersion ]


-- | Parse a TOML formatted string and return a Recipe
parseRecipe :: T.Text -> Either String Recipe
parseRecipe xs =
    case parseTomlDoc "" xs of
        Left err    -> Left ("Parsing TOML document failed. " ++ show err)
        Right table -> do
            let jsonValue = toJSON table
            case (fromJSON jsonValue :: Result Recipe) of
                Error err -> Left ("Converting from JSON to Recipe failed. " ++ show err)
                Success r -> Right r

-- | Convert a Recipe to a TOML string
recipeTOML :: Recipe -> T.Text
recipeTOML Recipe{..} = T.concat [nameText, versionText, descriptionText, modulesText, packagesText]
  where
    nameText = T.pack $ printf "name = \"%s\"\n" rName
    versionText = T.pack $ printf "version = \"%s\"\n" $ fromMaybe "" rVersion
    descriptionText = T.pack $ printf "description = \"%s\"\n\n" rDescription

    moduleText :: T.Text -> RecipeModule -> T.Text
    moduleText name RecipeModule{..} = T.pack $ printf "[[%s]]\nname = \"%s\"\nversion = \"%s\"\n\n" name rmName rmVersion
    packagesText = T.concat $ map (moduleText "packages") rPackages
    modulesText = T.concat $ map (moduleText "modules") rModules

-- | Convert a recipe name to a toml filename
--
-- Replaces spaces with - and append .toml
recipeTomlFilename :: String -> T.Text
recipeTomlFilename name = T.append (T.replace " " "-" (T.pack name)) ".toml"

-- | semver recipe version number bump
--
-- If neither have a version 0.0.1 is returned
-- If there is no previous version the new version is checked and returned
-- If there is no new version, but there is a previous one, bump its patch level
-- If the previous and new versions are the same, bump the patch level
-- If they are different, check and return the new version
bumpVersion :: Maybe String -> Maybe String -> Either String String
bumpVersion Nothing Nothing = Right "0.0.1"
-- Only a new version, make sure the new version can be parsed
bumpVersion Nothing (Just new_ver) =
    case SV.fromText (T.pack new_ver) of
        Right _ -> Right new_ver
        Left  _ -> Left ("Failed to parse version: " ++ new_ver)
-- If there was a previous version and no new one, bump the patch level
bumpVersion (Just prev_ver) Nothing =
    case SV.fromText (T.pack prev_ver) of
        Right version -> Right $ SV.toString $ SV.incrementPatch version
        Left _        -> Left ("Failed to parse version: " ++ prev_ver)
bumpVersion (Just prev_ver) (Just new_ver)
    | prev_ver == new_ver = bumpNewVer
    | otherwise           = checkNewVer
  where
    bumpNewVer =
        case SV.fromText (T.pack new_ver) of
            Right version -> Right $ SV.toString $ SV.incrementPatch version
            Left _        -> Left ("Failed to parse version: " ++ new_ver)
    checkNewVer =
        case SV.fromText (T.pack new_ver) of
            Right _ -> Right new_ver
            Left  _ -> Left ("Failed to parse version: " ++ new_ver)

-- | Bump or replace a Recipe Version with a new one
--
-- Pass the new recipe and the version from the previous recipe
-- Returns a new recipe with the correct version
recipeBumpVersion :: Recipe -> Maybe String -> Either String Recipe
recipeBumpVersion recipe prev_version = case bumpVersion prev_version (rVersion recipe) of
    Right version -> Right recipe { rVersion = Just version }
    Left  err     -> Left  err
