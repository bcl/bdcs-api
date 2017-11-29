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
{-# LANGUAGE QuasiQuotes       #-}

module RecipeSpec
  where

import           BDCS.API.Recipe
import           BDCS.API.TOMLMediaType
import           Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import           Data.Maybe(fromJust)
import           Data.String.Conversions(cs)
import           Data.String.QQ
import qualified Data.Text as T
import           Test.Hspec

-- Test data
test1TOML :: T.Text
test1TOML = [s|
name = "test-server"
version = "0.1.2"
description = "Testing git commit of a Recipe record"

[[modules]]
name = "httpd"
version = "2.4.*"

[[modules]]
name = "mod_auth_kerb"
version = "5.4"

[[modules]]
name = "mod_ssl"
version = "2.4.*"

[[modules]]
name = "php"
version = "5.4.*"

[[modules]]
name = "php-mysql"
version = "5.4.*"

[[packages]]
name = "tmux"
version = "2.2"

[[packages]]
name = "openssh-server"
version = "6.6.*"

[[packages]]
name = "rsync"
version = "3.0.*"

|]

-- Recipe record of test1TOML
test1Recipe :: Recipe
test1Recipe =
    Recipe {rName = "test-server",
            rVersion = Just "0.1.2",
            rDescription = "Testing git commit of a Recipe record",
            rPackages = [RecipeModule {rmName = "tmux", rmVersion = "2.2"},
                         RecipeModule {rmName = "openssh-server", rmVersion = "6.6.*"},
                         RecipeModule {rmName = "rsync", rmVersion = "3.0.*"}],
            rModules = [RecipeModule {rmName = "httpd", rmVersion = "2.4.*"},
                        RecipeModule {rmName = "mod_auth_kerb", rmVersion = "5.4"},
                        RecipeModule {rmName = "mod_ssl", rmVersion = "2.4.*"},
                        RecipeModule {rmName = "php", rmVersion = "5.4.*"},
                        RecipeModule {rmName = "php-mysql", rmVersion = "5.4.*"}]
    }

-- JSON version of test1Recipe
test1JSON :: BSL.ByteString
test1JSON = [s|{
"name": "test-server",
"version": "0.1.2",
"description": "Testing git commit of a Recipe record",
"packages": [{"name": "tmux", "version": "2.2"},
             {"name": "openssh-server", "version": "6.6.*"},
             {"name": "rsync", "version": "3.0.*"}],
"modules":  [{"name": "httpd", "version": "2.4.*"},
             {"name": "mod_auth_kerb", "version": "5.4"},
             {"name": "mod_ssl", "version": "2.4.*"},
             {"name": "php", "version": "5.4.*"},
             {"name": "php-mysql", "version": "5.4.*"}]
}
|]

{-# ANN module ("HLint: ignore Redundant do Found" :: String) #-}
spec :: Spec
spec =
    describe "Recipe Record" $ do
        it "Parse a TOML string and return a Recipe Record" $
            parseRecipe test1TOML `shouldBe` Right test1Recipe

        it "Output the TOML string from a Recipe Record" $
            recipeTOML test1Recipe `shouldBe` test1TOML

        it "Recipe filename from the Recipe name" $ do
            recipeTomlFilename "http server alpha" `shouldBe` "http-server-alpha.toml"
            recipeTomlFilename "jenkins-server" `shouldBe` "jenkins-server.toml"
            recipeTomlFilename "kubernetes" `shouldBe` "kubernetes.toml"

        it "Output the JSON string from a Recipe Record" $
            toJSON test1Recipe `shouldBe` fromJust (decode test1JSON)

        it "Parse a JSON string and return a Recipe Record" $
            fromJSON (fromJust $ decode test1JSON) `shouldBe` Success test1Recipe

        it "Output the TOML string from a Recipe Record" $
            toTOML test1Recipe `shouldBe` cs test1TOML

        it "Parse a TOML string and return a Recipe Record" $
            parseTOML (cs test1TOML) `shouldBe` Right test1Recipe

        it "Bump the version in various ways" $ do
            bumpVersion Nothing        Nothing        `shouldBe` Right "0.0.1"
            bumpVersion Nothing        (Just "0.1.0") `shouldBe` Right "0.1.0"
            bumpVersion (Just "0.0.2") Nothing        `shouldBe` Right "0.0.3"
            bumpVersion (Just "0.1.1") (Just "0.1.1") `shouldBe` Right "0.1.2"
            bumpVersion (Just "0.0.2") (Just "0.2.1") `shouldBe` Right "0.2.1"

        it "Bump some invalid versions" $ do
            bumpVersion Nothing        (Just "AAA")   `shouldBe` Left "Failed to parse version: AAA"
            bumpVersion (Just "AAA")   Nothing        `shouldBe` Left "Failed to parse version: AAA"
            bumpVersion (Just "BBB")   (Just "BBB")   `shouldBe` Left "Failed to parse version: BBB"
            bumpVersion (Just "AAA")   (Just "BBB")   `shouldBe` Left "Failed to parse version: BBB"

        it "Bump the version of a Recipe and return a new Recipe" $ do
            recipeBumpVersion test1Recipe Nothing        `shouldBe` Right test1Recipe { rVersion = Just "0.1.2" }
            recipeBumpVersion test1Recipe (Just "0.1.2") `shouldBe` Right test1Recipe { rVersion = Just "0.1.3" }
            recipeBumpVersion test1Recipe (Just "0.2.1") `shouldBe` Right test1Recipe { rVersion = Just "0.1.2" }
            let test2Recipe = test1Recipe {rVersion = Just "0.3.1"}
            recipeBumpVersion test2Recipe (Just "0.1.2") `shouldBe` Right test2Recipe
