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
{-# LANGUAGE ScopedTypeVariables #-}

{-| Utility functions for "BDCS.API"
-}
module BDCS.API.Utils(applyLimits,
                      argify,
                      caseInsensitive,
                      caseInsensitiveT,
                      GitLock(..),
                      maybeIO,
                      maybeThrow)
  where

import qualified Control.Concurrent.ReadWriteLock as RWL
import           Control.Exception
import           Control.Monad (liftM)
import           Data.List.Split (splitOn)
import qualified Data.Text as T
import qualified GI.Ggit as Git

-- | Git Repository and its RWLock
--
-- This is used to control access to the Git repository. Users should take the lock like this:
--
-- > RWL.withRead (gitRepoLock repoLock)
data GitLock = GitLock
  { gitRepoLock :: RWL.RWLock
  , gitRepo     :: Git.Repository
  }

-- | Turn exceptions from an action into 'Nothing'
maybeIO :: IO a -> IO (Maybe a)
maybeIO act = handle (\(_::SomeException) -> (return Nothing)) (Just `liftM` act)

-- | Throw an IO error when a 'Maybe' is 'Nothing'
maybeThrow :: (Exception e) => e -> Maybe a -> IO a
maybeThrow err Nothing = throwIO err
maybeThrow _ (Just v)  = return v

-- | Take a list of possiby comma, or comma-space, separated options and turn it into a list of options
argify :: Foldable t => t String -> [String]
argify xs = filter (/= "") $ concatMap (splitOn ",") xs

-- | Compare 2 strings case-insensitively
--
-- Takes into account unicode
caseInsensitive :: String -> String -> Ordering
caseInsensitive a b = T.toCaseFold (T.pack a) `compare` T.toCaseFold (T.pack b)

-- | Compare 2 T.Text's case-insensitively
--
-- Takes into account unicode
caseInsensitiveT :: T.Text -> T.Text -> Ordering
caseInsensitiveT a b = T.toCaseFold a `compare` T.toCaseFold b

-- | Apply limit and offset to a list.
applyLimits :: Int -> Int -> [a] -> [a]
applyLimits limit offset = take limit . drop offset
