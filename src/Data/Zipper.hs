{- |
Module      :  Data.Zipper
Description :  Small implementation of zipper
Copyright   :  (c) Cj.bc-sd a.k.a Cj-bc
License     :  GPL-3
Maintainer  :  cj.bc-sd@outlook.jp
Stability   :  experimental
Portability :  portable

This file is part of vmc-mixer.

vmc-mixer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

vmc-mixer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with vmc-mixer. If not, see <https://www.gnu.org/licenses/>.

-}
{-# LANGUAGE TemplateHaskell #-}
module Data.Zipper (
  Zipper(Zipper)
  -- * Lenses
  -- ** Access to contents
, peeked
, before
, after
  -- ** Get new one
, next
, previous
) where

import Lens.Micro (to, SimpleGetter)
import Lens.Micro.TH (makeLenses)

-- | Simple zipper
data Zipper a = Zipper { _peeked :: a
                       , _before :: [a]
                       , _after  :: [a]
                       } deriving (Eq)
makeLenses  ''Zipper

-- | Return 'Zipper' that peeks next value.
--
-- Return 'Nothing' if it's peeking last value.
next :: SimpleGetter (Zipper a) (Maybe (Zipper a))
next = let getter (Zipper c bs []) = Nothing
           getter (Zipper c bs (a:as)) = Just $ Zipper a (c:bs) as
  in to getter

-- | Return 'Zipper' that peeks previous value.
--
--  Return 'Nothing' if it's peeking first value.
previous :: SimpleGetter (Zipper a) (Maybe (Zipper a))
previous = let getter (Zipper c [] as)     = Nothing
               getter (Zipper c (b:bs) as) = Just $ Zipper b bs (c:as)
  in to getter
