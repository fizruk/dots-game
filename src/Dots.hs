{-# LANGUAGE RecordWildCards #-}
module Dots where

import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid
import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Network.WebSockets
import Text.Read

-- | Player's name.
type Name = String

-- | A color for the 'Dot'.
newtype DotColor = DotColor { getDotColor :: Color }

instance Show DotColor where
  show (DotColor c) = show (rgbaOfColor c)

instance Read DotColor where
  readPrec = fmap (DotColor . uncurry4 makeColor) readPrec
    where uncurry4 f (a, b, c, d) = f a b c d

-- | A dot. Each dot represents a player.
data Dot = Dot
  { dotPosition :: Point    -- ^ Dot position.
  , dotSpeed    :: Vector   -- ^ Dot speed.
  , dotColor    :: DotColor -- ^ Dot color.
  } deriving (Show, Read)

-- | A universe all 'Dot's live in.
data Universe = Universe
  { universeDots :: Map Name Dot  -- ^ All 'Dot's in the universe, by player name.
  } deriving (Show, Read)

-- | An empty 'Universe'.
emptyUniverse :: Universe
emptyUniverse = Universe Map.empty

-- | Move a 'Dot' using its speed.
moveDot :: Float -> Dot -> Dot
moveDot dt dot = moveDotByVector (mulSV dt (dotSpeed dot)) dot

-- | Move a 'Dot' by a given 'Vector'.
moveDotByVector :: Vector -> Dot -> Dot
moveDotByVector (dx, dy) dot@Dot{ dotPosition = (x, y) }
  = dot { dotPosition = (x + dx, y + dy) }

-- | Accelerate 'Dot' by a given 'Vector'.
accelDotByVector :: Vector -> Dot -> Dot
accelDotByVector (dx, dy) dot@Dot{ dotSpeed = (x, y) }
  = dot { dotSpeed = (x + dx, y + dy) }

-- | Directions.
data Dir
  = L   -- ^ Left.
  | R   -- ^ Right.
  | U   -- ^ Up.
  | D   -- ^ Down.
  deriving (Show, Read)

-- | A unit vector pointing in a given direction.
dirVector :: Dir -> Vector
dirVector L = (-1,  0)
dirVector R = ( 1,  0)
dirVector U = ( 0,  1)
dirVector D = ( 0, -1)

-- | Draw the whole 'Universe'.
drawUniverse :: Universe -> Picture
drawUniverse = foldMap drawDotWithName . Map.toList . universeDots

-- | Draw a single 'Dot' with its name just above.
drawDotWithName :: (Name, Dot) -> Picture
drawDotWithName (name, Dot{..}) =
  color (getDotColor dotColor) $
  uncurry translate dotPosition $
    drawName <> drawDot
  where
    drawName = translate 0 3 (scale 0.1 0.1 (text name))
    drawDot = thickCircle 1 1

-- | Update all 'Dot's in the 'Universe'.
updateUniverse :: Float -> Universe -> Universe
updateUniverse dt (Universe u) = Universe (fmap (moveDot dt) u)

-- | Player actions.
data Action
  = ActionAccel Dir   -- ^ Accelerate in a given direction.
  deriving (Show, Read)

-- | Handle a player's 'Action' and update the 'Universe' accordingly.
handlePlayerAction :: Name -> Action -> Universe -> Universe
handlePlayerAction name (ActionAccel dir) (Universe u)
  = Universe (Map.adjust (accelDotByVector (dirVector dir)) name u)

-- | A list of different 'DotColor's.
defaultDotColors :: [DotColor]
defaultDotColors = map DotColor [blue, green, red, white, yellow, cyan, magenta, rose, violet, azure, aquamarine, chartreuse, orange]

-- | Add a new player to the 'Universe'.
addPlayer :: Name -> DotColor -> Universe -> Universe
addPlayer name color (Universe u) = Universe (Map.insert name (Dot (0, 0) (0, 0) color) u)

-- =====================================
-- WebSockersData instances are needed
-- to send/receive Haskell structures
-- over websockets
-- =====================================

instance WebSocketsData Action where
  fromLazyByteString = read . BL8.unpack
  toLazyByteString   = BL8.pack . show

instance WebSocketsData Universe where
  fromLazyByteString = read . BL8.unpack
  toLazyByteString   = BL8.pack . show

