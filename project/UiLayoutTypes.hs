module UiLayoutTypes
  ( Window (..)
  , Layout (..)
  , Props (..)
  , Size (..)
  , Direction (..)
  , Resolved (..)
  , defaultProps
  , rowProps
  , colProps
  , box
  , row
  , col
  ) where

data Window = Window
  { windowWidth :: Int
  , windowHeight :: Int
  , windowLayout :: Layout
  }
  deriving (Eq, Show)

data Layout = Layout Props [Layout]
  deriving (Eq, Show)

data Props = Props
  { propWidth :: Size
  , propHeight :: Size
  , propDir :: Direction
  , propColor :: Maybe String
  }
  deriving (Eq, Show)

data Size = Px Int | Pct Double | Fill
  deriving (Eq, Show)

data Direction = Row | Col
  deriving (Eq, Show)

data Resolved = Resolved
  { rx :: Int
  , ry :: Int
  , rw :: Int
  , rh :: Int
  , rDir :: Direction
  , rColor :: Maybe String
  , rChildren :: [Resolved]
  }
  deriving (Eq, Show)

defaultProps :: Props
defaultProps =
  Props
    { propWidth = Fill
    , propHeight = Fill
    , propDir = Col
    , propColor = Nothing
    }

rowProps :: Props
rowProps = defaultProps {propDir = Row}

colProps :: Props
colProps = defaultProps {propDir = Col}

box :: Size -> Size -> Maybe String -> [Layout] -> Layout
box w h c = Layout defaultProps {propWidth = w, propHeight = h, propColor = c}

row :: [Layout] -> Layout
row = Layout rowProps

col :: [Layout] -> Layout
col = Layout colProps
