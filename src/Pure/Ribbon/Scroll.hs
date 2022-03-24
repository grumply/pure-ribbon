{-# language LambdaCase, NamedFieldPuns, BlockArguments, TypeApplications, RankNTypes, PostfixOperators, OverloadedStrings, FlexibleContexts #-}
module Pure.Ribbon.Scroll (Direction(..),Scroll(..),scroll) where

import qualified Pure.Ribbon.Internal as Internal

import Pure.Elm as Pure hiding (Reversed,Scroll,content,direction,not,passive,scroll)
import Pure.Data.Lifted (Options(..))

import Control.Monad
import Data.Coerce

data Direction = Natural | Reversed
  deriving (Eq)

data Scroll = Scroll 
  { direction :: Direction 
  , content   :: Maybe (IO ()) -> Maybe (IO ()) -> [View]
  }

data Model = Model 
  { moreLeft  :: Bool 
  , moreRight :: Bool 
  , node      :: Node
  }

data Msg = SetHost Node | Recalculate | ScrollLeft | ScrollRight

scroll :: Scroll -> View
scroll s = run app s
  where
    app = App [] [Recalculate] [] (pure mdl) update view
      where
        mdl = Model reversed natural (coerce window)
          where
            natural  = direction s == Natural
            reversed = not natural

type Update = Elm Msg => Scroll -> Model -> IO Model

update :: Msg -> Update
update = \case
  SetHost h   -> setHost h
  Recalculate -> recalculate
  ScrollLeft  -> scrollLeft
  ScrollRight -> scrollRight

setHost :: Node -> Update
setHost h Scroll { direction } mdl = do
  when (direction == Reversed) do
    Internal.scrollFull h
  pure mdl { node = h }

recalculate :: Update
recalculate _ mdl@Model { node } = do
  l <- Internal.moreLeft node
  r <- Internal.moreRight node
  pure mdl
    { moreLeft  = l 
    , moreRight = r
    }

scrollLeft :: Update
scrollLeft rbn mdl@Model { node } = do
  Internal.scrollLeft node
  recalculate rbn mdl 

scrollRight :: Update
scrollRight rbn mdl@Model { node } = do
  Internal.scrollRight node
  recalculate rbn mdl 

type Render = Elm Msg => Model -> View

view :: Scroll -> Render
view Scroll { content } Model { node, moreLeft, moreRight } = 
  Div <| props |> [wrapped]
  where
    props = Themed @Scroll . OnWheelWith background update 
      where 
        update _ = command Recalculate

        background = 
          def 
            { asynchronous = True
            , passive      = True 
            }

    wrapped = Div <| props |> content l r
      where
        props =
            Host node (command . SetHost) 
          . OnScroll (\_ -> command Recalculate)

        l | moreLeft  = Just (command ScrollLeft)
          | otherwise = Nothing

        r | moreRight = Just (command ScrollRight)
          | otherwise = Nothing

instance Theme Scroll where
  theme c = 
    is c do
      overflow =: hidden
      position =: relative

      child (tag Div) do
        height =: (100%)
        overflow-y =: hidden
        overflow-x =: "scroll"
        webkit-overflow-scrolling =: touch

