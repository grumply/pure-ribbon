{-# language NamedFieldPuns, TypeApplications, BlockArguments, OverloadedStrings, PostfixOperators #-}
module Pure.Ribbon (Scroll.Direction(..),Ribbon(..),ribbon,Scroll.Scroll,Arrow) where

import qualified Pure.Ribbon.Scroll as Scroll

import Pure.Elm as Pure hiding (Scroll,content,direction)

import Control.Monad
import Data.Coerce

import Prelude hiding (Left,Right)

data Ribbon = Ribbon
  { direction :: Scroll.Direction
  , content   :: View
  }

ribbon :: Ribbon -> View
ribbon Ribbon { direction, content } = 
  Scroll.scroll (Scroll.Scroll direction arrows)
  where
    arrows :: Maybe (IO ()) -> Maybe (IO ()) -> [View]
    arrows l r = [left, content, right]
      where
        left = maybe Null arrowLeft l
          where
            arrowLeft :: IO () -> View
            arrowLeft f = Div <| props |> [Div <||> ["❮"]]
              where 
                props = Themed @Arrow . Left 0 . OnClick (const f)

        right = maybe Null arrowRight r
          where
            arrowRight :: IO () -> View
            arrowRight f = Div <| props |> [Div <||> ["❯"]]
              where 
                props = Themed @Arrow . Right 0 . OnClick (const f)

data Arrow
instance Theme Arrow where
  theme c = 
    is c do
      top         =: 0
      user-select =: none
      display     =: flex
      height      =: (100%)
      margin      =: auto
      width       =: 45px
      cursor      =: pointer
      position    =: absolute

      firstChild do
        child (tag Div) do
          before do
            background-image =: linearGradient ["to left", hsba(0,0,0,0.45), hsba(0,0,0,0.75)]

      lastChild do
        child (tag Div) do
          before do
            background-image =: linearGradient ["to right", hsba(0,0,0,0.45), hsba(0,0,0,0.75)]
          
      child (tag Div) do
        user-select =: none
        z-index     =: 1
        text-align  =: center
        margin      =: auto
        font-size   =: 36px

        before do
          let content = "content"
          position =: absolute
          top =: 0
          left =: 0
          height =: (100%)
          width =: 45px
          display =: block
          content =: "''"
          opacity =: 0
          z-index =: (-100)
          transition =* [opacity,0.3s] 

        hover . before $ do
          opacity =: 0.65

