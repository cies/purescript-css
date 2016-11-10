module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION, error, throwException)
import CSS (Rendered, Path(..), Predicate(..), Refinement(..), Selector(..), renderedSheet, renderedInline, fromString, selector, block, display, render, borderBox, boxSizing, contentBox, blue, color, body, px, dashed, border, inlineBlock, red, (?))
import Data.Maybe (Maybe(..))

import Control.Monad.Eff.Console (log, CONSOLE)
import Data.Generic
import Data.Array (last)
import Data.Maybe (fromMaybe)
import Data.String (split, toLower, drop)
import Data.String.Regex (regex, noFlags, replace, parseFlags, Regex)
import Data.Either (fromRight)
import Partial.Unsafe (unsafePartial)

example1 :: Rendered
example1 = render do
  color red
  display block

example2 :: Rendered
example2 = render do
  display inlineBlock

example3 :: Rendered
example3 = render do
  border dashed (px 2.0) blue

example4 :: Rendered
example4 = render do
  body ? do
    color blue
  fromString "#world" ? do
    display block

example5 :: Rendered
example5 = render do
  boxSizing contentBox
  boxSizing borderBox

nestedNodes :: Rendered
nestedNodes = render do
  fromString "#parent" ? do
    display block
    fromString "#child" ? display block

nestedNodesWithEmptyParent :: Rendered
nestedNodesWithEmptyParent = render do
  fromString "#parent" ? do
    fromString "#child" ? display block

assertEqual :: forall a. (Eq a, Show a) => a -> a -> Eff (err :: EXCEPTION) Unit
assertEqual x y = unless (x == y) <<< throwException <<< error $ "Assertion failed: " <> show x <> " /= " <> show y




data CssId = TheWrapper | TheContent
derive instance genericCssId :: Generic CssId

data CssClass = WrapperFTW | SomeContent
derive instance genericCssClass :: Generic CssClass

-- instance showCssClass :: Show CssClass where
--   show = gShow


toId :: CssId -> Predicate
toId = Id <<< spineCase <<< predicateName

toClass :: CssClass -> Predicate
toClass = Class <<< spineCase <<< predicateName

toClassSel :: CssClass -> Selector
toClassSel c = Selector (Refinement [toClass c]) Star

spineCase :: String -> String
spineCase = drop 1 <<< toLower <<< replace matchCaps "-$&"

predicateName :: forall a. (Generic a) => a -> String
predicateName = fromMaybe "undefined" <<< last <<< split "." <<< gShow

matchCaps :: Regex
matchCaps = unsafePartial $ fromRight $ regex "[A-Z]" (parseFlags "g")


example6 :: Rendered
example6 = render do
  toClassSel WrapperFTW ? do
    display block


main :: Eff (err :: EXCEPTION) Unit
main = do

  selector (Selector (Refinement [toClass WrapperFTW]) Star) `assertEqual` ".wrapper-f-t-w"
  renderedSheet example6 `assertEqual` Just ".wrapper-f-t-w { display: block }\n"

  renderedInline example1 `assertEqual` Just "color: hsl(0.0, 100.0%, 50.0%); display: block"
  renderedInline example2 `assertEqual` Just "display: inline-block"
  renderedInline example3 `assertEqual` Just "border: dashed 2.0px hsl(240.0, 100.0%, 50.0%) "

  selector (Selector (Refinement [Id "test"]) Star) `assertEqual` "#test"

  selector (fromString "#test") `assertEqual` "#test"

  renderedSheet example4 `assertEqual` Just "body { color: hsl(240.0, 100.0%, 50.0%) }\n#world { display: block }\n"

  renderedInline example5 `assertEqual` Just "box-sizing: content-box; box-sizing: border-box"

  renderedSheet nestedNodes `assertEqual` Just "#parent { display: block }\n#parent #child { display: block }\n"

  renderedSheet nestedNodesWithEmptyParent `assertEqual` Just "#parent #child { display: block }\n"
