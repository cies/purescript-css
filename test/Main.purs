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




-- old attempts

-- class Generic a <= CssString a where
--   toCssString :: a -> String

-- instance cssStringCssClass :: CssString CssClass where
--   toCssString = spineCase <<< predicateName

-- instance cssStringCssId :: CssString CssId where
--   toCssString = spineCase <<< predicateName

-- toId :: CssId -> Predicate
-- toId = Id <<< toCssString  -- spineCase <<< predicateName

-- toClass :: CssClass -> Predicate
-- toClass = Class <<< spineCase <<< predicateName

----------


-- The two data types containing the ids and classes respectively.
-- Instances of Generic so we can get their type as a String at runtime.
-- Instances od CssPredicate so we can get a CSS formatted Predicate,
-- which is a wrapped String.

class Generic a <= CssPredicate a where
  toPredicate :: a -> Predicate

data CssId = TheWrapper | TheContent
derive instance genericCssId :: Generic CssId
instance cssPredicateCssId :: CssPredicate CssId where
  toPredicate = Id <<< spineCase <<< typeAsString

data CssClass = ButtonSEO | Active
derive instance genericCssClass :: Generic CssClass
instance cssPredicateCssClass :: CssPredicate CssClass where
  toPredicate = Class <<< spineCase <<< typeAsString


-- | Turns "ButtonSEO" into "button-s-e-o".
spineCase :: String -> String
spineCase = drop 1 <<< toLower <<< replace matchCaps "-$&"
  where
    matchCaps :: Regex
    matchCaps = unsafePartial fromRight $ regex "[A-Z]" (parseFlags "g")

-- | Indicate error (instead of empty String), in case it fails.
typeAsString :: forall a. (Generic a) => a -> String
typeAsString = fromMaybe "PREDICATE-ERROR" <<< last <<< split "." <<< gShow


-- | Turn a CssClass or CssId into a Selector.
sel :: forall a. (CssPredicate a) => a -> Selector
sel s = Selector (Refinement [toPredicate s]) Star

idSel :: CssId -> Selector
idSel i = sel i

classSel :: CssClass -> Selector
classSel c = sel c


-- | This mimics the "#button-s-e-o" notation
example6 :: Rendered
example6 = render do
  classSel ButtonSEO ? do
    display block

-- | This mimics the ".the-wrapper" notation
example7 :: Rendered
example7 = render do
  idSel TheWrapper ? do
    display block




main :: Eff (err :: EXCEPTION) Unit
main = do

  selector (Selector (Refinement [toPredicate ButtonSEO]) Star) `assertEqual` ".button-s-e-o"
  selector (classSel ButtonSEO) `assertEqual` ".button-s-e-o"
  renderedSheet example6 `assertEqual` Just ".button-s-e-o { display: block }\n"

  selector (Selector (Refinement [toPredicate TheWrapper]) Star) `assertEqual` "#the-wrapper"
  selector (idSel TheWrapper) `assertEqual` "#the-wrapper"
  renderedSheet example7 `assertEqual` Just "#the-wrapper { display: block }\n"



  renderedInline example1 `assertEqual` Just "color: hsl(0.0, 100.0%, 50.0%); display: block"
  renderedInline example2 `assertEqual` Just "display: inline-block"
  renderedInline example3 `assertEqual` Just "border: dashed 2.0px hsl(240.0, 100.0%, 50.0%)"

  selector (Selector (Refinement [Id "test"]) Star) `assertEqual` "#test"

  selector (fromString "#test") `assertEqual` "#test"

  renderedSheet example4 `assertEqual` Just "body { color: hsl(240.0, 100.0%, 50.0%) }\n#world { display: block }\n"

  renderedInline example5 `assertEqual` Just "box-sizing: content-box; box-sizing: border-box"

  renderedSheet nestedNodes `assertEqual` Just "#parent { display: block }\n#parent #child { display: block }\n"

  renderedSheet nestedNodesWithEmptyParent `assertEqual` Just "#parent #child { display: block }\n"
