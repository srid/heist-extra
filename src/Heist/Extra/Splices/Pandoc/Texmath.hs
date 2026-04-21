{- | Static math rendering using texmath.

This module converts LaTeX math expressions to MathML at build time,
producing XmlHtml nodes that browsers render natively without JavaScript.
-}
module Heist.Extra.Splices.Pandoc.Texmath (
  renderMath,
) where

import Text.Pandoc.Builder qualified as B
import Text.TeXMath (DisplayType (..), readTeX, writeMathML)
import Text.XML.Light qualified as XL
import Text.XmlHtml qualified as X

{- | Render math using texmath.
 Returns MathML nodes for the given LaTeX source. Returns Left with
 error message if the LaTeX fails to parse.
-}
renderMath ::
  B.MathType ->
  -- | LaTeX math source
  Text ->
  Either Text [X.Node]
renderMath mathType src =
  case readTeX src of
    Left err -> Left err
    Right exps -> Right [fromElement $ writeMathML (displayType mathType) exps]
  where
    displayType = \case
      B.InlineMath -> DisplayInline
      B.DisplayMath -> DisplayBlock

fromElement :: XL.Element -> X.Node
fromElement (XL.Element name attrs content _) =
  X.Element
    (toText $ XL.qName name)
    (map fromAttr attrs)
    (concatMap fromContent content)

fromAttr :: XL.Attr -> (Text, Text)
fromAttr (XL.Attr k v) = (toText $ XL.qName k, toText v)

fromContent :: XL.Content -> [X.Node]
fromContent = \case
  XL.Elem e -> [fromElement e]
  XL.Text cd -> [X.TextNode (toText $ XL.cdData cd)]
  XL.CRef _ -> []
