-- | Static math rendering via @texmath@ (LaTeX → MathML at build time).
module Heist.Extra.Splices.Pandoc.Texmath (
  renderMath,
) where

import Data.Text.Encoding qualified as TE
import Text.Pandoc.Builder qualified as B
import Text.TeXMath (DisplayType (..), readTeX, writeMathML)
import Text.XML.Light.Output (showElement)
import Text.XmlHtml qualified as X

renderMath :: B.MathType -> Text -> Either Text [X.Node]
renderMath mathType src = do
  exps <- readTeX src
  let mathml = toText . showElement $ writeMathML (displayType mathType) exps
  first toText . fmap X.docContent $
    X.parseXML "<texmath>" (TE.encodeUtf8 mathml)
  where
    displayType = \case
      B.InlineMath -> DisplayInline
      B.DisplayMath -> DisplayBlock
