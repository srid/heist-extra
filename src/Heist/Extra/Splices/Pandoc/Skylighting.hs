{- | Syntax highlighting for code blocks using skylighting.

This module provides functionality to tokenize and render code with
syntax highlighting, producing XmlHtml nodes with appropriate class
attributes for CSS styling.
-}
module Heist.Extra.Splices.Pandoc.Skylighting (
  highlightCode,
) where

import Skylighting (
  TokenizerConfig (..),
  defaultSyntaxMap,
  lookupSyntax,
  tokenize,
 )
import Skylighting.Types (SourceLine, Token, TokenType (..))
import Text.XmlHtml qualified as X

{- | Highlight code using skylighting.
 Returns XmlHtml nodes with span elements containing class attributes
 for each token type. Returns Left with error message if tokenization fails.
 Returns Right with plain text if language is unknown.
-}
highlightCode ::
  -- | Language identifier (e.g., "haskell", "python")
  Maybe Text ->
  -- | Source code to highlight
  Text ->
  -- | Either error message or XmlHtml nodes with highlighting spans
  Either Text [X.Node]
highlightCode mLang code =
  case mLang >>= flip lookupSyntax defaultSyntaxMap of
    Nothing ->
      -- Unknown language or no language specified: plain text (not an error)
      Right [X.TextNode code]
    Just syntax ->
      case tokenize config syntax code of
        Left err ->
          Left $ "Skylighting tokenization error: " <> toText err
        Right sourceLines ->
          Right $ renderSourceLines sourceLines
  where
    config =
      TokenizerConfig
        { syntaxMap = defaultSyntaxMap
        , traceOutput = False
        }

-- | Render tokenized source lines to XmlHtml nodes
renderSourceLines :: [SourceLine] -> [X.Node]
renderSourceLines = concatMap renderLine
  where
    renderLine :: SourceLine -> [X.Node]
    renderLine tokens = map renderToken tokens <> [X.TextNode "\n"]

    renderToken :: Token -> X.Node
    renderToken (tokenType, txt) =
      case tokenTypeClass tokenType of
        Nothing ->
          -- No special styling for this token type
          X.TextNode txt
        Just cls ->
          X.Element "span" [("class", cls)] [X.TextNode txt]

{- | Map token types to CSS class names.
 These class names match Pandoc's skylighting HTML output.
-}
tokenTypeClass :: TokenType -> Maybe Text
tokenTypeClass = \case
  KeywordTok -> Just "kw"
  DataTypeTok -> Just "dt"
  DecValTok -> Just "dv"
  BaseNTok -> Just "bn"
  FloatTok -> Just "fl"
  ConstantTok -> Just "cn"
  CharTok -> Just "ch"
  SpecialCharTok -> Just "sc"
  StringTok -> Just "st"
  VerbatimStringTok -> Just "vs"
  SpecialStringTok -> Just "ss"
  ImportTok -> Just "im"
  CommentTok -> Just "co"
  DocumentationTok -> Just "do"
  AnnotationTok -> Just "an"
  CommentVarTok -> Just "cv"
  OtherTok -> Just "ot"
  FunctionTok -> Just "fu"
  VariableTok -> Just "va"
  ControlFlowTok -> Just "cf"
  OperatorTok -> Just "op"
  BuiltInTok -> Just "bu"
  ExtensionTok -> Just "ex"
  PreprocessorTok -> Just "pp"
  AttributeTok -> Just "at"
  RegionMarkerTok -> Just "re"
  InformationTok -> Just "in"
  WarningTok -> Just "wa"
  AlertTok -> Just "al"
  ErrorTok -> Just "er"
  NormalTok -> Nothing
