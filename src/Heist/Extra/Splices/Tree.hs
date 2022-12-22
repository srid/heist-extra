module Heist.Extra.Splices.Tree (treeSplice) where

import Data.Map.Syntax ((##))
import Data.Tree (Tree (..))
import Heist qualified as H
import Heist.Interpreted qualified as HI
import Heist.Splices qualified as Heist

treeSplice ::
  forall a sortKey.
  (Ord sortKey) =>
  -- | How to sort children
  (NonEmpty a -> [Tree a] -> sortKey) ->
  -- | Input tree
  [Tree a] ->
  -- | How to render a (sub-)tree root
  (NonEmpty a -> [Tree a] -> H.Splices (HI.Splice Identity)) ->
  HI.Splice Identity
treeSplice =
  go []
  where
    go :: [a] -> (NonEmpty a -> [Tree a] -> sortKey) -> [Tree a] -> (NonEmpty a -> [Tree a] -> H.Splices (HI.Splice Identity)) -> HI.Splice Identity
    go pars sortKey trees childSplice = do
      let extendPars x = maybe (one x) (<> one x) $ nonEmpty pars
      let sorter x = sortKey (extendPars $ rootLabel x) (subForest x)
      flip foldMapM (sortOn sorter trees) $ \(Node lbl children) -> do
        HI.runChildrenWith $ do
          let herePath = extendPars lbl
          childSplice herePath children
          "has-children" ## Heist.ifElseISplice (not . null $ children)
          let childrenSorter x = sortKey (herePath <> one (rootLabel x)) (subForest x)
          let childrenSorted = sortOn childrenSorter children
          "children"
            ## go (toList herePath) sortKey childrenSorted childSplice
