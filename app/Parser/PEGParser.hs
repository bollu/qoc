module PEGParser where

import qualified Data.Map as M

data PEG a =
  PEGTerminal Char |
  PEGNonTerminal a |
  PEGSequence (PEG a) (PEG a) |
  PEGAlternative (PEG a) (PEG a) |
  PEGStar (PEG a) |
  PEGPlus (PEG a) |
  PEGOptional (PEG a)
--  PEGAnd (PEG a) |
--  PEGNot (PEG a)
  deriving (Eq)

-- Rule identifier is the name of the non-terminal
-- List of rules is *in order of declaration* (which affects how ambiguities
-- are resolved and must remain predictable)
newtype PEGGrammar = M.Map String [PEG String]

emptyPEGGrammar :: PEGGrammar
emptyPEGGrammar = M.empty

-- Context-free grammars

data CFGRule a =


first :: PEG a -> Maybe a
first (PEGTerminal _) = Nothing
first (PEGNonTerminal nt) = nt
first (PEGSequence

-- Left recursion elimination

elimLeftRecursion :: Grammar -> Grammar
elimLeftRecursion gr =
  foldl (\(nt, rules) -> addNonTerminal nt rules) emptyGrammar (M.toList gr)

addNonTerminal ::
     String       -- ^ Name of non-terminal
  -> [PEG String] -- ^ Original set of rules
  -> Grammar -> Grammar
addNonTerminal nt rules gr =
  let rules₁ = inlinePreviousNonTerminals rules gr
      rules₂ = removeDirectLeftRecursion rules₁ in
  M.insert nt rules₂ gr

inlinePreviousNonTerminals :: [PEG String] -> Grammar -> [PEG String]
inlinePreviousNonTerminals rules gr =
  let (new_rules, changed) = go rules in
  if changed then inlinePreviousNonTerminals new_rules gr else new_rules
  where
    go :: [PEG String] -> ([PEG String], Bool)
    go [] = ([], False)
    go (r:rs) =
      if isJust $ first r >>= flip M.lookup gr then
        (... {- inline it -} ++ (fst $ go rs), True)
      else
        (r : fst (go rs), snd (go rs))
