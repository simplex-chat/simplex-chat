{-# LANGUAGE TemplateHaskell #-}

module Predicate where

import ClassyPrelude
import Data.Type.Predicate
import Data.Type.Predicate.Auto
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax

-- This template adds instances of Auto typeclass (from decidable package)
-- to a given parametrised type definition
--
-- Given type definitions:
--
-- data P = A | B | C
--
-- $(predicate [d|
--   data T (a :: P) where
--     TA :: T 'A
--     TB :: T 'B
--   |]
--
-- `predicate` splice will add these instances:
--
-- instance Auto (TyPred T) 'A where auto = TA  -- autoTC could have been used here too
-- instance Auto (TyPred T) 'B where auto = TB
--
-- to be used in type constraints

predicate :: Q [Dec] -> Q [Dec]
predicate decls = concat <$> (decls >>= mapM addInstances)
  where
    addInstances :: Dec -> Q [Dec]
    addInstances d@(DataD _ ty _ _ constructors _) = do
      ds <- mapM (mkInstance ty) constructors
      return $ d : concat ds
    addInstances d = return [d]

    mkInstance :: Name -> Con -> Q [Dec]
    mkInstance ty (GadtC [con] [] (AppT _ (PromotedT p))) =
      [d|
        instance Auto (TyPred $(conT ty)) $(promotedT p) where
          auto = $(conE con)
        |]
    mkInstance _ _ = return []
