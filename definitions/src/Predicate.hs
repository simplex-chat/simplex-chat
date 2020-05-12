module Predicate where

import ClassyPrelude
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
predicate decls = mconcat . map mkInstances <$> decls
  where
    mkInstances :: Dec -> [Dec]
    mkInstances d@(DataD _ tName _ _ constructors _) =
      d : mapMaybe (mkInstance tName) constructors
    mkInstances d = [d]

    mkInstance :: Name -> Con -> Maybe InstanceDec
    mkInstance tName (GadtC [cName] [] (AppT _ pType)) =
      let tyCon name = AppT (ConT (mkName name))
          ty =  AppT
                  (tyCon "Auto"
                    (tyCon "TyPred"
                      (ConT tName)))
                  pType
          ds =  [ValD
                  (VarP (mkName "auto"))
                  (NormalB (ConE cName)) []]
      in Just $ InstanceD Nothing [] ty ds
    mkInstance _ _ = Nothing
