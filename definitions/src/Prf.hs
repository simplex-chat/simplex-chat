{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Data.Type.Prf
-- License     : BSD3
-- Copyright   : (c) Evgeny Poberezkin 2020
--
-- Maintainer  : evgeny@poberezkin.com
-- Stability   : experimental
-- Portability : non-portable
--
-- Template Haskell to generate Auto typeclass instances for
-- parametrized type.
module Prf
  ( Prf,
    autoPrf,
    proofs,
    getProofs,
  )
where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

-- import Text.Show.Pretty (ppShow)

class Prf a where
  autoPrf :: a

-- | Generate instances of 'Prf' typeclass for a given parametrised data type definition,
-- keeping the data type definition.
--
-- Given data type definitions:
--
-- > data P = A | B | C
-- >
-- > $(proofs [d|
-- >   data T (a :: P) where
-- >     TA :: T 'A
-- >     TB :: T 'B
-- >   |])
--
-- these instances will be added:
--
-- > instance Prf (T 'A) where autoPrf = TA
-- > instance Prf (T 'B) where autoPrf = TB
proofs :: Q [Dec] -> Q [Dec]
proofs decls = concat <$> (decls >>= mapM addInstances)
  where
    addInstances :: Dec -> Q [Dec]
    addInstances d = (d :) <$> mkInstances d

-- | Generate instances of 'Prf' typeclass for each of the provided type 'Name's
--
-- Instead of wrapping type declarations in 'proofs', you can pass type names to
-- this function:
--
-- > $(getProofs [''T])
getProofs :: [Name] -> Q [Dec]
getProofs names = concat <$> mapM mkTypeInstances names
  where
    mkTypeInstances :: Name -> Q [Dec]
    mkTypeInstances name =
      reify name >>= \case
        TyConI d -> mkInstances d
        _ -> do
          reportError $ "error: not a type constructor\n" ++ pprint name
          return []

mkInstances :: Dec -> Q [Dec]
mkInstances d@(DataD _ _ _ _ constructors _) = do
  ds <- mapM mkInstance constructors
  if any null ds
    then do
      reportWarning $
        "warning: not a parametrised GADT (no instances added)\n" ++ pprint d
      return []
    else return $ concat ds
mkInstances d = do
  reportWarning $ "warning: not a data type declaration\n" ++ pprint d
  return []

mkInstance :: Con -> Q [Dec]
mkInstance (ForallC _ ctx (GadtC [con] _ ty)) = returnQ [instanceTH ctx ty con]
mkInstance (GadtC [con] _ ty) = returnQ [instanceTH [] ty con]
mkInstance _ = return []

instanceTH :: Cxt -> Type -> Name -> Dec
instanceTH ctx ty con =
  InstanceD
    Nothing
    ctx
    (AppT (ConT (mkName "Prf")) ty)
    [ValD (VarP (mkName "autoPrf")) (NormalB (ConE con)) []]
-- introspect :: Name -> Q Exp
-- introspect n = reify n >>= runIO . putStrLn . ppShow >> [|return ()|]
