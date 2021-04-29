{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices -ddump-to-file #-}

module Equality where

import Control.Monad
import Data.Type.Equality
import Template
import qualified Language.Haskell.TH as TH
import qualified GHC.Generics as G

data Request k v a where
  Get :: k -> Request k v (Maybe v)
  Set :: k -> Maybe v -> Request k v (Maybe v)
  Next :: Maybe k -> Request k v (Maybe (k, v))

-- deriving instance G.Generic (Request k v a)

deriving instance (Show k, Show v) => Show (Request k v a)

{-
instance
    (Eq k, Eq v) => TestEquality (Request k v)
  where
  testEquality (Get k1) (Get k2)
    | k1 == k2 = Just Refl
  testEquality (Set k1 mv1) (Set k2 mv2)
    | k1 == k2, mv1 == mv2 = Just Refl
  testEquality (Next mk1) (Next mk2)
    | mk1 == mk2 = Just Refl
  testEquality _ _ = Nothing
-}

class (Monad m) => MonadRequest k v m | m -> k v where
  handleRequest :: Request k v a -> m a

enumerate :: (MonadRequest k v m) => m [(k, v)]
enumerate = go [] Nothing
  where
    go buf mk = do
      mknext <- handleRequest $ Next mk
      case mknext of
        Just (k, v) -> go ((k, v) : buf) (Just k)
        Nothing -> pure buf

spec :: Either String ()
spec =
  testRequest
    [ RequestPlan (Next Nothing)    (Just ("x", 10))
    , RequestPlan (Next (Just "x")) (Just ("y", 20))
    , RequestPlan (Next (Just "y")) (Just ("z", 30))
    , RequestPlan (Next (Just "z"))  Nothing
    ]
    "[(\"z\",30),(\"y\",20),(\"x\",10)]"
    (show <$> enumerate)

data RequestPlan k v
  = forall t. RequestPlan (Request k v t) t

data Freer k v a
  = forall t. Request (Request k v t) (t -> Freer k v a)
  | Done a

instance Functor (Freer k v) where
  fmap = liftM

instance Applicative (Freer k v) where
  pure = Done
  (<*>) = ap

instance Monad (Freer k v) where
  Request req next >>= sel = Request req (next >=> sel)
  Done x >>= sel = sel x

instance MonadRequest k v (Freer k v) where
  handleRequest req = Request req Done

testRequest ::
  (Eq k, Eq v, Eq r, Show k, Show v, Show r) =>
  [RequestPlan k v] ->
  r ->
  Freer k v r ->
  Either String ()
testRequest [] final (Done x) =
  if final == x
    then Right ()
    else Left $
      "final result mismatch, expected " ++ show final ++
      ", given " ++ show x
testRequest (RequestPlan plannedReq _ : _) _ (Done _) =
  Left $
    "request mismatch, expected " ++ show plannedReq ++
    ", given none"
testRequest
    (RequestPlan plannedReq plannedResult : rest)
    final
    (Request req next) =
  case testEquality plannedReq req of
    Just Refl ->
      testRequest rest final (next plannedResult)
    Nothing -> Left $
      "request mismatch, expected " ++ show plannedReq ++
      ", given " ++ show req
testRequest [] _ (Request req _) =
  Left $ "request mismatch, expected none, given " ++ show req

[d| |]

instance
    (Eq k, Eq v) => TestEquality (Request k v)
  where
  testEquality = $(heqTemplate ''Request)

{-
TyConI $
  DataD [] Equality.Request [KindedTV k_1 StarT, KindedTV v_2 StarT, KindedTV a_3 StarT] Nothing
    [ ForallC [KindedTV k_4 StarT, KindedTV v_5 StarT] [] $
          GadtC [Equality.Get]
            [ (Bang NoSourceUnpackedness NoSourceStrictness, VarT k_4)
            ]
            ( ConT Equality.Request
              `AppT` VarT k_4
              `AppT` VarT v_5
              `AppT` (ConT GHC.Maybe.Maybe `AppT` VarT v_5)
            )
    , ForallC [KindedTV k_6 StarT, KindedTV v_7 StarT] [] $
          GadtC [Equality.Set]
            [ (Bang NoSourceUnpackedness NoSourceStrictness, VarT k_6)
            , (Bang NoSourceUnpackedness NoSourceStrictness, ConT GHC.Maybe.Maybe `AppT` VarT v_7)
            ]
            ( ConT Equality.Request
              `AppT` VarT k_6
              `AppT` VarT v_7
              `AppT` (ConT GHC.Maybe.Maybe `AppT` VarT v_7)
            )
    , ForallC [KindedTV k_8 StarT, KindedTV v_9 StarT] [] $
          GadtC [Equality.Next]
            [ (Bang NoSourceUnpackedness NoSourceStrictness, ConT GHC.Maybe.Maybe `AppT` VarT k_8)
            ]
            ( ConT Equality.Request
              `AppT` VarT k_8
              `AppT` VarT v_9
              `AppT` (ConT GHC.Maybe.Maybe `AppT` (TupleT 2 `AppT` VarT k_8 `AppT` VarT v_9))
            )
    ]
    []
-}