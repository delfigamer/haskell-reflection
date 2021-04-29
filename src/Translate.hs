{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Translate where

import Control.Applicative
import qualified Data.ByteString as Bs
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified GHC.Generics as G

newtype Plain a = Plain { unPlain :: a }
  deriving (Functor)

newtype Cypher a = Cypher { unCypher :: Bs.ByteString }
  deriving (Functor)

class Applicative f => ApEncrypt f where
  encrypt :: Plain Bs.ByteString -> f (Cypher Bs.ByteString)

encryptWith ::
  (ApEncrypt f) => (a -> Bs.ByteString) -> Plain a -> f (Cypher a)
encryptWith f = fmap (Cypher . unCypher)  . encrypt . fmap f

class Applicative f => Translate f a b | a f -> b where
  translate :: a -> f b

  default translate ::
    (G.Generic a, G.Generic b, GTranslate f (G.Rep a) (G.Rep b)) =>
    a -> f b
  translate = genericTranslate

instance
    (ApEncrypt f) =>
    Translate f (Plain Bs.ByteString) (Cypher Bs.ByteString)
  where
  translate = encrypt

instance
    (ApEncrypt f) =>
    Translate f (Plain Text.Text) (Cypher Text.Text)
  where
  translate = encryptWith Encoding.encodeUtf8

-- instance (Translate f a b) => Translate f (Maybe a) (Maybe b) where
  -- translate Nothing = pure Nothing
  -- translate (Just x) = Just <$> translate x

-- instance (Translate f a b) => Translate f [a] [b] where
  -- translate [] = pure []
  -- translate (x : xs) = liftA2 (:) (translate x) (translate xs)

data PersonT f
  = Person
    { name :: f Text.Text
    , surname :: f Text.Text
    , employeeId :: Int
    }
  deriving (G.Generic)

-- instance
    -- (ApEncrypt f) =>
    -- Translate f (PersonT Plain) (PersonT Cypher)
  -- where
  -- translate (Person n s i) =
    -- Person <$>
      -- translate n <*>
      -- translate s <*>
      -- pure i

-- data PersonT Plain
  -- = Person
    -- { name :: Plain Text.Text
    -- , surname :: Plain Text.Text
    -- , employeeId :: Int
    -- }

-- data PersonT Cypher
  -- = Person
    -- { name :: Cypher Text.Text
    -- , surname :: Cypher Text.Text
    -- , employeeId :: Int
    -- }

--

class (Applicative f) => GTranslate f ra rb where
  gTranslate :: ra x -> f (rb y)

instance
    (GTranslate f ra rb) =>
    GTranslate f (G.M1 ka ma ra) (G.M1 kb mb rb)
  where
  gTranslate (G.M1 x) = G.M1 <$> gTranslate x

instance
    (GTranslate f ra rb, GTranslate f rt ru) =>
    GTranslate f (ra G.:+: rt) (rb G.:+: ru)
  where
  gTranslate (G.L1 x) = G.L1 <$> gTranslate x
  gTranslate (G.R1 y) = G.R1 <$> gTranslate y

instance
    (GTranslate f ra rb, GTranslate f rt ru) =>
    GTranslate f (ra G.:*: rt) (rb G.:*: ru)
  where
  gTranslate (x G.:*: y) = liftA2 (G.:*:) (gTranslate x) (gTranslate y)

instance (Applicative f) => GTranslate f G.U1 G.U1 where
  gTranslate G.U1 = pure G.U1

instance (Applicative f) => GTranslate f G.V1 G.V1 where
  gTranslate _ = pure (error "V1")

instance
    (Translate f a b) =>
    GTranslate f (G.Rec0 a) (G.Rec0 b)
  where
  gTranslate (G.K1 x) = G.K1 <$> translate x

instance {-# OVERLAPPING #-}
    (Applicative f) =>
    GTranslate f (G.Rec0 a) (G.Rec0 a)
  where
  gTranslate (G.K1 x) = pure (G.K1 x)

genericTranslate ::
  (G.Generic a, G.Generic b, GTranslate f (G.Rep a) (G.Rep b)) =>
  a -> f b
genericTranslate = fmap G.to . gTranslate . G.from

instance (ApEncrypt f) => Translate f (PersonT Plain) (PersonT Cypher)

instance (Translate f a b) => Translate f (Maybe a) (Maybe b)

instance (Translate f a b) => Translate f [a] [b]

instance
    (Translate f a b, Translate f t u) =>
    Translate f (Either a t) (Either b u)

instance
    (Translate f a b, Translate f t u) =>
    Translate f (a, t) (b, u)
