{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE StrictData #-}

module Serialize where

import Control.Applicative
import Data.Int
import Data.Word
import qualified Data.Attoparsec.ByteString as Atp
import qualified Data.ByteString as Bs
import qualified Data.ByteString.Builder as Builder
import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Data.Void as Void
import qualified GHC.Generics as G
import qualified GHC.TypeLits as TypeLits

-- class Serializable a where
  -- toSerial :: a -> Builder.Builder
  -- parseSerial :: Atp.Parser a

instance Serializable Word64 where
  toSerial = Builder.word64LE
  parseSerial = do
    bs <- Atp.take 8
    pure $ Bs.foldr' (\b x -> fromIntegral b + x * 0x100) 0 bs

instance Serializable Int where
  toSerial = toSerial @Word64 . fromIntegral
  parseSerial = fromIntegral <$> parseSerial @Word64

instance Serializable Bs.ByteString where
  toSerial bs = toSerial (Bs.length bs) <> Builder.byteString bs
  parseSerial = do
    size <- parseSerial
    Atp.take size

instance Serializable Text.Text where
  toSerial = toSerial @Bs.ByteString . Encoding.encodeUtf8
  parseSerial = do
    bs <- parseSerial @Bs.ByteString
    case Encoding.decodeUtf8' bs of
      Right x -> pure x
      Left err -> fail $ show err

instance Serializable String where
  toSerial = toSerial @Text.Text . Text.pack
  parseSerial = Text.unpack <$> parseSerial @Text.Text

--

instance
    (Serializable a, Serializable b) =>
    Serializable (a, b)
  where
  toSerial (x, y) = toSerial @a x <> toSerial @b y
  parseSerial = do
    x <- parseSerial @a
    y <- parseSerial @b
    pure (x, y)

instance
    (Serializable a, Serializable b) =>
    Serializable (Either a b)
  where
  toSerial (Left x) = "\0" <> toSerial @a x
  toSerial (Right y) = "\1" <> toSerial @b y
  parseSerial = do
    tag <- Atp.anyWord8
    case tag of
      0 -> Left <$> parseSerial @a
      1 -> Right <$> parseSerial @b
      _ -> empty

instance Serializable () where
  toSerial () = ""
  parseSerial = pure ()

instance Serializable Void.Void where
  toSerial = Void.absurd
  parseSerial = empty

--

data PrefixTree a
  = Empty
  | Leaf a
  | Arm Text.Text (PrefixTree a)
  | Branch (PrefixTree a) (PrefixTree a)
  deriving stock (G.Generic)
{-
instance (Serializable a) => Serializable (PrefixTree a) where
  toSerial Empty = "\0"
  toSerial (Leaf x) = "\1" <> toSerial x
  toSerial (Arm prefix next) = "\2" <> toSerial prefix <> toSerial next
  toSerial (Branch left right) = "\3" <> toSerial left <> toSerial right
  parseSerial = do
    tag <- Atp.anyWord8
    case tag of
      0 -> pure Empty
      1 -> Leaf <$> parseSerial
      2 -> Arm <$> parseSerial <*> parseSerial
      3 -> Branch <$> parseSerial <*> parseSerial
      _ -> empty
-}
{-
newtype PrefixTreeSOP a = PrefixTreeSOP
  ( Either
      ()
      ( Either
          a
          ( Either
              (Text.Text, PrefixTreeSOP a)
              (PrefixTreeSOP a, PrefixTreeSOP a)
          )
      )
  )

from :: PrefixTree a -> PrefixTreeSOP a
from Empty =
  PrefixTreeSOP $ Left ()
from (Leaf x) =
  PrefixTreeSOP $ Right (Left x)
from (Arm prefix next) =
  PrefixTreeSOP $ Right (Right (Left (prefix, from next)))
from (Branch left right) =
  PrefixTreeSOP $ Right (Right (Right (from left, from right)))

to :: PrefixTreeSOP a -> PrefixTree a
to (PrefixTreeSOP (Left ())) =
  Empty
to (PrefixTreeSOP (Right (Left x))) =
  Leaf x
to (PrefixTreeSOP (Right (Right (Left (prefix, sopNext))))) =
  Arm prefix (to sopNext)
to (PrefixTreeSOP (Right (Right (Right (sopLeft, sopRight))))) =
  Branch (to sopLeft) (to sopRight)

deriving newtype instance
    Serializable a =>
    Serializable (PrefixTreeSOP a)

instance
    Serializable a =>
    Serializable (PrefixTree a)
  where
  toSerial = toSerial @(PrefixTreeSOP a) . from
  parseSerial = to <$> parseSerial @(PrefixTreeSOP a)
-}

type Rep a =
  G.D1 ('G.MetaData "PrefixTree" "Serialize" "main" 'False)
    ( (   G.C1 ('G.MetaCons "Empty" 'G.PrefixI 'False)
            G.U1
        G.:+:
          G.C1 ('G.MetaCons "Leaf" 'G.PrefixI 'False)
            ( G.S1 ('G.MetaSel 'Nothing 'G.NoSourceUnpackedness 'G.NoSourceStrictness 'G.DecidedLazy)
                (G.Rec0 a)
            )
      )
    G.:+:
      (   G.C1 ('G.MetaCons "Arm" 'G.PrefixI 'False)
            (   G.S1 ('G.MetaSel 'Nothing 'G.NoSourceUnpackedness 'G.NoSourceStrictness 'G.DecidedLazy)
                  (G.Rec0 Text.Text)
              G.:*:
                G.S1 ('G.MetaSel 'Nothing 'G.NoSourceUnpackedness 'G.NoSourceStrictness 'G.DecidedLazy)
                  (G.Rec0 (PrefixTree a))
            )
        G.:+:
          G.C1 ('G.MetaCons "Branch" 'G.PrefixI 'False)
            (   G.S1 ('G.MetaSel 'Nothing 'G.NoSourceUnpackedness 'G.NoSourceStrictness 'G.DecidedLazy)
                  (G.Rec0 (PrefixTree a))
              G.:*:
                G.S1 ('G.MetaSel 'Nothing 'G.NoSourceUnpackedness 'G.NoSourceStrictness 'G.DecidedLazy)
                  (G.Rec0 (PrefixTree a))
            )
      )
    )

type ReducedRep a =
    (   G.U1
      G.:+:
        G.Rec0 a
    )
  G.:+:
    (   (   G.Rec0 Text.Text
          G.:*:
            G.Rec0 (PrefixTree a)
        )
      G.:+:
        (   G.Rec0 (PrefixTree a)
          G.:*:
            G.Rec0 (PrefixTree a)
        )
    )

class GSerializable rep where
  gToSerial :: rep x -> Builder.Builder
  gParseSerial :: Atp.Parser (rep x)

instance
    (GSerializable rep) =>
    GSerializable (G.M1 k d rep)
  where
  gToSerial = gToSerial @rep . G.unM1
  gParseSerial = G.M1 <$> gParseSerial @rep

instance
    (GSerializable repa, GSerializable repb) =>
    GSerializable (repa G.:*: repb)
  where
  gToSerial (x G.:*: y) = gToSerial @repa x <> gToSerial @repb y
  gParseSerial = do
    x <- gParseSerial @repa
    y <- gParseSerial @repb
    pure (x G.:*: y)

instance
    (GSerializable repa, GSerializable repb) =>
    GSerializable (repa G.:+: repb)
  where
  gToSerial (G.L1 x) = "\0" <> gToSerial @repa x
  gToSerial (G.R1 y) = "\1" <> gToSerial @repb y
  gParseSerial = do
    tag <- Atp.anyWord8
    case tag of
      0 -> G.L1 <$> gParseSerial @repa
      1 -> G.R1 <$> gParseSerial @repb
      _ -> empty

instance GSerializable G.U1 where
  gToSerial G.U1 = ""
  gParseSerial = pure G.U1

instance GSerializable G.V1 where
  gToSerial _ = error "V1"
  gParseSerial = empty

instance (Serializable a) => GSerializable (G.Rec0 a) where
  gToSerial (G.K1 x) = toSerial @a x
  gParseSerial = G.K1 <$> parseSerial @a

-- instance Serializable a => Serializable (PrefixTree a) where
  -- toSerial tree = gToSerial @(G.Rep (PrefixTree a)) $ G.from tree
  -- parseSerial = G.to <$> gParseSerial @(G.Rep (PrefixTree a))

genericToSerial :: (G.Generic a, GSerializable (G.Rep a)) => a -> Builder.Builder
genericToSerial = gToSerial . G.from

genericParseSerial :: (G.Generic a, GSerializable (G.Rep a)) => Atp.Parser a
genericParseSerial = G.to <$> gParseSerial

class Serializable a where
  toSerial :: a -> Builder.Builder
  parseSerial :: Atp.Parser a

  default toSerial ::
    (G.Generic a, GSerializable (G.Rep a)) => a -> Builder.Builder
  toSerial = genericToSerial

  default parseSerial ::
    (G.Generic a, GSerializable (G.Rep a)) => Atp.Parser a
  parseSerial = genericParseSerial

instance (Serializable a) => Serializable (PrefixTree a)

type EitherRep a b =
  G.D1 ('G.MetaData "Either" "Data.Either" "base" 'False)
    (   G.C1 ('G.MetaCons "Left" 'G.PrefixI 'False)
          ( G.S1 ('G.MetaSel 'Nothing 'G.NoSourceUnpackedness 'G.NoSourceStrictness 'G.DecidedLazy)
              (G.Rec0 a)
          )
      G.:+:
        G.C1 ('G.MetaCons "Right" 'G.PrefixI 'False)
          ( G.S1 ('G.MetaSel 'Nothing 'G.NoSourceUnpackedness 'G.NoSourceStrictness 'G.DecidedLazy)
              (G.Rec0 b)
          )
    )

type PairRep a b =
  G.D1 ('G.MetaData "(,)" "GHC.Tuple" "ghc-prim" 'False)
    ( G.C1 ('G.MetaCons "(,)" 'G.PrefixI 'False)
        (   G.S1 ('G.MetaSel 'Nothing 'G.NoSourceUnpackedness 'G.NoSourceStrictness 'G.DecidedLazy)
              (G.Rec0 a)
          G.:*:
            G.S1 ('G.MetaSel 'Nothing 'G.NoSourceUnpackedness 'G.NoSourceStrictness 'G.DecidedLazy)
              (G.Rec0 b)
        )
    )

type ListRep a =
  G.D1 ('G.MetaData "[]" "GHC.Types" "ghc-prim" 'False)
    (   G.C1 ('G.MetaCons "[]" 'G.PrefixI 'False)
          G.U1
      G.:+:
        G.C1 ('G.MetaCons ":" ('G.InfixI 'G.LeftAssociative 9) 'False)
          (   G.S1 ('G.MetaSel 'Nothing 'G.NoSourceUnpackedness 'G.NoSourceStrictness 'G.DecidedLazy)
                (G.Rec0 a)
            G.:*:
              G.S1 ('G.MetaSel 'Nothing 'G.NoSourceUnpackedness 'G.NoSourceStrictness 'G.DecidedLazy)
                (G.Rec0 [a])
          )
    )

data Complex a = !a :+ !a

type ComplexRep a =
  G.D1 ('G.MetaData "Complex" "Data.Complex" "base" 'False)
    ( G.C1 ('G.MetaCons ":+" ('G.InfixI 'G.NotAssociative 6) 'False)
      (   G.S1 ('G.MetaSel 'Nothing 'G.NoSourceUnpackedness 'G.SourceStrict 'G.DecidedStrict)
            (G.Rec0 a)
        G.:*:
          G.S1 ('G.MetaSel 'Nothing 'G.NoSourceUnpackedness 'G.SourceStrict 'G.DecidedStrict)
            (G.Rec0 a)
      )
    )

--

infixr 6 :>
infixl 5 :|:
data PrefixTree2 a
  = Leaf2 { ptElement :: !a }
  | {-# UNPACK #-} !Text.Text    :>    {-# NOUNPACK #-} ~(PrefixTree2 a)
  | PrefixTree2 a :|: PrefixTree2 a
  deriving (G.Generic)

type PrefixTree2Rep a =
  G.D1 ('G.MetaData "PrefixTree2" "Serialize" "main" 'False)
    (  G.C1 ('G.MetaCons "Leaf2" 'G.PrefixI 'True)
          ( G.S1 ('G.MetaSel ('Just "ptElement") 'G.NoSourceUnpackedness 'G.SourceStrict 'G.DecidedStrict)
              (G.Rec0 a)
          )
      G.:+:
        (   G.C1 ('G.MetaCons ":>" ('G.InfixI 'G.RightAssociative 6) 'False)
              (   G.S1 ('G.MetaSel 'Nothing 'G.SourceUnpack 'G.SourceStrict 'G.DecidedStrict)
                    (G.Rec0 Text.Text)
                G.:*:
                  G.S1 ('G.MetaSel 'Nothing 'G.SourceNoUnpack 'G.SourceLazy 'G.DecidedLazy)
                    (G.Rec0 (PrefixTree2 a))
              )
          G.:+:
            G.C1 ('G.MetaCons ":|:" ('G.InfixI 'G.LeftAssociative 5) 'False)
              (   G.S1 ('G.MetaSel 'Nothing 'G.NoSourceUnpackedness 'G.NoSourceStrictness 'G.DecidedLazy)
                    (G.Rec0 (PrefixTree2 a))
                G.:*:
                  G.S1 ('G.MetaSel 'Nothing 'G.NoSourceUnpackedness 'G.NoSourceStrictness 'G.DecidedLazy)
                    (G.Rec0 (PrefixTree2 a))
              )
        )
    )

symbolVals :: forall s. TypeLits.KnownSymbol s => ShowS
symbolVals = showString $ TypeLits.symbolVal @s (error "Proxy")

natVal :: forall n. TypeLits.KnownNat n => Int
natVal = fromInteger $ TypeLits.natVal @n (error "Proxy")

class GShow rep where
  gShowsPrec :: Int -> rep x -> ShowS

instance
    (GShow repa, GShow repb) =>
    GShow (repa G.:+: repb)
  where
  gShowsPrec d (G.L1 x) = gShowsPrec d x
  gShowsPrec d (G.R1 y) = gShowsPrec d y

instance (GShow rep) => GShow (G.D1 d rep) where
  gShowsPrec d (G.M1 x) = gShowsPrec d x

instance
    (TypeLits.KnownSymbol conName, GShowFieldsRecord rep) =>
    GShow (G.C1 ('G.MetaCons conName conFixity 'True) rep)
  where
  gShowsPrec _ (G.M1 x) =
    symbolVals @conName .
    showString " {" .
    gShowsPrecFieldsRecord x .
    showString "}"

instance
    (TypeLits.KnownSymbol conName, GShowFieldsPrefix rep) =>
    GShow (G.C1 ('G.MetaCons conName 'G.PrefixI 'False) rep)
  where
  gShowsPrec d (G.M1 x) =
    showParen (d > 10) $
      conNameWrapped . gShowsPrecFieldsPrefix x
    where
      conNameStr = symbolVals @conName
      conNameWrapped = case conNameStr "" of
        ':' : _ -> showParen True $ conNameStr
        _ -> conNameStr

instance
    ( TypeLits.KnownSymbol conName
    , TypeLits.KnownNat fixLevel
    , GShowFieldsInfix rep
    ) =>
    GShow (G.C1 ('G.MetaCons conName ('G.InfixI fixAssoc fixLevel) 'False) rep)
  where
  gShowsPrec d (G.M1 x) =
    showParen (d > natVal @fixLevel) $
      gShowsPrecFieldsInfix conNameWrapped (natVal @fixLevel + 1) x
    where
      conNameStr = symbolVals @conName
      conNameWrapped = case conNameStr "" of
        ':' : _ -> showString " " . conNameStr . showString " "
        _ -> showString " `" . conNameStr . showString "` "

class GShowFieldsRecord rep where
  gShowsPrecFieldsRecord :: rep x -> ShowS

instance
    (GShowFieldsRecord repa, GShowFieldsRecord repb) =>
    GShowFieldsRecord (repa G.:*: repb)
  where
  gShowsPrecFieldsRecord (x G.:*: y) =
    gShowsPrecFieldsRecord x .
    showString ", " .
    gShowsPrecFieldsRecord y

instance
    (TypeLits.KnownSymbol selName, Show a) =>
    GShowFieldsRecord
      ( G.S1 ('G.MetaSel ('Just selName) selUnpack selStrict selDecided)
          (G.Rec0 a)
      )
  where
  gShowsPrecFieldsRecord (G.M1 (G.K1 x)) =
    symbolVals @selName . showString " = " . showsPrec 0 x

class GShowFieldsPrefix rep where
  gShowsPrecFieldsPrefix :: rep x -> ShowS

instance
    (GShowFieldsPrefix repa, GShowFieldsPrefix repb) =>
    GShowFieldsPrefix (repa G.:*: repb)
  where
  gShowsPrecFieldsPrefix (x G.:*: y) =
    gShowsPrecFieldsPrefix x . gShowsPrecFieldsPrefix y

instance
    (Show a) =>
    GShowFieldsPrefix (G.S1 s (G.Rec0 a))
  where
  gShowsPrecFieldsPrefix (G.M1 (G.K1 x)) =
    showString " " . showsPrec 11 x

instance GShowFieldsPrefix G.U1 where
  gShowsPrecFieldsPrefix G.U1 = id

class GShowFieldsInfix rep where
  gShowsPrecFieldsInfix :: ShowS -> Int -> rep x -> ShowS

instance
    (Show a, Show b) =>
    GShowFieldsInfix
      (   G.S1 sa (G.Rec0 a)
        G.:*:
          G.S1 sb (G.Rec0 b)
      )
  where
  gShowsPrecFieldsInfix conNameWrapped d (G.M1 (G.K1 x) G.:*: G.M1 (G.K1 y)) =
    showsPrec d x . conNameWrapped . showsPrec d y

genericShowsPrec :: (G.Generic a, GShow (G.Rep a)) => Int -> a -> ShowS
genericShowsPrec d x = gShowsPrec d (G.from x)

instance (Show a) => Show (PrefixTree2 a) where
  showsPrec = genericShowsPrec
