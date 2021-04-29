{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Template where

import Data.Type.Equality
import qualified Language.Haskell.TH as TH

{-
\x y ->
  case x of
    {


    }
-}

heqTemplate :: TH.Name -> TH.Q TH.Exp
heqTemplate typeName = do
    r <- TH.reify typeName
    -- TH.runIO $ print r
    TH.DataD _ _ _ _ cons _ <-
      case r of
        TH.TyConI typeDecl@TH.DataD {} -> return typeDecl
        _ -> fail "heqTemplate: type name expected"
    nx <- TH.newName "x"
    ny <- TH.newName "y"
    yMatches <- mapM (heqTemplateConMatch $ TH.VarE ny) cons
    return $
        TH.LamE [TH.VarP nx, TH.VarP ny] $
            TH.CaseE (TH.VarE nx) yMatches

{-
      UnitaryCon ->
        { case y of
            { UnitaryCon -> Just Refl
            ; _ -> Nothing
            }
        }
-}

heqTemplateConMatch :: TH.Exp -> TH.Con -> TH.Q TH.Match
heqTemplateConMatch bsource (TH.GadtC [conName] [] _) = do
  return $
    TH.Match (TH.ConP conName [])
      ( TH.NormalB $ TH.CaseE bsource
          [ TH.Match (TH.ConP conName [])
            (TH.NormalB $ TH.AppE (TH.ConE 'Just) (TH.ConE 'Refl))
            []
          , TH.Match TH.WildP
            (TH.NormalB $ TH.ConE 'Nothing)
            []
          ]
      )
    []

{-
      FunCon a1 a2 a3 ->
        { case y of
            { FunCon b1 b2 b3
                | (a1 == b1) && (a2 == b2) && (a3 == b3)
                -> Just Refl
            ; _ -> Nothing
            }
        }
-}

heqTemplateConMatch bsource (TH.GadtC [conName] fields _) = do
    varsA <- mapM (const $ TH.newName "a") fields
    varsB <- mapM (const $ TH.newName "b") fields
    let abEquality =
          foldr1
            (\e1 e2 -> TH.InfixE (Just $ e1) (TH.VarE '(&&)) (Just $ e2))
            ( zipWith
                ( \va vb ->
                    TH.InfixE
                      (Just $ TH.VarE va) (TH.VarE '(==)) (Just $ TH.VarE vb)
                )
                varsA varsB
            )
    return $
      TH.Match (TH.ConP conName (map TH.VarP varsA))
        (TH.NormalB $ TH.CaseE bsource
          [ TH.Match (TH.ConP conName (map TH.VarP varsB))
            ( TH.GuardedB
                [ ( TH.NormalG abEquality
                  , TH.AppE (TH.ConE 'Just) (TH.ConE 'Refl)
                  )
                ]
            )
            []
          , TH.Match TH.WildP
            (TH.NormalB $ TH.ConE 'Nothing)
            []
          ]
        )
      []
heqTemplateConMatch bsource (TH.ForallC _ _ con) =
  heqTemplateConMatch bsource con
heqTemplateConMatch _ con =
  fail $ "unsupported constructor syntax: " ++ show con
