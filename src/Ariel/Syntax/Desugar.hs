{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Ariel.Syntax.Desugar where

import Ariel.Common.Types
import qualified Ariel.Core.Types as Core
import Ariel.Syntax.Types
import Data.List (foldl')
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Language.SexpGrammar (Position)

desugarDefs :: Defs -> Core.Defs
desugarDefs _ = mempty

desugarDecl :: Map Text (Set Name) -> Decl -> Core.Decl
desugarDecl ns (Decl _ (Name name) e) = Core.Decl name (desugarExpr ns e)
desugarDecl ns (DeclLam p (Name name :| args) e) = Core.Decl name (desugarExpr ns (Lam p args e))

desugarExpr :: Map Text (Set Name) -> Expr -> Core.Expr
desugarExpr _ (Int i) = Core.Int i
desugarExpr _ (String s) = Core.String s
desugarExpr _ F = Core.Bool False
desugarExpr _ T = Core.Bool True
desugarExpr _ (Con _) = error "Unimplemented"
desugarExpr ns (Lam p args e) = foldr (Core.Lam p) (desugarExpr ns e) args
desugarExpr ns (Let p decls e) =
  foldr
    (\(LetDecl name e1) -> Core.Let p name (desugarExpr ns e1))
    (desugarExpr ns e)
    decls
desugarExpr ns (Prim p name args) = Core.Prim p name (map (desugarExpr ns) args)
desugarExpr ns (IOPrim p name args) = Core.IOPrim p name (map (desugarExpr ns) args)
desugarExpr _ (Case _ _) = error "Unimplemented"
desugarExpr ns (If p c t f) = Core.If p (desugarExpr ns c) (desugarExpr ns t) (desugarExpr ns f)
desugarExpr ns (NamedLam p name args e) = Core.Fix p $ desugarExpr ns (Lam p (name : args) e)
desugarExpr ns (BindIO p e1 e2) = Core.BindIO p (desugarExpr ns e1) (desugarExpr ns e2)
desugarExpr ns (Ann p e t) = Core.Ann p (desugarExpr ns e) (desugarTy t)
desugarExpr ns (Fix p e) = Core.Fix p (desugarExpr ns e)
desugarExpr ns (Var p name) = resolveName p name ns
desugarExpr ns (App p (e :| es)) = foldl' (Core.App p) (desugarExpr ns e) (desugarExpr ns <$> es)

desugarTy :: Ty -> Core.Ty
desugarTy = desugarTy' []

desugarTy' :: [Name] -> Ty -> Core.Ty
desugarTy' vars (TSym name) =
  if name `elem` vars
    then Core.TVar (TyVar name)
    else Core.TCon name
desugarTy' vars (TApp (t :| ts)) = foldl' Core.TApp (desugarTy' vars t) (fmap (desugarTy' vars) ts)
desugarTy' vars (Forall vs t) = Core.Forall vs (desugarTy' (map unTyVar vs ++ vars) t)
desugarTy' vars (TArr ts) = desugarTy' vars $ TApp $ TSym "->" :| ts

resolveName :: Position -> Name -> Map Text (Set Name) -> Core.Expr
resolveName p name ns = case matches of
  [] -> Core.Var p name
  [moduleName] -> Core.Global (QName moduleName name)
  _ -> error ("Ambiguous name: " ++ show name)
  where
    matches = mapMaybe findModuleName (Map.toList ns)
    findModuleName (moduleName, names) =
      if Set.member name names
        then Just moduleName
        else Nothing
