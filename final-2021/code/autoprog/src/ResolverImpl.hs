-- Put your Resolver implementation in this file
module final-2021.code.autoprog.src.ResolverImpl where

import Defs

import Control.Monad.State -- could also easily hand-define

type TVEnv = TVName -> EM SType -- handy abbreviation

-- resolve a parser type in tc- and tv-env
resolve :: TCEnv -> TVEnv -> PType -> EM SType
resolve tce tve (PTVar tv) = tve tv --may fail
resolve tce tve (PTApp tc pts) =
    case lookup tc tce of
        Nothing -> Left $ "nuknown tycon: " ++ tc
        Just f -> do ts <- mapM (\pt -> resolve tce tve pt) pts; f ts

--  type TCEnv = [(TCName, [SType] -> EM SType)]
 
-- Declarations state is growing list of unique field/rcon names and tcenv
-- (can't use Writer monad, since also need to read while processing)
type  Decl a = StateT ([String], TCEnv) (Either ErrMsg) a

--report error in Decl monad
complain :: ErrMsg -> Decl a
complain s = lift (Left s)

-- process list of declarations , starting from tce0 and reserved field names
declare :: [TDecl] -> EM TCEnv
declare ds = 
  do ((), (_fs, tce)) <- runStateT (mapM_ doTDecl ds) (['fst', 'snd'], tce0)
     return tce

-- process single tycon declaration to extend declarations state
doTDecl :: TDecl -> Decl ()
doTDecl (TDSyn th pt) = addTycon th (\tce tve -> resolve tce tve pt)
doTDecl (TDRcd th rcn fpts) = 
  do mapM_ addVName (rcn : map fst fpts) -- rcon and field names
     addTycon th (\tce tve -> resolveRcd tce tve rcn fpts)

-- add record-constructor/projecttion name to global list (or fail)
addVName :: FName -> Decl()
addVName s = 
  do (ss, tce) <- get
     if s `elem` ss then complain $ "rcon/feild name already used: " ++ s
      else put (s:ss, tce)

addTycon :: TDHead -> (TCEnv -> TVenv -> EM SType) -> Decl ()
addTycon (tc, tvs) rrhs = 
  do (ss, tce) <- get
     let f :: [SType] -> EM SType
         f ts =  case (do tve <- mkTVEnv tvs ts; rrhs tce tve) of
                  Left e -> Left $ e ++ " in tycon " ++ tc
                                  -- refine errmsg with location info 
                  Right r -> Right t
      _ <- lift $ f (map STVar tvs) -- probe for any latent errors in f
      case lookup tc tce of
        Just _ -> complain $ "tycon name already used: " ++ tc
        Nothing -> put (ss, (tc, f) : tce)

-- match formal and actual type params of tycon to build tvenv for decl RHS
mkTVEnv :: [TVName] -> [SType] -> EM TVEnv
mkTVENv [] [] = 
  return $ \tv -> Left $ "bad tyvar " ++ tv -- initial tvenv, all unbound
mkTVEnv (tv:tvs) (t:ts) =
  do tve <- mkTVEnv tvs ts
     case tve tv of -- check for existing binding
      Left _ -> return $ \tv' -> if tv' == tv then return t else tve tv'
      Right _ -> Left $ "dup tyvar " ++ tv
mkTVEnv _ _ = Left $ "arity mismatch"

-- resolve all components of record-declaration RHS
resolveRcd :: TCEnv -> TVEnv -> RCName -> [(FName, PType)] -> EM SType
resolveRcd tce tve tcn fpts =
  do fts < mapM (\(fn, pt) -> do t <- resolve tce tve pt; return (fn, t)) fpts
     return $ STRcd rcn fts