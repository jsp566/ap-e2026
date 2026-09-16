module APL.Check (checkExp, Error) where

import APL.AST (Exp (..), VName)

type Error = String

newtype CheckM a = CheckM (a -> Maybe Error) -- TODO - give this a proper definition.

check :: Exp -> CheckM ()
check e = undefined

checkExp :: Exp -> Maybe Error
checkExp e = Just ""
