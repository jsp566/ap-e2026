module APL.AST
  ( VName,
    Exp (..),
    printExp,
  )
where

type VName = String

data Exp
  = CstInt Integer
  | CstBool Bool
  | Add Exp Exp
  | Sub Exp Exp
  | Mul Exp Exp
  | Div Exp Exp
  | Pow Exp Exp
  | Eql Exp Exp
  | If Exp Exp Exp
  | Var VName
  | Let VName Exp Exp
  -- TODO: add cases
  | ForLoop (VName, Exp) (VName, Exp) Exp
  | Lambda VName Exp
  | Apply Exp Exp
  | TryCatch Exp Exp
  deriving (Eq, Show)


maybeParenthesize :: Exp -> String
maybeParenthesize (CstInt x) = printExp (CstInt x)
maybeParenthesize (CstBool x) = printExp (CstBool x)
maybeParenthesize (Var x) = printExp (Var x)
maybeParenthesize x = "(" ++ printExp x ++ ")"


notParenthesizeApply :: Exp -> String
notParenthesizeApply (Apply exp1 exp2) = printExp (Apply exp1 exp2)
notParenthesizeApply x = maybeParenthesize x

join :: Exp -> String -> Exp -> String
join exp1 s exp2 = maybeParenthesize exp1 ++ s ++ maybeParenthesize exp2

printExp :: Exp -> String -- TODO
printExp (CstInt x) = show x
printExp (CstBool False) = "false"
printExp (CstBool True) = "true"
printExp (Add exp1 exp2) = join exp1 " + " exp2
printExp (Sub exp1 exp2) = join exp1 " - " exp2
printExp (Mul exp1 exp2) = join exp1 " * " exp2
printExp (Div exp1 exp2) = join exp1 " / " exp2
printExp (Pow exp1 exp2) = join exp1 " ** " exp2
printExp (Eql exp1 exp2) = join exp1 " == " exp2
printExp (If exp1 exp2 exp3) = "if " ++ maybeParenthesize exp1 ++ " then " ++ maybeParenthesize exp2 ++ " else " ++ maybeParenthesize exp3
printExp (Var v) = v
printExp (Let v exp1 exp2) = "let " ++ v ++ " = " ++ join exp1 " in " exp2
printExp (ForLoop (p, initial) (i, bound) body) = "loop " ++ p ++ " = " ++ maybeParenthesize initial ++ " for " ++ i ++ " < " ++ maybeParenthesize bound ++ " do " ++ maybeParenthesize body
printExp (Lambda v exp1) = "\\" ++ v ++ " -> " ++ maybeParenthesize exp1
printExp (Apply exp1 exp2) = notParenthesizeApply exp1 ++ " " ++ maybeParenthesize exp2
printExp (TryCatch exp1 exp2) = "try " ++ join exp1 " catch " exp2