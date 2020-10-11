import Data.List (union)

--------------------------------------------------------------------------------

-- 'TVar' as the type for variables
type TVar = String

-- 'Var' for variables
-- 'Abs' for λ-abstractions
-- 'App' for λ-applications
data Expr = Var TVar | Abs (TVar, Expr) | App (Expr, Expr) deriving (Show)

--------------------------------------------------------------------------------

-- FV(x) = {x}
-- FV(λx.e) = FV(e) - {x}
-- FV(e1 e2) = FV(e1) ∪ FV(e2)
getFreeVariables :: Expr -> [TVar]
getFreeVariables e =
  case e of
    Var x -> [x]
    Abs (x, e') -> filter (\y -> y /= x) (getFreeVariables e')
    App (e1, e2) -> union (getFreeVariables e1) (getFreeVariables e2)

-- getFreshVariable
getFreshVariable :: TVar -> Expr -> Expr -> TVar
getFreshVariable varName e e' =
  let
    getAllNames :: Expr -> [TVar]
    getAllNames (Var varName) = [varName]
    getAllNames (Abs (varName, e')) = union [varName] (getAllNames e')
    getAllNames (App (e1, e2)) = union (getAllNames e1) (getAllNames e2)

    isFreshVar :: TVar -> Bool
    isFreshVar [] = True
    isFreshVar (h:t) = (h == '\'') && isFreshVar t

    findLongestFreshVar :: TVar -> [TVar] -> TVar -> TVar
    findLongestFreshVar targetVarName [] longestFreshVar = longestFreshVar
    findLongestFreshVar targetVarName (h:t) longestFreshVar =
      let
        prefix  = take (length targetVarName) h
        postfix = drop (length targetVarName) h
      in
        if (prefix == targetVarName
            && isFreshVar postfix
            && length h > length longestFreshVar) then
          findLongestFreshVar targetVarName t h
        else
          findLongestFreshVar targetVarName t longestFreshVar
    
    allNames = union (getAllNames e) (getAllNames e')
  in
    (findLongestFreshVar varName allNames varName) ++ "'"

-- Substitution [e'/x]e
-- [e'/x]y       = e'                    if y == x
-- [e'/x]y       = y                     if y /= x
-- [e'/x](e1 e2) = [e'/x]e1 [e'/x]e2
-- [e'/x]λy.e''  = λy.e''                if y == x
-- [e'/x]λy.e''  = λy.[e'/x]e''          if y != x, y ∉ FV(e')
-- [e'/x]λy.e''  = λz.[e'/x][y↔z]e''     if y != x, y ∈ FV(e') where z != y, z ∉ FV(e), z != x, z ∉ FV(e')
--               = λz.[e'/x][z/y]e''     where z is a fresh variable for y
substitute :: (Expr, TVar) -> Expr -> Expr
substitute (e', x) e =
  case e of
    Var y ->
      if y == x then
        e'
      else
        Var y
    App (e1, e2) ->
      App (substitute (e', x) e1, substitute (e', x) e2)
    Abs (y, e'') ->
      if y == x then
        Abs (y, e'')
      else
        if not (elem y (getFreeVariables e')) then
          Abs (y, substitute (e', x) e'')
        else
          let
            z = getFreshVariable y e' e''
          in
            Abs (z, substitute (e', x) (substitute (Var z, y) e''))

-- Call-by-Name
stepCallByName :: Expr -> Expr
stepCallByName expr =
  case expr of
    App (Abs (x, e), e') -> substitute (e', x) e
    App (e1, e2) -> App (stepCallByName e1, e2)
    otherwise -> expr

-- Call-by-Value
stepCallByValue :: Expr -> Expr
stepCallByValue expr =
  case expr of
    App (Abs (x, e), App (e20, e21)) -> App (Abs (x, e), stepCallByValue (App (e20, e21)))
    App (Abs (x, e), v) -> substitute (v, x) e
    App (e1, e2) -> App (stepCallByName e1, e2)
    otherwise -> expr

--------------------------------------------------------------------------------

-- just in case someone is interested in how to implement [x↔y]e,
-- although it is not necessary for this assignment :)
swapVariables :: (TVar, TVar) -> Expr -> Expr
swapVariables (x, y) e =
  case e of
    Var v ->
      if v == x then
        Var y
      else if v == y then
        Var x
      else
        Var v
    Abs (v, e') ->
      if v == x then
        Abs (y, swapVariables (x, y) e')
      else if v == y then
        Abs (x, swapVariables (x, y) e')
      else
        Abs (v, swapVariables (x, y) e')
    App (e1, e2) ->
      App (swapVariables (x, y) e1, swapVariables (x, y) e2)

-- a reference implementation for checking the alpha-equivalence between two
-- expressions
isAlphaEquivalent :: Expr -> Expr -> Bool
isAlphaEquivalent e e' =
  case (e, e') of
    (Var x, Var y) ->
      x == y
    (App (e1, e2), App (e1', e2')) ->
      (isAlphaEquivalent e1 e1')
      && (isAlphaEquivalent e2 e2')
    (Abs (x, e1), Abs (y, e2)) ->
      if x == y then
        isAlphaEquivalent e1 e2
      else
        (not (elem y (getFreeVariables e1)))
        && (isAlphaEquivalent (swapVariables (x, y) e1) e2)
    otherwise ->
      False

--------------------------------------------------------------------------------

main = putStrLn "Load Complete!"