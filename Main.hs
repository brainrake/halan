import Data.Maybe (fromMaybe, listToMaybe)
import Data.Void (Void)
import Control.Monad (void)
import System.Environment (getArgs)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- Lambda Expression. Keep minimal to show how everything reduces to it.
data Exp
    = Lam { argumentName :: String, functionBody :: Exp }
    | App { function :: Exp, argument :: Exp }
    | Var { name :: String }
    deriving (Show)

-- Variable Bindings aka Dynamic Environment
type Env = [(String, Val)]

-- Evaluation result
data Val
    -- Function closure, carries lexical environment explicitly
    = Closure {envV :: Env, varNameV :: String, functionBodyV :: Exp}
    -- Application that cannot reduce further
    | StuckApp { functionV :: Val, argumentV :: Val }
    -- Free (unknown) variable, can not reduce further
    | StuckVar { nameV :: String }

-- Evaluator (lazy)
eval :: Env -> Exp -> Val
eval env (Lam argName body) = -- function abstraction
    Closure env argName body -- defer evaluation, just capture lexical scope
eval env (App funExp argExp) = -- apply function to argument
    case eval env funExp of -- evaluate function expression
        Closure closureEnv argName body -> -- funExp evals to closure, we can apply 
            eval ((argName, eval env argExp) : closureEnv) body -- eval body in extended env
        stuck ->  -- can not apply
            StuckApp stuck (eval env argExp) -- eval arg anyway to normalize
eval env (Var name) = -- look up variable in environment
    fromMaybe (StuckVar name) (lookup name env)

-- | Parser. Supports sugar: let bindings, optional parens, multi argument functions and calls, comments.
parser :: Parsec Void String Exp
parser = spaces >> expr <* spaces <* eof where
    spaces = L.space space1 (L.skipLineComment "#") empty
    expr = try def <|> try lam <|> app
    varName = (:) <$> letterChar <*> many (alphaNumChar <|> char '_')
    lam = do -- function definition
        n <- varName <* (spaces >> chunk ":" >> spaces)
        Lam n <$> expr
    app = do -- function call
        x <- atom
        xs <- many $ try $ lookAhead (space1 *> atomStart) *> space1 *> atom
        pure $ foldl App x xs
    atomStart = void letterChar <|> void (chunk "(") <|> void digitChar <|> void (chunk "[")
    atom = Var <$> varName <|> parens <|> nat <|> list 
    parens = chunk "(" >> spaces >> expr <* chunk ")"
    -- syntax aspartame
    def = do -- recursive let binding automatically applies Y combinator
        n <- varName <* (spaces >> chunk "=" >> spaces)
        val <- expr <* (chunk ";" >> spaces)
        body <- expr
        pure $ if n == "Y" then App (Lam n body) val else App (Lam n body) (App (Var "Y") (Lam n val))
    nat = church . read <$> some digitChar
    church n = iterate (App (Var "Succ")) (Var "Zero") !! n
    list = foldr (\e acc -> App (App (Var "Cons") e) acc) (Var "Nil") <$>
        between (chunk "[" >> spaces) (spaces >> chunk "]") (sepBy expr (try (spaces >> chunk "," >> spaces)))

-- pretty print values with sugar (bool, nat, list)
pretty :: Val -> String
pretty val = fromMaybe (show $ quote 0 val) (exp $ quote 0 val) where
    quote _ (StuckVar name) = Var name
    quote i (StuckApp fun arg) = App (quote i fun) (quote i arg)
    quote i (Closure env argName body) =
        Lam ("_" ++ show i) (quote (i + 1) (eval ((argName, StuckVar ("_" ++ show i)) : env) body))

    exp (Lam a (Lam b e))
      | Just n <- nat a b e = Just $ show n
      | Just xs <- listBody a b e = Just $ "[" ++ concat (zipWith (++) ("" : repeat ", ") xs) ++ "]"
      | Var x <- e = lookup x [(a, "True"), (b, "False")]
    exp (Lam n e) = (\v -> n ++ ": " ++ v) <$> exp e
    exp (App f x) = liftA2 (\f x -> "(" ++  f ++ ") (" ++ x ++ ")") (exp f) (exp x)
    exp (Var n) = pure n

    nat s z (Var x) | x == z = Just 0
    nat s z (App (Var x) e) | x == s = succ <$> nat s z e
    nat _ _ _ = Nothing

    listBody c n (Var x) | x == n = Just [] 
    listBody c n (App (App (Var x) y) ys) | x == c = do
        rest <- listTail ys
        pure $ fromMaybe (show y) (exp y) : rest 
      where
        listTail (Lam c' (Lam n' e)) = listBody c' n' e
        listTail e = listBody c n e
    listBody _ _ _ = Nothing

main :: IO ()
main = do
    preludeSrc <-  readFile "prelude.hal"
    programSrc <- readFile . fromMaybe "test.hal" . listToMaybe =<< getArgs
    exp <- either (fail . ("Parse error: " ++) . errorBundlePretty) pure $
        runParser parser "" (preludeSrc ++ "\n" ++ programSrc)
    let out = pretty $ eval [] exp
    putStrLn out
    writeFile "output" $ out
