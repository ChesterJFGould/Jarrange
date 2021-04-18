{-
= J'arrange - Main
This document is currently the entire source code for the Jarrange language
so far and is written in a literate style which can be rendered into Github
compatible Markdown with the `lhsToMarkown.sh` script.
-}

module Main where
 
import Text.Megaparsec hiding (match)
import Text.Megaparsec.Char
import Control.Monad.State.Lazy
import Data.Void
import Data.List
import System.Environment

{-
We load in all the modules we'll be needing.
The main one to note is `Megaparsec`, it is the combinator parsing we'll be
using as it produces very nice error messages and is simple to use.
In the future it might be cool to write one ourselves, a very basic one is
extremely simple however the error messages seem to be the tricky part.
-}

iff p c a = if p then c else a

{-
`iff` is a simple helper function that makes some code a bit more concise.
-}

data JSON = Number Double
          | String String
          | Bool Bool
          | Array [JSON]
          | Obj [(String, JSON)]
          | Null
          deriving (Eq, Ord)

{-
This is the basic JSON data type we will be working with.
As you can see we have added a variable type so we can do pattern matching.
It might be good in the future to separate this data type into one that
contains variables and one that doesn't so that we can prove the output of the
program won't ever contain a variable.
-}


instance Show JSON where
         show (Number n) = show n
         show (String s) = "\"" ++ s ++ "\""
         show (Bool True) = "true"
         show (Bool False) = "false"
         show (Array a) = "[" ++ (unwords $ intersperse "," $ map show a) ++ "]"
         show (Obj mems) = "{" ++ (unwords $ intersperse "," $ map showMem mems) ++ "}"
                           where showMem (var, val) = unwords [var, ":", show val]
         show Null = "null"

{-
Now we just give some basic code that turns the JSON data type into valid JSON
text.
-}

data JSONTemplate = TNumber Double
                  | TString String
                  | TBool Bool
                  | TArray [JSONTemplate]
                  | TObj [(String, JSONTemplate)]
                  | TVar String
                  | TNull
                  deriving (Eq, Ord)

instance Show JSONTemplate where
         show (TNumber n) = show n
         show (TString s) = "\"" ++ s ++ "\""
         show (TBool True) = "true"
         show (TBool False) = "false"
         show (TArray a) = "[" ++ (unwords $ intersperse "," $ map show a) ++ "]"
         show (TObj mems) = "{" ++ (unwords $ intersperse "," $ map showMem mems) ++ "}"
                           where showMem (var, val) = unwords ["\"" ++ var ++ "\"", ":", show val]
         show (TVar v) = v
         show TNull = "null"

data Rule = To JSONTemplate JSONTemplate

instance Show Rule where
         show (To f t) = unwords [show f, "->", show t]

{-
`Rule` represents a rewrite rule with the first JSON value being the one we pattern
match on while the second is the template we build a concrete value from.
-}

parseJsons :: String -> String -> Either (ParseErrorBundle String Void) [JSON]
parseJsons fileName = parse jsons fileName

{-
`parseJsons` is the method that abstracts over the parser we will see in a bit.
It takes in a file name and input string then parses a list of JSON values from
the input string.
-}

parseRules :: String -> String -> Either (ParseErrorBundle String Void) [Rule]
parseRules fileName = parse rules fileName

{-
`parseRules` is the same as parseJsons except it parses a list of rules instead
of just JSON values.
-}

rules = rule `sepEndBy` space
rule = To <$> jsonTemplate
          <*> (space >> string "->" >> jsonTemplate)
          <?> "rule"
jsons = json `sepEndBy` space
json = space >> (((Obj <$> obj json)
                  <|> (Array <$> array json)
                  <|> (Number <$> num)
                  <|> (String <$> str)
                  <|> (Bool <$> bool)
                  <|> nul) <?> "JSON value")
jsonTemplate = space >> (((TObj <$> obj jsonTemplate)
                          <|> (TArray <$> array jsonTemplate)
                          <|> (TNumber <$> num)
                          <|> (TString <$> str)
                          <|> (TBool <$> bool)
                          <|> tnul
                          <|> var) <?> "JSON template")

obj :: Parsec Void String a -> Parsec Void String [(String, a)]
obj p = (char '{' *> ((member p <* space) `sepBy` (char ',' >> space)) <* char '}')
member :: Parsec Void String a -> Parsec Void String (String, a)
member p = (,) <$> (space >> (str <?> "member key"))
             <*> (space >> char ':' >> (p <?> "member value"))
             <?> "object member"
array :: Parsec Void String a -> Parsec Void String [a]
array p = (char '[' *> (p `sepBy` (space >> char ',' >> space)) <* char ']')
num :: Parsec Void String Double
num = (read . (foldl (++) "") . concat)
       <$> (sequence [(try $ (: []) <$> string "-") <|> pure [""]
                     , (: []) <$> some (digitChar :: Parsec Void String Char)
                     , (try $ sequence [string "."
                                       , some digitChar]) <|> pure [""]
                     , (try $ sequence [(: []) <$> oneOf "eE"
                                       , (try $ ((: []) <$> oneOf "+-")) <|> pure ""
                                       , some (digitChar :: Parsec Void String Char)] <|> pure [""])])
str = char '"' *> (concat
                   <$> many ((some $ noneOf "\"\\")
                             <|> (sequence [char '\\', anySingle])))
                  <* char '"'
bool = (string "true" >> pure True) <|> (string "false" >> pure False)
var = TVar <$> some alphaNumChar
nul = string "null" >> pure Null
tnul = string "null" >> pure TNull

{-
Now we get to the parser, it looks a bit scary but it essentially embodies the
following BNF.

```
rules = <rule> <rules>
      | <rule>

rule = <json> -> <json>

jsons = <json> <jsons>
      | <json>

json = <obj>
     | <array>
     | <val>

obj = {}
    | { <members> }

members = <member> , <members>
        | <member>

member = "<str>" : <json>

array = []
      | [ <elements> ]

elements = <json> , <elements>
         | <json>

val = <num>
    | <str>
    | <bool>
    | <var>
    | null

num = -?\d+(.\d+)?([+-]?[eE]\d+)?

str = "([^\\"]+|\\.)*"

bool = true
     | false

var = [a-z|A-Z|0-9]+
```
-}

{-
Now we get to the fun bit, pattern matching.
First we'll define some functions to help with that.
-}

vars :: JSONTemplate -> [String]
vars (TArray arr) = concat $ map vars arr
vars (TObj mems) = concat $ map (vars . snd) mems
vars (TVar v) = [v]
vars _ = []

{-
`vars` returns a list of all the variable names used in a JSON value.
-}

freeVars :: [String] -> JSONTemplate -> [String]
freeVars env obj = let ov = nub $ vars obj in foldl (flip delete) ov env

{-
`freeVars` returns all the variables in the given JSON value that are not bound
in the given environment, i.e. they are free.
-}

checkFreeVars :: Rule -> Either String Rule
checkFreeVars (To f t) = case freeVars (nub $ vars f) t of
                            [] -> Right (To f t)
                            fv -> Left $ unwords ["rule", show (To f t), "contains free variables", show fv]

{-
`checkFreeVars` checks a rule to make sure that all the variables used on the
right side are bound on the left side.
If the right side contains free variables then we return an error message.
-}

match :: JSONTemplate -> JSON -> StateT [(String, JSON)] Maybe ()
match (TNumber a) (Number b) = iff (a == b) (return ()) (lift Nothing)
match (TString a) (String b) = iff (a == b) (return ()) (lift Nothing)
match (TBool a) (Bool b) = iff (a == b) (return ()) (lift Nothing)
match (TArray a) (Array b) = iff (length a == length b)
                                ((sequence $ map (uncurry match) (zip a b)) >> return ())
                                (lift Nothing)
match (TObj a) (Obj b) = let ((aVars, aVals), (bVars, bVals)) = (unzip $ sort a, unzip $ sort b)
                        in iff (aVars == bVars)
                               ((sequence $ map (uncurry match) (zip aVals bVals)) >> return ())
                               (lift Nothing)
match (TVar v) a = gets (lookup v) >>= maybe (modify $ ((v, a) :))
                                            (\b -> iff (a == b) (return ()) (lift Nothing))
match TNull Null = return ()
match _ _ = lift Nothing

{-
Here it is, the function you've been waiting for.
`match` is suprisingly simple, it takes in the rule then the value to match on.
If the values are of the same type and atomic then we just check them for
equality.
If they are complex (an object or array) then we recursively match on their
elements.
Finally if the rule is a variable then we look it up in the environment
(we'll get to that in a second), if it is bound we then check if it is equal to
the value, otherwise we bind the variable to the value.
The one tricky part of this function is that it takes place in the State monad.
If you don't know what that is or just want to watch a really great explanation
check out [this](https://www.youtube.com/watch?v=XxzzJiXHOJs) video.
The state in this case is the environment we are building up.
This is in fact the best way to think of this functon, we traverse the rule and
value in sync and build up an environment of all the variables contained in the
rule which will then be used in the next function.
You might also notice that we use the State monad transformer with the Maybe
monad instead of the vanilla State monad.
This is just so that if the value doesn't in fact match the rule we can just
signal failure and exit the function.
-}
                        
reify :: [(String, JSON)] -> JSONTemplate -> Maybe JSON
reify env (TArray a) = (sequence $ map (reify env) a) >>= (Just . Array)
reify env (TObj mems) = let (vars, vals) = unzip mems in (sequence $ map (reify env) vals) >>= (Just . Obj . zip vars)
reify env (TVar v) = lookup v env
reify env (TNumber n) = Just $ Number n
reify env (TString s) = Just $ String s
reify env (TBool b) = Just $ Bool b
reify env (TNull) = Just $ Null

{-
`reify` is much simpler than the `match` function.
We just traverse the given JSON value (which is the right side of a rule) and
replace all the variables it contains with the value in the given environment.
This function returns a maybe value because technically we might encounter a
variable that is not bound, however this shouldn't ever happen.
-}

rearrange :: [Rule] -> JSON -> JSON
rearrange [] obj = obj
rearrange (To f t : rest) obj = maybe (rearrange rest obj)
                                 id
                                 (execStateT (match f obj) [] >>= (flip reify $ t))

{-
`rearrange` simply takes in a list of rules and applied them in sequence to the
given JSON value.
If none of them match then we just return the value.
-}

getRules :: IO (Either String [Rule])
getRules = do
        args <- getArgs
        case args of
             (file : _) -> readFile file
                            >>= (return . parseRules file)
                            >>= (return . either (Left . errorBundlePretty)
                                                 (sequence . map checkFreeVars))
             [] -> return $ Left "Please pass the file containing the rules"

{-
`getRules` reads in the name of the file containing the rules as the first
program argument.
It then parses the rules from this file or returns an error if the file doesn't
exist.
-}

main :: IO [()]
main = do
       rules <- getRules
       either (sequence . (: []) . putStrLn)
              (\rules -> getContents >>= (sequence
                                         . either ((: []) . putStrLn . errorBundlePretty)
                                                  (map (putStrLn . show . rearrange rules))
                                         . parseJsons "stdin"))
              rules

{-
And finally the `main` function.
We first read in the rules and then apply them to all the JSON values we parse
from stdin, or print out a parse error if we encouter one.
-}
