= Header!
+ list 1
+ list 2
+ list 3

> module Main(main) where
> 
> import Text.Megaparsec hiding (match)
> import Text.Megaparsec.Char
> import Control.Monad.State.Lazy
> import Data.Void
> import Data.List
> import System.Environment
> 
> iff p c a = if p then c else a
> 
> data JSON = Number Double
>           | String String
>           | Bool Bool
>           | Array [JSON]
>           | Obj [(String, JSON)]
>           | Var String
>           | Null
>           deriving (Eq, Ord)
> 
> instance Show JSON where
>          show (Number n) = show n
>          show (String s) = "\"" ++ s ++ "\""
>          show (Bool True) = "true"
>          show (Bool False) = "false"
>          show (Array a) = "[" ++ (unwords $ intersperse "," $ map show a) ++ "]"
>          show (Obj mems) = "{" ++ (unwords $ intersperse "," $ map showMem mems) ++ "}"
>                            where showMem (var, val) = unwords [var, ":", show val]
>          show (Var s) = s
>          show Null = "null"
> 
> data Rule = To JSON JSON
>           deriving Show
> 
> -- Parses a list of JSON data
> 
> parseJsons :: String -> Either (ParseErrorBundle String Void) [JSON]
> parseJsons = parse jsons ""
> 
> parseRules :: String -> String -> Either (ParseErrorBundle String Void) [Rule]
> parseRules fileName = parse rules fileName
> 
> rules = rule `sepEndBy` space
> rule = To <$> json
>           <*> (space >> string "->" >> json)
>           <?> "rule"
> jsons = json `sepEndBy` space
> json = space >> ((obj <|> array <|> val) <?> "JSON value")
> val = num <|> str <|> bool <|> nul <|> var
> num = (Number . read . (foldl (++) "") . concat)
>        <$> (sequence [(try $ (: []) <$> string "-") <|> pure [""]
>                      , (: []) <$> some (digitChar :: Parsec Void String Char)
>                      , (try $ sequence [string "."
>                                        , some digitChar]) <|> pure [""]
>                      , (try $ sequence [(: []) <$> oneOf "eE"
>                                        , (try $ ((: []) <$> oneOf "+-")) <|> pure ""
>                                        , some (digitChar :: Parsec Void String Char)] <|> pure [""])])
> str = String <$> (char '"' *> str' <* char '"')
> str' = concat <$> many ((some $ noneOf "\"\\") <|> (sequence [char '\\', anySingle]))
> nul = string "null" >> pure Null
> bool = (Bool) <$> ((string "true" >> pure True) <|> (string "false" >> pure False))
> array = Array <$> (char '[' *> (json `sepBy` (space >> char ',' >> space)) <* char ']')
> obj = Obj <$> (char '{' *> ((member <* space) `sepBy` (char ',' >> space)) <* char '}')
> member = (,) <$> (space >> ((char '"' *> str' <* char '"') <?> "member key"))
>              <*> (space >> char ':' >> (json <?> "member value"))
>              <?> "object member"
> var = Var <$> some alphaNumChar
> 
> vars :: JSON -> [String]
> vars (Array arr) = concat $ map vars arr
> vars (Obj mems) = concat $ map (vars . snd) mems
> vars (Var v) = [v]
> vars _ = []
> 
> freeVars :: [String] -> JSON -> [String]
> freeVars env obj = let ov = nub $ vars obj in foldl (flip delete) ov env
> 
> checkFreeVars :: Rule -> Either String Rule
> checkFreeVars (To f t) = case freeVars (nub $ vars f) t of
>                             [] -> Right (To f t)
>                             fv -> Left $ unwords ["rule", show (To f t), "contains free variables", show fv]
> 
> match :: JSON -> JSON -> StateT [(String, JSON)] Maybe ()
> match (Number a) (Number b) = iff (a == b) (return ()) (lift Nothing)
> match (String a) (String b) = iff (a == b) (return ()) (lift Nothing)
> match (Bool a) (Bool b) = iff (a == b) (return ()) (lift Nothing)
> match (Array a) (Array b) = iff (length a == length b)
>                                 ((sequence $ map (uncurry match) (zip a b)) >> return ())
>                                 (lift Nothing)
> 
> match (Obj a) (Obj b) = let ((aVars, aVals), (bVars, bVals)) = (unzip $ sort a, unzip $ sort b)
>                         in iff (aVars == bVars)
>                                ((sequence $ map (uncurry match) (zip aVals bVals)) >> return ())
>                                (lift Nothing)
> match (Var v) a = gets (lookup v) >>= maybe (modify $ ((v, a) :))
>                                             (\b -> iff (a == b) (return ()) (lift Nothing))
> match Null Null = return ()
> match _ _ = lift Nothing
>                         
> reify :: [(String, JSON)] -> JSON -> Maybe JSON
> reify env (Array a) = (sequence $ map (reify env) a) >>= (Just . Array)
> reify env (Obj mems) = let (vars, vals) = unzip mems in (sequence $ map (reify env) vals) >>= (Just . Obj . zip vars)
> reify env (Var v) = lookup v env
> reify env obj = Just obj
> 
> rearrange :: [Rule] -> JSON -> JSON
> rearrange [] obj = obj
> rearrange (To f t : rest) obj = maybe (rearrange rest obj)
>                                  id
>                                  (execStateT (match f obj) [] >>= (flip reify $ t))
> 
> getRules :: IO (Either String [Rule])
> getRules = do
>         args <- getArgs
>         case args of
>              (file : _) -> readFile file
>                             >>= (return . parseRules file)
>                             >>= (return . either (Left . errorBundlePretty)
>                                                  (sequence . map checkFreeVars))
>              [] -> return $ Left "Please pass the file containing the rules"
> 
> -- |This is a comment
> main :: IO [()]
> main = do
>        rules <- getRules
>        either (sequence . (: []) . putStrLn)
>               (\rules -> getContents >>= (sequence
>                                          . either ((: []) . putStrLn . errorBundlePretty)
>                                                   (map (putStrLn . show . rearrange rules))
>                                          . parseJsons))
>               rules
