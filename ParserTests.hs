import AST
import Parser
import Text.Parsec
import Text.Parsec.String
import Data.Char
import Data.Maybe

data Result = OK | Failed String

getFailure :: Result -> Maybe String
getFailure (Failed msg) = Just msg
getFailure _ = Nothing

whitespace = many (satisfy isSpace)

test :: (Show a, Eq a) => Parser a -> (String, Maybe a) -> Result
test parser (input, expected) =
    let parseResult = parse (whitespace *> parser <* eof) "" input
        fail msg = Failed $ show input ++ ": " ++ msg
    in case (expected, parseResult) of
        (Nothing, Left _) -> OK
        (Just x, Left err) -> fail $ "Unexpected failure: " ++ show err
        (Nothing, Right x) -> fail $ "Unexpected success: " ++ show x
        (Just x, Right y) | x == y -> OK
                          | otherwise -> fail $ "Wrong output, expected " ++ show x ++ ", got " ++ show y

runTests :: [Result] -> IO ()
runTests results =
  case mapMaybe getFailure results of
    [] -> return ()
    failures -> mapM_ (putStrLn . (++"\n")) failures >> putStrLn "FAIL!"

exprTests = map (test expr)
  [ ("", Nothing)
  , ("1", Just (LitNumber 1))
  , ("49384783294", Just (LitNumber 49384783294))
  , ("'", Nothing)
  , ("'bar'", Just (LitString "bar"))
  , ("'bar\\'baz'", Just (LitString "bar'baz"))
  , ("'\\Q'", Just (LitString "Q"))
  , ("'\\n'", Just (LitString "\n"))
  , ("foo", Just (Var "foo"))
  , ("foo.bar", Just (Col "foo" "bar"))
  , ("foo . bar", Just (Col "foo" "bar"))
  , ("2 + 5", Just (Op Add (LitNumber 2) (LitNumber 5)))
  , ("2 + 5 * 7", Just (Op Add (LitNumber 2) (Op Mul (LitNumber 5) (LitNumber 7))))
  , ("2+5*7", Just (Op Add (LitNumber 2) (Op Mul (LitNumber 5) (LitNumber 7))))
  , ("(2 + 5) * 7", Just (Op Mul (Op Add (LitNumber 2) (LitNumber 5)) (LitNumber 7)))
  ]

selectTests = map (test selectClause)
  [ ("", Nothing)
  , ("SELECT 1", Just $ Select [ExprTarget (LitNumber 1) Nothing] [] Nothing [])
  , ("SELECT foo FROM bar", Just $
      Select [ExprTarget (Var "foo") Nothing]
        [TableRef "bar" Nothing] Nothing [])
  , ("SELECT foo, bar FROM baz, thud xyzzy", Just $
      Select [ExprTarget (Var "foo") Nothing, ExprTarget (Var "bar") Nothing]
        [TableRef "baz" Nothing, TableRef "thud" (Just "xyzzy")] Nothing [])
  ]

otherTests = map (test tableRef)
  [ ("foo", Just (TableRef "foo" Nothing))
  , ("foo AS bar", Just (TableRef "foo" (Just "bar")))
  , ("foo bar", Just (TableRef "foo" (Just "bar")))
  ] ++ map (test sortClause)
  [ ("ORDER BY ", Nothing)
  , ("ORDER BY foo", Just [SortBy Asc (Var "foo")])
  , ("ORDER BY foo desc, bar", Just [SortBy Desc (Var "foo"), SortBy Asc (Var "bar")])
  ] ++ map (test identifier)
  [ ("ORDER", Nothing)
  , ("ORDERfoo", Just "ORDERfoo")
  , ("select", Nothing)
  , ("FROM", Nothing)
  , ("bar", Just "bar")
  ] ++ map (test fromClause)
  [ ("FROM foo", Just $ [TableRef "foo" Nothing])
  , ("FROM FROM", Nothing)
  ] ++ map (test $ sqlKeyword "FROM" >> identifier)
  [ ("FROM foo", Just "foo")
  , ("From bar", Just "bar")
  , ("FROMwat foo", Nothing)
  , ("wat man", Nothing)
  ]

main = mapM_ runTests [exprTests, selectTests, otherTests]
