{-# LANGUAGE FlexibleContexts #-}
-- probably means I'm doing something way out of line

import Text.ParserCombinators.Parsec

data StompFrame = StompFrame
  { command :: String
  , headers :: [(String, String)]
  , body   :: String
  } deriving (Show)

cmd = string "CONNECT" <|> string "STOMP" <|> string "SEND" <|> string "SUBSCRIBE"
      <|> string "UNSUBSCRIBE" <|> string "ACK" <|> string "NACK" <|> string "BEGIN"
      <|> string "ABORT" <|> string "COMMIT" <|> string "DISCONNECT"

{-
headersParse = sepBy (try header <|> headerEnd) newline
  where headerEnd = (:[]) <$> string "\n"
header = sepBy1 (many1 $ noneOf ":\n") (char ':')
-}

headersParse = manyTill header headerEnd
header = endBy (sepBy1 (many1 $ noneOf ":\n") (char ':')) newline
headerEnd = (:[]) <$> string "\n"

-- this is the function that requires FlexibleContexts
bodyParse contentLength = case contentLength of
                    Just n  -> count (n - 1) anyChar
                    Nothing -> many $ noneOf "\NUL"

stompFrame = do 
  c <- cmd
  newline
  h <- headersParse
  let headerTuples = toTuples $ head h -- I think we're always getting a [...headers]:[]
  let cL = contentLength headerTuples
  b <- bodyParse cL
  char '\NUL'
  return StompFrame{ command=c, headers=headerTuples, body=b }

contentLength :: [(String, String)] -> Maybe Int
contentLength xs = case filter clFilter xs of
              []  -> Nothing
              [x] -> Just $ read $ snd x
              ys  -> Just $ read $ snd $ last ys
  where clFilter = (==) "content-length" . fst

-- probably a better way to handle the error case
-- though it shouldn't ever go down that branch
toTuples :: [[String]] -> [(String, String)]
toTuples []     = []
toTuples (x:xs) = if length x == 2 then (head x, last x) : toTuples xs else error $ "bad parse: header: " ++ concat x

-- eta reduced
parseFrame :: String -> Either ParseError StompFrame
parseFrame = parse stompFrame "(unknown)"

-- problem: does not respect content-length FIXED
-- problem: headers not in the (String, String) format I want FIXED
-- will have to revisit after my lunchbreak
