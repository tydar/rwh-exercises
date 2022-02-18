import Text.ParserCombinators.Parsec

data StompFrame = StompFrame
  { command :: String
  , headers :: [[String]]
  , body   :: String
  } deriving (Show)

cmd = string "CONNECT" <|> string "STOMP" <|> string "SEND" <|> string "SUBSCRIBE"
      <|> string "UNSUBSCRIBE" <|> string "ACK" <|> string "NACK" <|> string "BEGIN"
      <|> string "ABORT" <|> string "COMMIT" <|> string "DISCONNECT"

headersParse = sepBy (try header <|> headerEnd) newline
  where headerEnd = (:[]) <$> string "\n"
header = sepBy1 (many1 $ noneOf ":\n") (char ':')

bodyParse = endBy (many $ noneOf "\NUL") (char '\NUL')

stompFrame = do 
  c <- cmd
  newline
  h <- headersParse
  b <- bodyParse
  return StompFrame{ command=c, headers=h, body=head b }

parseFrame :: String -> Either ParseError StompFrame
parseFrame input = parse stompFrame "(unknown)" input

-- problem: does not respect content-length
-- problem: headers not in the (String, String) format I want
-- will have to revisit after my lunchbreak
