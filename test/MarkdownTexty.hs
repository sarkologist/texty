{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MarkdownTexty where

import           Texty

import           Control.Lens            hiding ( Context
                                                , noneOf
                                                )
import           Control.Lens.TH
import           Control.Monad.State           as S
import           Data.Text                     as T
import           Text.Parsec             hiding ( choice )

many1Until
  :: (Stream s m t, Show end)
  => ParsecT s u m a
  -> ParsecT s u m end
  -> ParsecT s u m [a]
many1Until p end = do
  notFollowedBy end
  first <- p
  rest  <- manyTill p end
  return (first : rest)

withinMany delim p = delim *> many1Until p (lookAhead delim) <* delim

newtype Italic = Italic { _unItalic :: Text } deriving (Eq, Show)
newtype Strikethrough = Strikethrough { _unStrikethrough :: Text } deriving (Eq, Show)
data Header = Header
  { _level   :: Int
  , _content :: Text
  }
  deriving (Eq, Show)
data Bullet = Bullet Text Int Text
  deriving (Eq, Show)
data HeaderTitleContent = HeaderTitleContent Int Text Text
  deriving (Eq, Show)
makePrisms ''HeaderTitleContent

i :: PPrism Text Italic
i = pPrism parse render
 where
  parse = Italic . pack <$> withinMany (char '*') (noneOf (['*']))
  render (Italic txt) = ("*" <> txt <> "*")

strikethrough :: PPrism Text Strikethrough
strikethrough = pPrism parse render
 where
  parse = Strikethrough . pack <$> withinMany (string "~~") (noneOf (['~']))
  render (Strikethrough txt) = ("~~" <> txt <> "~~")

h :: Int -> PPrism Text Header
h n = pPrism parse render
 where
  parse = Header n . pack <$> between (string (hashes n) *> char ' ')
                                      endOfLine
                                      (many1 (noneOf ['\n']))
  render (Header k txt) = pack (hashes k) <> " " <> txt <> "\n"

  hashes k = Prelude.take k (Prelude.repeat '#')

bullet :: PPrism Text Bullet
bullet = pPrism parse render
 where
  parse = uncurry Bullet <$> indentation <*> content

  render (Bullet style lvl txt) = T.replicate lvl style <> "- " <> txt <> "\n"

  content = pack <$> many1 (noneOf "\n") <* char '\n'

  indentation :: Parser (Text, Int)
  indentation =
    (styleLengthOf "  " <|> styleLengthOf "\t" <|> noIndent) <* string "- "

  styleLengthOf :: String -> Parser (Text, Int)
  styleLengthOf x =
    (\len -> (pack x, len)) . Prelude.length <$> many1 (string x)

  noIndent = ("", 0) <$ string ""

htc :: PPrism Text HeaderTitleContent
htc = pPrism parse render
 where
  parse =
    (HeaderTitleContent <$> (numHashes <* char ' ')) <*> title <*> content

  render (HeaderTitleContent lvl title content) =
    T.replicate lvl "#" <> " " <> title <> "\n" <> content

  numHashes = Prelude.length <$> many1 (char '#')
  title     = pack <$> many1 (noneOf ['\n']) <* char '\n'
  content =
    (pack .)
      .   (<>)
      <$> manyTill anyChar (() <$ try (lookAhead (string "\n#")) <|> eof)
      <*> (string "\n" <|> "" <$ eof)

unindentBulletIntoSubheader :: Text -> Text -> Text
unindentBulletIntoSubheader style =
  execState $ zoom (text . many' htc . _1 . _HeaderTitleContent) $ do
    (headerLevel, _, _) <- get
    zoom (_3 . text . many' (bullet <%> (h headerLevel)) . _1) $ do
      let f (Left (Bullet _ lvl content)) = if lvl == 0
            then Right (Header (headerLevel + 1) content)
            else Left (Bullet style (lvl - 1) content)
          f (Right x) = Right x
      modify f

headers :: PTraversal Text Header
headers = choice'
  [ ChoiceTraversal (h 1)
  , ChoiceTraversal (h 2)
  , ChoiceTraversal (h 3)
  , ChoiceTraversal (h 4)
  , ChoiceTraversal (h 5)
  , ChoiceTraversal (h 6)
  ]

skip :: [Char] -> PPrism Text Text
skip toSkip = pPrism parse id where parse = pack <$> many1 (noneOf toSkip)

allTheHeaders :: PTraversal Text (Either Header Text)
allTheHeaders = many' (headers <||> skip "#")

makeLenses ''Italic
makeLenses ''Header
makeLenses ''Strikethrough
