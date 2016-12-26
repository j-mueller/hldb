{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Data.Accounting.Parser(parseJournal) where

import           Control.Applicative hiding (empty, optional)
import           Control.Lens hiding ((...), singular)
import           Control.Monad.Except
import           Control.Monad.State
import           Data.Accounting.Account (Accounts, account, balance)
import qualified Data.Accounting.Account as A
import           Data.Accounting.Currency (Currency(..), mapCurrencies, nonZero)
import           Data.Accounting.Journal (Journal, singleton)
import           Data.Accounting.Transaction (Transaction(..), accounts, addAccounts)
import           Data.AdditiveGroup
import qualified Data.Char as C
import           Data.Foldable (elem)
import qualified Data.Map.Strict as M
import           Data.Ratio
import           Data.Semigroup hiding (option)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time.Calendar (Day, fromGregorian)
import           Data.TreeMap (pathTo)
import           Text.Parsec hiding (State, (<|>), many)
import           Text.Parsec.Combinator
import           Text.Parsec.Text
import           Text.Read (readEither)


-- | Data that is needed during parsing
--
data ParsingState = ParsingState{
  _lastCurrencySymbol :: Text, -- ^ Last seen currency symbol. Used as a default value if no currency symbol is found.
  _defaultDay :: Day, -- ^ Default value for dates when parsing transactions.
  _currentTransaction :: Maybe Transaction, -- ^ The transaction that is currently being read,
  _runningTotal :: Currency Text, -- ^ A posting with no currencies, will be used for balancing when the current transaction is finished (ie. it will be given the negated sum of all other postings)
  _currentJournal :: Journal,
  _currentLine :: Int
}
  deriving (Eq, Ord, Show)

makeLenses ''ParsingState

parsingState :: ParsingState
parsingState = ParsingState cur dt Nothing mempty mempty 0 where
  cur = ""
  dt = fromGregorian 1970 1 1

-- | Finish the current transaction, if there is one, and add it to the journal
-- Set `currentTransaction` and `runningTotal` to `Nothing`. 
finishCurrentTransaction :: ParsingState -> ParsingState
finishCurrentTransaction ps = ps 
  & currentTransaction .~  Nothing 
  & runningTotal       .~  mempty
  & currentJournal     <>~ newJournal where
    newJournal       = maybe mempty singleton $ ps^.currentTransaction

addAccount :: Accounts -> ParsingState -> ParsingState
addAccount acc ps = ps
  & (currentTransaction . _Just) %~ (addAccounts acc)

getSourceName :: ParsingState -> String
getSourceName = (<>) "Line " . show

parseJournal :: Text -> Either Text Journal
parseJournal = result . flip runState parsingState . runExceptT . mapM_ parseLine . lines' where
  result (e, s) = fmap (const $ view currentJournal $ finishCurrentTransaction s) e

parseLine :: (Monad m, MonadState ParsingState m, MonadError Text m) => Text -> m ()
parseLine t = modify increaseLine >> parseLine' where
  increaseLine = over currentLine succ
  doParse p = do
    state <- get
    let theParser = p >> fmap stateUser getParserState
    let sourceName = getSourceName state
    let result = runParser theParser state sourceName t
    either (throwError . T.pack . show) put result
  parseLine' = case t of
    "" -> return ()
    "\t" -> return ()
    c | C.isDigit $ T.head c -> doParse txnFirstLine
    _ -> doParse accountP
  -- TODO:
  -- | Commodity
  -- | AutomatedTransaction
  -- | PeriodicTransaction
  -- | Command -- Commands not implemented...

txnFirstLine :: (Stream s m Char, Monad m) => ParsecT s ParsingState m ()
txnFirstLine = do
   d <- dateP <?> "date"
   _ <- spaces
   description <- fmap T.pack (manyTill anyChar eof) <?> "description"
   modifyState finishCurrentTransaction
   modifyState (set currentTransaction $ Just $ Transaction d description mempty)

-- | Parse a posting or a balancing posting
--
accountP :: (Monad m, Stream s m Char) => ParsecT s ParsingState m ()
accountP = do
  _ <- spaces <?> "space before account name"
  accName <- accountNameP <?> "account name"
  _       <- spaces <?> "space between account name and currency"
  curr    <- balancingCurrencyP <?> "currency"
  modifyState $ addAccount $ account $ pathTo accName curr

-- | Parse the name of an account in a hierarchy. The `:` symbol is used as a
-- separator. Example: `Expenses:Gifts` results in `["Expenses", "Gifts"]`.
--
accountNameP :: (Monad m, Stream s m Char) => ParsecT s u m [Text]
accountNameP = fmap (T.splitOn ":") p where
  p = T.pack <$> theChars <?> "account name"
  theChars = (:) <$> letter <*> rest
  rest = manyTill (alphaNum  <|> oneOf ":-&()" <|> space) end <?> "rest of account name"
  end = (try $ string "  ") <|> (try $ string "\t\t") <|> (fmap (const "") eof)

balancingCurrencyP :: (Monad m, Stream s m Char) => ParsecT s ParsingState m (Currency Text)
balancingCurrencyP = (getState & fmap (view runningTotal)) >>= \old -> do
  option (negateV old) currencyP

-- | Parse a `Currency` from `Text`, optionally followed by its equivalend in 
-- another currency (separated by '@@').
currencyP :: (Monad m, Stream s m Char) => ParsecT s ParsingState m (Currency Text)
currencyP = (try currencyWithExchangeRateP <?> "currencyWithExchangeRateP") <|> (defaultCurrencyP <?> "defaultCurrencyP")

-- | Two currencies separated by an '@@'.
currencyWithExchangeRateP :: (Monad m, Stream s m Char) => ParsecT s ParsingState m (Currency Text)
currencyWithExchangeRateP = do
  first <- currencyWithSymbolP
  _ <- many (satisfy C.isSpace) <?> "space"
  _ <- currencySeparator <?> "currency separator (@@)"
  _   <- many (satisfy C.isSpace) <?> "space"
  second <- currencyWithSymbolP
  modifyState (runningTotal <>~ second)
  return first

-- | Parse a `Currency` from `Text`. The return value has a single
-- currency-amount pair. If no currency amount is found, then the currency will
-- be `Nothing`.
-- >>> parseOnly singleCurrencyP "1 EUR"
-- Right [(Just "EUR",1 % 1)]
-- >>> parseOnly singleCurrencyP "0.5"
-- Right [(Nothing,1 % 2)]
-- >>> parseOnly singleCurrencyP "GBP 12.0"
-- Right [(Just "GBP",12 % 1)]
-- >>> parseOnly singleCurrencyP "GBP38.11"
-- Right [(Just "GBP",3811 % 100)]
singleCurrencyP :: (Monad m, Stream s m Char) => ParsecT s ParsingState m (Currency (Maybe Text))
singleCurrencyP = pr <?> "currencyP" where
  pr = (try $ fmap (mapCurrencies Just) currencyWithSymbolP) 
      <|> (noSymbolCurrencyP    <?> "noSymbolCurrencyP")
  currency' r = Currency . nonZero . flip M.singleton r
  noSymbolCurrencyP = do
    sgn <- signP
    r   <- fmap sgn $ rational
    return $ currency' r $ Nothing

-- | Parse a currency with a symbol (left or right of it)
currencyWithSymbolP :: (Monad m, Stream s m Char) => ParsecT s ParsingState m (Currency Text)
currencyWithSymbolP = pr <?> "currencyWithSymbolP" where
  pr = (try leftSymbolCurrencyP <?> "leftSymbolCurrencyP")
      <|> (rightSymbolCurrencyP  <?> "rightSymbolCurrencyP")
  currency' r = Currency . nonZero . flip M.singleton r
  leftSymbolCurrencyP  = do
    sgn <- signP <?> "sign"
    s   <- currencySymbol <?> "currency symbol"
    _  <- many (satisfy C.isSpace) <?> "space"
    sgn2 <- signP <?> "sign 2"
    amt <- fmap (sgn2 . sgn) $ (rational <?> "rational")
    return $ currency' amt s
  rightSymbolCurrencyP = do
    amt <- rational
    _   <- many (satisfy C.isSpace)
    s   <- currencySymbol
    return $ currency' amt s

-- | Currency separator ('@@')
currencySeparator :: (Monad m, Stream s m Char) => ParsecT s u m ()
currencySeparator = fmap (const ()) $ char '@' >> char '@' 

-- | Parse a `Currency`. If only a number but no symbol is found, the last known
-- symbol will be used.
--
-- >>> parseOnly defaultCurrencyP "-10.0"
-- Right [("",(-10) % 1)]
defaultCurrencyP :: (Monad m, Stream s m Char) => ParsecT s ParsingState m (Currency Text)
defaultCurrencyP = (getState & fmap (view lastCurrencySymbol)) >>= \c -> do
  let applyDefault = mapCurrencies (maybe c id)
  r <- fmap applyDefault singleCurrencyP
  modifyState (runningTotal <>~ r)
  return r

-- | Parse a currency symbol
currencySymbol :: (Monad m, Stream s m Char) => ParsecT s ParsingState m Text
currencySymbol = do
  let cond c = C.isPrint c && (not $ C.isSpace c) && (not $ C.isDigit c) && (not $ c `elem` ("+-" :: String))
  let p = many1 (satisfy cond) <?> "currency symbol"
  result <- fmap T.pack p
  modifyState (lastCurrencySymbol .~ result)
  return result

-- | Parse a sign (+/-) to a function,  `id` for optional '+' and `negate`
--   for '-'
signP :: (Monad m, Stream s m Char) => ParsecT s u m (Rational -> Rational)
signP = try m <|> p where
  m = char '-' >> return negate
  p = option id $ char '+' >> return id

-- | Parse a rational number
--
-- TODO: Move to Utils module?
rational :: (Stream s m Char, Monad m) => ParsecT s u m Rational
rational = do
  s <- signP
  before <- many digit
  _ <- optional $ char '.'
  after <- many digit
  case (before ++ after) of
    [] -> fail "rational"
    ds -> either fail (return . s . flip (%) (10^(length after))) (readEither ds :: Either String Integer)

-- | /O(n)/ Portably breaks a 'Text' up into a list of 'Text's at line
-- boundaries.
--
-- A line boundary is considered to be either a line feed, a carriage
-- return immediately followed by a line feed, or a carriage return.
-- This accounts for both Unix and Windows line ending conventions,
-- and for the old convention used on Mac OS 9 and earlier.
--
-- N.B. (j-mueller) Found this gem commented out in the source code of `text`
lines' :: Text -> [Text]
lines' ps | T.null ps   = []
          | otherwise = h : case T.uncons t of
                              Nothing -> []
                              Just (c,t')
                                  | c == '\n' -> lines' t'
                                  | c == '\r' -> case T.uncons t' of
                                                   Just ('\n',t'') -> lines' t''
                                                   _               -> lines' t'
    where (h,t)    = T.span notEOL ps
          notEOL c = c /= '\n' && c /= '\r'
{-# INLINE lines' #-}


-- | Parse a date
--
dateP :: (Stream s m Char, Monad m) => ParsecT s u m Day
dateP = do
  let toI p = p >>= either fail return . readEither
  year <- toI (count 4 digit) <?> "year"
  _ <- char '/' <?> "year/month separator"
  month <- toI (count 2 digit) <?> "month"
  _ <- char '/' <?> "month/day separator"
  day <- toI (count 2 digit) <?> "day"
  return $ fromGregorian year month day