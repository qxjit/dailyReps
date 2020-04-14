module Year2020.Month04.Day14 where

trivialMaybeDo :: Maybe Integer
trivialMaybeDo = do
  Just 10

trivialListDo :: [Integer]
trivialListDo = do
  [10, 11]

trivialPolymorphicDo :: Applicative m => m Integer
trivialPolymorphicDo = do
  pure 10

maybeDoWithBind :: Maybe Integer -> Maybe Integer
maybeDoWithBind maybeInt = do
  int <- maybeInt
  Just (int + 1)

listDoWithBind :: [Integer] -> [Integer]
listDoWithBind ints = do
  int <- ints
  [int + 1, int + 2]

polymorphicDoWithBind :: Monad m => m Integer -> m Integer
polymorphicDoWithBind mInt = do
  int <- mInt
  pure (int + 1)

allSumsAndProducts :: [Integer] -> [Integer] -> [Integer]
allSumsAndProducts as bs = do
  a <- as
  b <- bs
  [a + b, a * b]

monadicSum :: Monad m => m Integer -> m Integer -> m Integer
monadicSum mA mB = do
  a <- mA
  b <- mB
  pure (a + b)

monadicSumAndProduct :: Monad m => m Integer -> m Integer -> m (Integer, Integer)
monadicSumAndProduct mA mB = do
  a <- mA
  b <- mB

  let
    sumAB = a + b
    productAB = a * b

  pure (sumAB, productAB)

getUserInput :: IO String
getUserInput =
  getLine

lengthOfUserInput1 :: IO Int
lengthOfUserInput1 = do
  line <- getLine
  pure (length line)

lengthOfUserInput2 :: IO Int
lengthOfUserInput2 =
  fmap length getLine

printLineToScreen :: String -> IO ()
printLineToScreen =
  putStrLn

askUser1 :: String -> IO String
askUser1 prompt = do
  _ <- putStrLn prompt
  getLine

askUser2 :: String -> IO String
askUser2 prompt = do
  putStrLn prompt
  getLine

askUser3 :: String -> IO String
askUser3 prompt = do
  () <- putStrLn prompt
  getLine

