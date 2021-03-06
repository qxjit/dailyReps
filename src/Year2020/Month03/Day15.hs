{-
   The Dot and Dollar Operators

   (.) is function composition. (.) was chosen for this operator because it
   vaguely resembles the small centered o used in math to denote function
   composition.

   ($) is function application. It may seem useless to have this because we
   can always apply functions in Haskell just by putting arguments after them.
   ($) is useful though because it has a low operator precendence that allows
   it to be use to avoid excessive parentheses.

   Using (.) and ($) together can get confusing sometimes. These reps will
   help etch the meaning of (.) and ($) into your muscles so that knowing
   when to use each will become a reflex.

   - Implement a `dollar` function that takes a function and an argument
     and uses ($) to apply the function to that argument.

   - Implement a `myDollar` function that takes a function and and argument
     and applies the function to that argument without using dollar. Yes,
     these two functions really do the same thing -- dollar isn't magical.

   - Implement a `functionalDollar` "function" that takes a function a returns
     that function. Make sure to denote this way of thinking in the type signature
     with parentheses . Also note that this function is still identical to the
     other too.

   - Use the dollar operator in a trivial fashion to find the length of a string
   - Use parentheses to find the length of a string that is made by appending two strings
   - Use dollar to find the length of a string made by appending two strings

   - Implement a `compose` function that uses (.) to combine two functions. Your
     function should take all the arguments involved in the type signature
     explicitly.

   - Implement a `myCompose` function that is the same as `compose`, but does
     not use (.). Instead just apply the functions use normal Haskell syntax.

   - Implement a `functionalCompose` function that use (.), but leaves off the
     third argument. That is to say, the only arguments your function should
     explicitly take are the two functions being combined. Use parentheses in
     the type signature to reflect this way of thinking about composition.

   - Use ($) twice to find the number of words in a string that is made by
     appending two strings
   - Use (.) and parenteses to do the same thing
   - Use (.) and ($) together to do it again, using no parentheses


   - Use ($) three times to find the number of words in a string constructed
     by `concat`ing a list of strings.
       - Do the same thing using one (.) and two ($)s 
   - Do the same thing using two (.)s and one ($)
   - Do the same thing using three (.)s and paretheses - no ($)s allowed!

   - Compose a haiku in a Haskell String
   - Find number of words on each line of the haiku by using `map`, `words` and `lines`.
   - Do it again, finding a different combination of (.) and ($)s that also works
-}
module Year2020.Month03.Day15 where

dollar :: (a -> b) -> a -> b
dollar f a =
  f $ a

myDollar :: (a -> b) -> a -> b
myDollar f a =
  f a

functionalDollar :: (a -> b) -> (a -> b)
functionalDollar f =
  f

useDollar :: Int
useDollar =
  length $ "Hello World"

useParentheses :: Int
useParentheses =
  length ("Hello " ++ "World")

useDollarAgain :: Int
useDollarAgain =
  length $ "Hello " ++ "World"

compose :: (b -> c) -> (a -> b) -> a -> c
compose f g a =
  (f . g) a

myCompose :: (b -> c) -> (a -> b) -> a -> c
myCompose f g a =
  f (g a)

functionalCompose :: (b -> c) -> (a -> b) -> (a -> c)
functionalCompose f g =
  f . g

wordCount1 :: Int
wordCount1 =
  length $ words $ "Hello " ++ "World"

wordCount2 :: Int
wordCount2 =
  (length . words) ("Hello " ++ "World")

wordCount3 :: Int
wordCount3 =
  length . words $ "Hello " ++ "World"

wordCount4 :: Int
wordCount4 =
  length $ words $ concat $ ["Hello ", "World"]

wordCount5 :: Int
wordCount5 =
  length . words $ concat $ ["Hello ", "World"]

wordCount6 :: Int
wordCount6 =
  length . words . concat $ ["Hello ", "World"]

wordCount7 :: Int
wordCount7 =
  (length . words . concat) ["Hello ", "World"]

haiku :: String
haiku =
  "Good morning Haskell.\n\
  \It's good to see you again.\n\
  \Keep all my values pure."

wordsOnLines :: [Int]
wordsOnLines =
  map (length . words) . lines $ haiku

wordsOnLines2 :: [Int]
wordsOnLines2 =
  (map (length . words) . lines) haiku

wordsOnLines3 :: [Int]
wordsOnLines3 =
  map (length . words) $ lines haiku

