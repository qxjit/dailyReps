{-

- Write out abstract let form
- Calculate 8, using let to bind 4 to a name
- Calculate 9, multiplying a let variable, three, by itself
- Calculate 10 using two let expression, one for 5 and one for 2
- Calculate 10 again using one let expression for both 5 and 2
- Calculate 8 again, using a lambda to simulate let
- Calculate 10 again, using a lambda to simulate let
- Calculate 5 with a single let, start with two, multiplying by itself to get 4,
  defining 1 and then adding that to four. (4 bindings in total, including 5 itself)
- Calculate 5 again as above, but this time order the let bindings in
  decreasing order of magnitude (starting with 5, ending with 1)
- Build a list of 1000 ones by using a recursive let to build an infinite list
  and then using `take 1000`
- Build an infinite list of alternating ones and zeros using a mutually
  recursive let
- Use a let in a do expression. Add Just 1, 2, 3, and Just 4, binding each
  one to a name using either let or `<-`
-}
module Year2020.Month04.Day07 where

{-
   Let Form:

   let
     <name> = <expression> {1+}
   in
     <expression>
-}

eight :: Integer
eight =
  let
    four = 4
  in
    four * 2

nine :: Integer
nine =
  let
    three = 3
  in
    three * three

longTen :: Integer
longTen =
  let
    five = 5
  in
    let
      two = 2
    in
      five * two

shortTen :: Integer
shortTen =
  let
    five = 5
    two = 2
  in
    five * two

lambdaEight :: Integer
lambdaEight =
  (\four -> four * 2) 4

lambdaTen :: Integer
lambdaTen =
  (\five two -> five * two) 5 2

orderedFive :: Integer
orderedFive =
  let
    two = 2
    four = two * two
    one = 1
    five = four + one
  in
    five

unorderedFive :: Integer
unorderedFive =
  let
    five = four + one
    four = two * two
    two = one + one
    one = 1
  in
    five

thousandOnes :: [Integer]
thousandOnes =
  let
    ones = 1 : ones
  in
    take 1000 ones

infiniteOnesAndZeros :: [Integer]
infiniteOnesAndZeros =
  let
    onesAndZeros = 1 : zerosAndOnes
    zerosAndOnes = 0 : onesAndZeros
  in
    onesAndZeros

just10 :: Maybe Integer
just10 = do
  one <- Just 1

  let
    two = 2
    three = 3

  four <- Just 4
  Just (one + two + three + four)
