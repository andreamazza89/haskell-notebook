import Control.Applicative

{- Applicative
 -
 - Applicative is part of the Functor/Applicative/Monad family.
 -  (a long parenthesis on the above family begins)
 -    The way I think of it is that these help describe some sort of 'context'
 -    in which a program (or part of it) is running. The easiest example for
 -    me is Maybe, which is a context signalling the possibility that the
 -    result of a program may or may not be there.
 -
 -    A typical metaphor for this 'context' idea is that of a 'box'. In this case a
 -    box that may or may not contain the result of a program.
 -
 -    Another important concept to grasp is that this 'context' or 'box' carries
 -    some kind of information; in Maybe's case, the information is whether the
 -    content of the box is there or not, but with Either you can describe failures
 -    with an arbitrary type.
 -
 -  (parenthesis over)
 -
 - First of all, Applicative is a type class, with this definition:


   class Functor f where
     fmap :: (a -> b) -> f a -> f b

   class Functor f => Applicative f where
     pure :: a -> f a
     (<*>) :: f (a -> b) -> f a -> f b


 - Notice the similarity with functor: there is some 'box' type (f), which 'contains'
 -  another type ('a' in the definition above, let's call this 'content'), which you are able
 -  to map by providing a function that takes the content as input and returns some other
 -  content.
 -
 -  The big difference is that with Functor you simply provide the mapping function, whereas
 -  with Applicative the function must be inside the 'box'.
 -  'pure' is how you can put the mapping function into the container (the correct term is 'lift' -
 -  you lift the function into the 'box')
 -
 - Ok, so far so good, but what's the point?
 -  The way I currently understand it, is that the motivation for Applicative is when you need to
 -  'mix' (can I say 'compose' here?) together multiple 'boxes'.
 -
 -  Let's make a concrete EXAMPLE, using the Maybe type.
 -
 -  Say we are trying to setup tennis teams, so we have functions for finding
 -  available players from different parts of the world:
 -}

findPlayer :: From -> Maybe Player
findPlayer "Italy" = Just "Berrettini"
findPlayer "Switzerland" = Just "Federer"
findPlayer _ = Nothing

type Player = String
type From = String

{-
 - Ok, so now we have a nice way of describing the fact that players may or may
 - not be available.
 -
 - If what we need to do is deal with one player at a time, then Functor is all we
 - need. For example, say we want to **highlight** that player, we have a way to
 - do so without worrying about the player being there or not:
 -}

highlightPlayer :: Player -> Player
highlightPlayer player = "**" <> player <> "**"

highlightItalianPlayer :: Maybe Player
highlightItalianPlayer = fmap highlightPlayer (findPlayer "Italy")

highlightRussianPlayer :: Maybe Player
highlightRussianPlayer = fmap highlightPlayer (findPlayer "Russia")

{-
 - Great. Now, what if we have a function that needs two players to arrange a match?
 -
 -}

type Match = String

match :: Player -> Player -> Match
match p1 p2 = p1 <> " VS " <> p2

{-
 - As things stand, we're a little stuck, because we have to check that both players exist
 - in order to set up a match.
 -}

organiseInternationalMatch1 :: From -> From -> Maybe Match
organiseInternationalMatch1 country1 country2 =
  case findPlayer country1 of
    Nothing -> Nothing
    Just p1 -> case findPlayer country2 of
      Nothing -> Nothing
      Just p2 -> Just (match p1 p2)

sampleMatch1 :: Maybe Match
sampleMatch1 = organiseInternationalMatch1 "Italy" "Peru"

{-
 - This works fine, but it's both dense and not very scalable: what are we going to do about doubles??
 - It'll be a nasty pyramid.
 -
 - This is where Applicative comes in. Recall the shape of (<*>), where we have a function inside the
 - 'box' and a 'box' with the input for that function
 -
 -   f (a -> b) -> f a -> f b
 -
 - The above doesn't quite solve our problem as we need something more like
 -
 -   f (Player -> Player -> Match) -> f Player -> f Player -> f Match
 -
 - However, multiple applications of (<*>) get us to this type, as follows:
 -   (bear in mind that <*> associates left, function types associate right
 -    and 'pure' brings a function into the 'box')
 -
 -   1 - ((pure match <*> findPlayer country1) <*> findPlayer country2)
 -         __________________________________      ___________________
 -                        |                                |
 -                        v                                |
 -        (Maybe (Player -> (Player -> Match)) -> Maybe Player -> Maybe (Player -> Match))
 -                                                         |      ______________________
 -                                                         |                |
 -         ------------------------------------------------------------------
 -         |                                               |
 -         |                              |----------------|
 -         v                              v
 -   2 -  Maybe (Player -> Match) -> Maybe Player -> Maybe Match
 -
 -
 - Woof, hope the above makes some sense - here's what we get
 -}

organiseInternationalMatchApplicative ::  From -> From -> Maybe Match
organiseInternationalMatchApplicative country1 country2=
  pure match <*> findPlayer country1 <*> findPlayer country2

sampleMatchApplicative :: Maybe Match
sampleMatchApplicative = organiseInternationalMatchApplicative "Italy" "Switzerland"

{-
 - Ok, pretty cool no? This also keeps working as we add more arguments to the initial function, like so:
 -

    pure f (a -> ... -> n) <*> f a <*> ... <*> f n

 -
 - The main intuition for applicative is that if you squint, it kinda looks like simple
 -  function application. For example, the above kinda just looks like
 -

     match (findPlayer country1) (findPlayer country2)

 - Which is why (I think) they called it Applicative
 -
 -}

{-
 - Another classic example would be that of a Parser.
 -
 - Parsers are used to extract (*parse*) structured information out of unstructured data; for
 -  example, consider the following signature:
 -
 -}

parse :: Parser p -> String -> Maybe p
parse = undefined

-- This is not very useful, but I don't want to dig into how parsers work,
-- so just adding this type to make the compiler happy
data Parser p = Parser

{-
 - In the above function, given a 'recipe' for parsing a value of type 'p' and a String of
 -  text, we will either get a value of type p if we succeed, or Nothing.
 -
 - Now, imagine we have type, Person, whose data constructor requires age and height
 -
 -}

data Person = Person Age Height

type Age = Int
type Height = Int

{-
 - And the following parsers (just believe that these exist and can be implemented)
 -
 -}

ageParser :: Parser Age
ageParser = undefined

heightParser :: Parser Height
heightParser = undefined

{-
 - Now, if the Parser is Applicative, we have all the ingredients we need to
 -  create a Parser of Person, like so:
 -
 -}

-- Again, just making the compiler happy here

instance Functor Parser where
  fmap = undefined

instance Applicative Parser where
  pure = undefined
  (<*>) = undefined

parsePerson :: Parser Person
parsePerson =
  pure Person
    <*> ageParser
    <*> heightParser

{-
 - In this case, the 'box'/'context' is Parser, into which we 'lift' the
 -  Person contructor using *pure*, like so:
 -
     Parser (Age -> Height -> Person) -> Parser (Age) -> Parser (Height) -> Parser (Person)
         pure Person                 <*>   parserAge  <*> heightParser
 -
 -
 -  By the way, *pure g* simplifies (not sure if simplification is the correct term here) to *g <$>*,
 -  so the above could be simplified to
 -}

simplifiedParsePerson :: Parser Person
simplifiedParsePerson = Person <$> ageParser <*> heightParser

{-
 - Which is what I think is called "applicative style". As in    g <$> a <*> b <*>   is applicative style.
 -
 -}

{-
 - There is a funky little function that is available for all Applicatives, with the following type signature:
 -

       (*>) :: f a -> f b -> f b

 -  Which basically takes 2 Applicative values and returns the second, similar to *const*
 -

       const :: a -> b -> b

 -
 - If we consider the examples we've seen so far (Maybe and Parser), this funciton
 -  seems pretty pointless: why would we want to discard any of the Applicatives?
 -
 -  I might be missing something, but I think the reason for this operator is when the 'context' is
 -   affected by sequencing a few together.
 -   For example, within the Parser context, the thing that's changing between parser is the input
 -   String that gets consumed. In other words, let's imagine an input string like
 -

    "age: 22, height: 135"

 -
 -  then we can imagine the parser we made above to kinda work like this:
 -

    1
  ______
  Person <$> ageParser <*> heightParser -- "age: 22, height: 135" -- (Age -> Int -> Person)

                2
              ______
  Person <$> ageParser <*> heightParser -- "height: 135" -- (Int -> Person)

                              3
                            ______
  Person <$> ageParser <*> heightParser -- "" -- (Person)


 -
 -
 - Now, let's imagine that we need to support a new input string, like so:
 -

    "age: 22, name: Lucio, height: 135"

 -
 - Our parser will not work with the above: it first parses Age fine, but then fails on Height.
 -  This is where (*>) comes to the rescue, as we can add a nameParser
 -
 -}

nameParser :: Parser String
nameParser = undefined

{-
 - And use this in our new parser, which parses and then discards the name:
 -   (we're using <*, which is *> flipped)
 -
 -}

personParserWithName :: Parser Person
personParserWithName =
  Person <$> ageParser <* nameParser <*> heightParser


{-
 - Right, final one: there is an Applicative instance that deserves a special
 -  mention because it is extra confusing.
 -
 -  It's the Applicative instance for the function type (->).
 -
 -  I think the confusion stems from the fact that the 'context' we are lifting
 -  a function into (or over? not sure about the lingo) is a function itself, which is a bit meta.
 -
 -  But let's try and dispel the confusion.
 -  First of all, there is a type to describe functions, which is the arrow type (->).
 -  This type has two parameters, like this:

    (->) a b = ....

 -  where a is the function argument type and b is the return type.
 -
 - Now, because Applicatives must be declared on types with kind * -> *, the (->) instance
 -  is like

   instance Applicative (-> r) where
     ...

 -  which means that the 'context' when using Applicative for a function is a function with an argument
 -  of a fixed type (r in the instance declaration above). In other words, when we compose a number of
 -  Applicative functions, they must all accept the same input type, like so:


    g <$> a <*> b <*> c -- a, b and c must be functions that all take the same input type

 - A practical use of a series of functions that all take the same input is sharing the same Configuration.
 - In haskell, if you wanted to pass some Config around a number of functions, you could simply make it one of
 - the function arguments.
 -
 - However, Applicative lets us avoid doing that; in the following example, we build a summary of the Config
 - using the (->) Applicative:
 -
 -}

data Config = Config { url :: String, portNumber :: Int, message :: String }

type Summary = String

configSummary :: String -> Int -> String -> Summary
configSummary url portNumber message =
  "Hi, the url is: " <> url <> ", portNumber: " <> show portNumber <> " and message: " <> message

runSummary :: Config -> Summary
runSummary =
  configSummary <$> url <*> portNumber <*> message

-- the cool (and not very intuitive to me) thing is that the resulting 'context' above is itself a function,
--  so we can simply apply the function:

doRunSummary :: Summary
doRunSummary =
  runSummary (Config "my.place" 42 "howdy")


