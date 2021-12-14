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
 - Ok, pretty cool no?
 -
 -}

 -- TODO - applicative style and the 'hungry' function
 -- TODO - other examples aside from Maybe
 -- TODO - the (->) applicative
 -- TODO - composing applicatives? (e.g. reader + maybe)
