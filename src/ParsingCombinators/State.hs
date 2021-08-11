module ParsingCombinators.State (
  State,
  make,
  next,
  index,
  current,
) where

data State = State {index :: Int, current :: String}
  deriving (Show)

make :: String -> State
make str = State 0 str

next :: State -> Maybe (Char, State)
next (State _ "") = Nothing
next (State i (ch : chs)) = Just (ch, State (i + 1) chs)

instance Eq State where
  (State i _) == (State i' _) = i == i
