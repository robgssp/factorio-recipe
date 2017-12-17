import qualified Data.Set as Set
import qualified Data.Map as Map

data Item = Recipe { name :: String
                   , time :: Rational
                   , yield :: Integer
                   , components :: [(Item, Integer)]}
          | Base { name :: String } deriving (Read, Show, Eq, Ord)

recipes :: [(String, Rational, Integer, [(String, Integer)])]
recipes = [("circuit2"
           ,6
           ,1
           ,[("circuit1", 2)
            ,("plastic", 2)
            ,("wire", 4)])
          ,("circuit1"
           ,1/2
           ,1
           ,[("iron", 1)
            ,("copper", 3)])]

items :: [Item]
items = map (\(name, time, yield, components) -> 
               Recipe name time yield
                      (map (\(name', amt) -> (itemOf name', amt))
                           components))
            recipes

itemOf name' = 
  case filter ((==name') . name) items of 
    [item] -> item
    [] -> Base name'

bases :: Item -> Set.Set Item
bases b@(Base name) = Set.singleton b
bases (Recipe name time yield components) =
  Set.unions (map (bases . fst) components)

amounts :: Item -> Map.Map Item Integer
amounts 
