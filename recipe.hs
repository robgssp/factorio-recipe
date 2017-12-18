-- Factorio recipe utilities by Rob Glossop.

import Data.Aeson
import Data.Aeson.Types
import Data.Ratio
import Data.String
import System.IO
import System.IO.Unsafe
import Data.Either
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Vector as Vec

newtype Item = Item { itemName :: String } 
  deriving (Read, Show, Eq, Ord)

instance IsString Item where
  fromString = Item

data Recipe = Recipe { recName :: String
                     , recTime :: Double
                     , recResults :: [(Item, Integer)]
                     , recComponents :: [(Item, Integer)]} 
       deriving (Read, Show, Eq, Ord)

parseResult :: Value -> Parser (Item, Integer)
parseResult = withObject "result/component" $ \v -> (,)
                 <$> v .: "name"
                 <*> v .: "amount"

instance FromJSON Item where
  parseJSON = withText "item" $ \v -> 
                return (Item (Text.unpack v))

parseResults :: Text.Text -> Object -> Parser [(Item, Integer)]
parseResults s v = 
  v .: s >>= 
  (withArray (Text.unpack s) $ \arr -> 
     sequence (Vec.toList (fmap parseResult arr)))

instance FromJSON Recipe where
  parseJSON = withObject "Recipe" $ \v -> Recipe
                <$> v .: "name"
                <*> v .: "energy_required"
                <*> parseResults "results" v
                <*> parseResults "ingredients" v

readJson :: IO (Either String [Recipe])
readJson = do f <- openFile "recipes-normalized16.json" ReadMode
              str <- B.hGetContents f
              return (eitherDecode str)

-- Limit recipe scope to the given item: remove all recipes that
-- result in the given item
limit :: Item -> [Recipe] -> [Recipe]
limit item recipes = filter (\r -> not (any ((==item) . fst) (recResults r))) recipes

limits :: [Item] -> [Recipe] -> [Recipe]
limits items recipes = foldr (\item rs -> limit item rs) recipes items

recipesOf :: Item -> [Recipe] -> [Recipe]
recipesOf item recipes = filter (\r -> any ((==item) . fst) (recResults r)) recipes

-- Get the unique recipe for an item. Break if there's multiple. 
recipeOf :: Item -> [Recipe] -> Maybe Recipe
recipeOf item recipes = case recipesOf item recipes of
                          [r] -> Just r
                          [] -> Nothing
                          _ -> error ("Multiple recipes for " ++ itemName item)

bases :: Item -> [Recipe] -> Set.Set Item
bases item recipes = bases' item
  where bases' :: Item -> Set.Set Item
        bases' item = case recipeOf item recipes of
                        Just r -> Set.unions (map (bases' . fst) (recComponents r))
                        Nothing -> Set.singleton item

ingredients :: Item -> [Recipe] -> Set.Set Item
ingredients item recipes = ingredients' item
  where ingredients' :: Item -> Set.Set Item
        ingredients' item = case recipeOf item recipes of
                        Just r -> Set.unions (Set.singleton item :
                                              (map (ingredients' . fst) 
                                                   (recComponents r)))
                        Nothing -> Set.singleton item

yield :: Item -> Recipe -> Integer
yield item recipe = case filter ((==item) . fst) (recResults recipe) of
                      [(_, yield)] -> yield
                      [] -> error ("No yield for " ++ show item ++ " in " ++ show recipe)
                      _ -> error ("Multiple yields for " ++ show item ++ " in " ++ show recipe)

unionAdd = Map.unionWith (+)
unionsAdd = foldr unionAdd Map.empty

baseAmounts :: Item -> [Recipe] -> Map.Map Item Rational
baseAmounts item recipes = amounts' 1 item
  -- to make count of item, how many base are required? a: input count / yield
  where amounts' :: Rational -> Item -> Map.Map Item Rational
        amounts' count item = 
          case recipeOf item recipes of
            Just recipe -> unionsAdd 
                             (map (\(component, needed) -> 
                                    amounts' (count * 
                                              (needed % yield item recipe))
                                              component)
                                  (recComponents recipe))
            Nothing -> Map.singleton item count

fullAmounts :: Item -> [Recipe] -> Map.Map Item Rational
fullAmounts item recipes = amounts' 1 item
  where amounts' :: Rational -> Item -> Map.Map Item Rational
        amounts' count item = 
          case recipeOf item recipes of
            Just recipe -> unionsAdd 
                             (Map.singleton item count :
                              (map (\(component, needed) -> 
                                     amounts' (count * 
                                               (needed % yield item recipe))
                                               component)
                                   (recComponents recipe)))
            Nothing -> Map.singleton item count

sciences :: [Item]
sciences = ["science-pack-1", "science-pack-2", "science-pack-3", "production-science-pack","high-tech-science-pack"]

amounts :: [Item] -> [Recipe] -> Map.Map Item Rational
amounts items recipes = unionsAdd (map (flip fullAmounts recipes) items)
amounts' items recipes = unionsAdd (map (flip baseAmounts recipes) items)

readable :: Map.Map Item Rational -> Map.Map Item Double
readable = fmap fromRational

dingus = do Right recipes <- readJson
            return $ limits [ "iron-plate","copper-plate", "plastic-bar"
                            , "petroleum-gas" 
                            , "electric-engine-unit", "steel-plate"]
                            recipes
recipes = unsafePerformIO dingus

main = do recipes <- dingus          
          print (fullAmounts "high-tech-science-pack" recipes)

-- check base ingredients for ht, prod science
-- prod sci: 10 brick, 10 iron, 25 copper, 10 steel, 10 plastic, 1 EEU
-- ht sci: 15 iron, 47.5 copper, 10 plastic, 3 processor, 1 battery
