{-# LANGUAGE TupleSections, OverloadedStrings #-}

module Day19 where

import qualified Data.Text as T
import qualified Data.Map as M

import Data.Maybe (catMaybes, fromMaybe)
import Data.List (find)
import Data.Char (isDigit)

import Text.Regex.Applicative

data Property = X | M | A | S deriving (Show, Eq, Ord)

type Object = [(Property, Int)]
-- property value, lower bound, upper bound (exclusive)
data PropConstraint = PropConstraint Property Int Int
instance Show PropConstraint where
    show (PropConstraint prop lower upper) = show lower ++ "<=" ++ show prop ++ "<" ++ show upper 
type ConstrainedObject = [PropConstraint]

objectValue :: Object -> Int
objectValue = sum . map snd

constrainedCount :: ConstrainedObject -> Int
constrainedCount = foldl (\acc (PropConstraint _ lower upper) -> acc * (max 0 $ upper - lower)) 1 

-- the Bool signals whether it is an upper bound (True) or a lower bound (False)
data Rule = Rule Property Bool Int deriving (Show, Eq, Ord)

data Mapping = Reject | Accept | Mapping {name :: String, rules :: [(Rule, String)], defaultMapping :: String} deriving (Show, Eq, Ord)

type Process = M.Map String Mapping


propertyParser :: RE Char Property
propertyParser = (pure X <* sym 'x') <|> (pure M <* sym 'm') <|> (pure A <* sym 'a') <|> (pure S <* sym 's')

numberParser :: RE Char Int
numberParser = read <$> many (psym isDigit)

ruleParser :: RE Char Rule
ruleParser = Rule <$> propertyParser 
                  <*> (pure True <* sym '<' <|> pure False <* sym '>') 
                  <*> numberParser

mappingParser :: RE Char Mapping
mappingParser = Mapping <$> (many (psym (/='{'))) <* sym '{'
                        <*> (many ((,) <$> ruleParser <* sym ':' <*> many (psym (/=',')) <* sym ','))
                        <*> many (psym (/='}')) <* sym '}'

objectParser :: RE Char Object
objectParser = sym '{' *> many ((,) <$> propertyParser <* sym '=' <*> numberParser <* (sym ',' <|> sym '}'))

isFinished :: Mapping -> Bool
isFinished (Mapping _ _ _) = False
isFinished _ = True

fulfillsRule :: Object -> Rule -> Bool
fulfillsRule [] _ = False
fulfillsRule ((prop, val):props) rule@(Rule comp direction bound) = 
    if prop == comp
    then (val < bound && direction) || (val > bound && not direction)
    else fulfillsRule props rule

followMapping :: Process -> Mapping -> Object -> Mapping
followMapping p (Mapping _ rs def) obj = p M.! (fromMaybe def $ fmap snd $ find (fulfillsRule obj . fst) rs)
followMapping _ res _ = res

isAccepted :: Process -> Mapping -> Object -> Bool
isAccepted p m o = Accept == (head $ filter isFinished $ iterate (flip (followMapping p) o) m)

-- first one will fit the rule and second one will not
cutConstraint :: PropConstraint -> Rule -> (PropConstraint, PropConstraint)
cutConstraint (PropConstraint prop lower upper) (Rule _ True value) = (PropConstraint prop lower cutOff, PropConstraint prop cutOff upper)
    where
        cutOff = max lower $ min value upper
cutConstraint (PropConstraint prop lower upper) (Rule _ False value) = (PropConstraint prop cutOff upper, PropConstraint prop lower cutOff)
    where
        cutOff = max lower $ 1 + (min value upper)

constrainObject :: ConstrainedObject -> Rule -> (ConstrainedObject, ConstrainedObject)
constrainObject [] _ = ([], [])
constrainObject (prop@(PropConstraint p _ _):props) rule@(Rule comp direction bound) =
    if p == comp
    then (valid:validRem, invalid:invalidRem)
    else (prop:validRem, prop:invalidRem)
    where
        (valid, invalid) = cutConstraint prop rule
        (validRem, invalidRem) = constrainObject props rule

pushObject :: Process -> Mapping -> ConstrainedObject -> [(Mapping, ConstrainedObject)]
pushObject p (Mapping _ [] def) o = [(p M.! def, o)]
pushObject p (Mapping mn ((r, n):rs) def) o = (p M.! n, valid) : (pushObject p (Mapping mn rs def) invalid)
    where
        (valid, invalid) = constrainObject o r
pushObject _ m o = [(m, o)]

countObjects :: Process -> [(Mapping, ConstrainedObject)] -> Int
countObjects _ [] = 0
countObjects p ((Reject, _):xs) = countObjects p xs
countObjects p ((Accept, o):xs) = constrainedCount o + countObjects p xs
countObjects p ((m, o):xs) = countObjects p ((pushObject p m o) ++ xs)

part1 :: Process -> [Object] -> Int
part1 p = sum . map objectValue . filter (isAccepted p beginning) 
    where
        beginning = p M.! "in"

part2 :: Process -> Int
part2 p = countObjects p [(p M.! "in", PropConstraint <$> [X, M, A, S] <*> [1] <*> [4001])]

solve :: T.Text -> [String]
solve text = [show $ part1 mappings objects, show $ part2 mappings]
    where
        [first, second] = T.splitOn "\n\n" text
        rawMappings = catMaybes $ map (=~ mappingParser) $ lines $ T.unpack first
        mappings = foldl (\acc m -> M.insert (name m) m acc) (M.fromList [("A", Accept), ("R", Reject)]) rawMappings
        objects = catMaybes $ map (=~ objectParser) $ lines $ T.unpack second
