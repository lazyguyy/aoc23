{-# LANGUAGE TupleSections, OverloadedStrings #-}

module Day20 where

import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Set as S

import Data.Maybe (catMaybes, fromMaybe)
import Data.Monoid

import Text.Regex.Applicative
import Control.Monad.State
import Control.Monad.Trans.Writer.CPS

data Signal = Low | High deriving (Show, Eq)

flipSignal :: Signal -> Signal
flipSignal Low = High
flipSignal High = Low

type Pulse = (Module, Signal)

data ModuleType = Broadcaster | FlipFlop | Conjunction deriving (Eq)
instance Show ModuleType where
    show Broadcaster = " "
    show FlipFlop    = "%" 
    show Conjunction = "&" 

data Module = Output String | Module {name :: String, mType :: ModuleType, lastSignal :: Signal, inputs :: [String], outputs :: [String]} deriving (Eq)

instance Show Module where
    show (Module n t l _ o) = show t ++ n ++ "(" ++ show l ++ "):" ++ show o
    show (Output n) = n ++ "!"

type Network = M.Map String Module

parseModule :: RE Char (String, [String])
parseModule = (,) <$> (many (psym (/= ' ')) <* string " -> ")
                  <*> many (many (psym (/= ',')) <* (optional $ string ", "))

buildModule :: (String, [String]) -> Module
buildModule (n, os) = Module parsedName parsedType Low [] os
    where
        (parsedName, parsedType) = case head n of
            '%' -> (tail n, FlipFlop)
            '&' -> (tail n, Conjunction)
            _   -> (n, Broadcaster)

buildGraph :: [(String, [String])] -> Network
buildGraph parsed = foldl (\m i -> let newM = buildModule i in M.insert (name newM) (newM {inputs = inputsByName (name newM)}) m) initialMap parsed 
    where
        getName str = case head str of
            '%' -> tail str
            '&' -> tail str
            _   -> str
        inputsByName :: String -> [String]
        inputsByName target = map (getName . fst) $ filter (elem target . snd) parsed
        nonTerminals = S.fromList $ map fst parsed
        terminals = filter (not . flip S.member nonTerminals) $ concat $ map snd parsed
        initialMap = M.fromList [(terminal, Output terminal) | terminal <- terminals]

updateNetwork :: Module -> State Network ()
updateNetwork m = get >>= put . M.insert (name m) m

sendSignal :: [String] -> Signal -> State Network [Pulse]
sendSignal names s = do
    network <- get
    pure $ zip (map (network M.!) names) $ repeat s 

handlePulse :: Pulse -> State Network [Pulse]
handlePulse (Output _, _) = pure []
handlePulse (m, s) = case mType m of
    Broadcaster -> do
        updateNetwork (m {lastSignal = s})
        sendSignal (outputs m) s 
    FlipFlop -> if s == High then pure [] else do
        let outgoing = flipSignal $ lastSignal m
        updateNetwork (m {lastSignal = outgoing})
        sendSignal (outputs m) outgoing
    Conjunction -> do
        network <- get
        let outgoing = if all (== High) $ map (lastSignal . (network M.!)) $ inputs m then Low else High
        updateNetwork (m {lastSignal = outgoing})
        sendSignal (outputs m) outgoing

simulate :: [Pulse] -> WriterT (Sum Int, Sum Int) (State Network) ()
simulate []     = return ()
simulate (p:ps) = do
    tell (if snd p == High then (1, 0) else (0, 1))
    newPulses <- lift $ handlePulse p
    simulate (ps ++ newPulses)

part1 :: Network -> Int -> (Sum Int, Sum Int)
part1 _ 0 = (0, 0)
part1 network n = mappend counts $ part1 updated $ n - 1
    where ((_, counts), updated) = runState (runWriterT $ simulate [(network M.! "broadcaster", Low)]) network

observeModule :: [Pulse] -> Module -> WriterT (Sum Int) (State Network) ()
observeModule [] _ = return ()
observeModule (p:ps) m = if p == (m, Low)
                         then tell 1 >> return ()
                         else do
    newPulses <- lift $ handlePulse p
    observeModule (ps ++ newPulses) m

part2 :: Network -> Module -> Int
part2 network target = if count == 1 then 1 else 1 + (part2 updated target)
    where
        ((_, count), updated) = runState (runWriterT $ observeModule [(network M.! "broadcaster", Low)] target) network

solve :: T.Text -> [String]
solve text = [let (high, low) = part1 network 1000 in show $ high * low,
              -- technically this does work for part 2, but since the gate feeding into rx is a conjunction gate (G) and the input graph is a bunch of disjoint cycles,
              -- I used this function to find when each of the inputs leading to G gives a high signal and calculated their lcm manually.
              show $ part2 network (fromMaybe (network M.! "broadcaster") $ M.lookup "rx" network)]
    where
        network = buildGraph $ catMaybes $ map (=~ parseModule) $ lines $ T.unpack text
