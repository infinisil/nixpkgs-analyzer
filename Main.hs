{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import qualified Control.Concurrent.Async      as Async
import           Control.Concurrent.Async.Pool
import           Control.Concurrent.STM
import           Control.Monad.State
import           Data.Aeson                    hiding (Result)
import           Data.Aeson.Types              hiding (Result)
import qualified Data.ByteString.Lazy.Char8    as BS
import           Data.Char
import           Data.List                     (intercalate, isPrefixOf, nub)
import           Data.List.Split
import           Data.Map                      (Map)
import qualified Data.Map                      as Map
import           Data.Maybe                    (fromMaybe, listToMaybe)
import           Data.Set                      (Set)
import qualified Data.Set                      as Set
import           GHC.Conc
import           GHC.Generics
import           ListT                         (toList)
import qualified STMContainers.Map             as SM
import           System.Exit
import           System.Process

data Input = Derivation_ { derivation :: String }
           | Attrs_ { names :: [String] }
           | Other_ { nixType :: String }
           deriving (Show, Generic)

instance FromJSON Input where
  parseJSON = genericParseJSON defaultOptions
    { constructorTagModifier = map toLower . init }

type AttrPath = [String]
type UseCounts = SM.Map String Int

data Result a = Derivation a
              | Attrs (Map String (Result a))
              | Other
              | Failure String
              | Blacklist String
              deriving (Show, Functor, Generic)

instance ToJSON a => ToJSON (Result a) where
  toEncoding = genericToEncoding defaultOptions
    { constructorTagModifier = map toLower }

blacklist :: Map AttrPath String
blacklist = Map.fromList
  [ (["gnome3", "gnome3"], "Recursive")
  ]

getPath :: UseCounts -> TaskGroup -> AttrPath -> IO (Result ())
getPath useCounts group path = case Map.lookup path blacklist of
  Just reason -> return $ Blacklist reason
  Nothing -> do
    let args = [ "eval.nix", "--eval", "--arg", "path", "[ " ++ intercalate " " (map show path) ++ " ]", "--strict", "--json"]
    (exitCode, stdout, stderr) <- readProcessWithExitCode "/run/current-system/sw/bin/nix-instantiate" args ""
    case exitCode of
      ExitFailure _ -> do
        putStrLn $ "Evaluation failure for " ++ show path
        return $ Failure $ unlines $ filter (not . ("trace: " `isPrefixOf`)) (lines stderr)
      ExitSuccess   -> case eitherDecode (BS.pack stdout) of
        Left err               -> return $ Failure err
        Right Attrs_ { names } -> do
          let ioMap = Map.fromSet (\name -> getPath useCounts group (path ++ [name])) (Set.fromList names)
          Attrs <$> mapTasks group ioMap
        Right Derivation_ { derivation } -> do
          let args = [ "eval.nix", "--arg", "path", "[ " ++ intercalate " " (map show path) ++ " ]" ]
          putStrLn $ "Instantiating " ++ show path
          (exitCode, stdout, stderr) <- readProcessWithExitCode "/run/current-system/sw/bin/nix-instantiate" args ""
          let usedAttrs = nub $ drop 17 <$> filter ("trace: attrpath: " `isPrefixOf`) (lines stderr)
          forM usedAttrs $ \attr -> atomically $ do
            val <- fromMaybe 0 <$> SM.lookup attr useCounts
            SM.insert (val + 1) attr useCounts
          return $ Derivation ()
        Right _ -> return Other



main :: IO ()
main = do
  caps <- getNumCapabilities
  print caps
  useCounts <- SM.newIO :: IO UseCounts
  result <- withTaskGroup caps $ \group -> Async.withAsync (runTaskGroup group) (\a -> getPath useCounts group [])
  counts <- atomically . toList . SM.stream $ useCounts
  let final = collectResults result (Map.fromList (map (\(k, v) -> (splitOn "." k, v)) counts))
  let encoded = encode final
  BS.writeFile "out" encoded

collectResults :: Result () -> Map [String] Int -> Result Int
collectResults (Derivation _) counts = Derivation $ Map.findWithDefault 0 [] counts
collectResults (Attrs attrs) counts = Attrs $ Map.mapWithKey collectForElems attrs
  where collectForElems key value = collectResults value
          (Map.mapKeys tail $ Map.filterWithKey (\k _ -> maybe False (\v -> v == key) (listToMaybe k)) counts)
collectResults Other _ = Other
collectResults (Failure err) _ = Failure err
collectResults (Blacklist reason) _ = Blacklist reason
