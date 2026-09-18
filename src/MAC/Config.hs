module MAC.Config where

import Prelude

data Config = Config
  {
    useModuleFullAdder :: Bool,
    useState :: Bool,
    useVector :: Bool,
    useRotation :: Bool,
    useOneHot :: Bool
  }
  deriving (Show, Bounded)


allConfigs :: [Config]
allConfigs = [ Config useModuleAdder useState useVector useRotation useOneHot |
  useModuleAdder  <- [False, True],
  useState  <- [False, True] ,
  useVector  <- [False, True],
  useRotation  <- [False, True] ,
  useOneHot  <- [False, True]
  ]



describe :: Config -> String
describe Config {..} =
  (if useModuleFullAdder then "module adder" else "inline adder") <> " / " <>
  (if useState then "state monad" else "mealy machine") <> " / " <>
  (if useRotation then "rotating" else "indexing") <> " / " <>
  (if useVector then "Vec" else "BitVector") <> " / " <>
  (if useOneHot then "OneHotCounter" else "IndexCounter")


-- does not talk about the adder module
describe' :: Config -> String
describe' Config {..} =
  (if useState then "state monad" else "mealy machine") <> " / " <>
  (if useRotation then "rotating" else "indexing") <> " / " <>
  (if useVector then "Vec" else "BitVector") <> " / " <>
  (if useOneHot then "OneHotCounter" else "IndexCounter")

defaultConfig :: Config
defaultConfig = Config {
  useModuleFullAdder = False,
  useState = False,
  useVector = False,
  useRotation = False,
  useOneHot = False
}
