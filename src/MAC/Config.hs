module MAC.Config where

import Prelude

data Config = Config
  {
    useExtraRoundStage ::  Bool,
    useModuleFullAdder :: Bool,
    useState :: Bool,
    useVector :: Bool,
    useRotation :: Bool,
    useOneHot :: Bool
  }
  deriving (Show, Bounded)


allConfigs :: [Config]
allConfigs = [ Config useExtraStage useModuleAdder useState useVector useRotation useOneHot |
  useExtraStage <- [False, True],
  useModuleAdder  <- [False, True],
  useState  <- [False, True] ,
  useVector  <- [False, True],
  useRotation  <- [False, True] ,
  useOneHot  <- [False, True]
  ]


configsIWSBP26 :: [Config]
configsIWSBP26 = [ Config useExtraStage useModuleAdder useState useVector useRotation useOneHot |
  useExtraStage <- [True],
  useModuleAdder  <- [False], -- we decided not to use the explicit module adder
  useState  <- [False, True] ,
  useVector  <- [False, True],
  useRotation  <- [False, True] ,
  useOneHot  <- [False, True]
  ]



describe :: Config -> String
describe Config {..} =
  (if useExtraRoundStage then "with EndRound tage" else "no EndRound stage") <> " / " <>
  (if useModuleFullAdder then "module adder" else "inline adder") <> " / " <>
  (if useState then "state monad" else "mealy machine") <> " / " <>
  (if useRotation then "rotating" else "indexing") <> " / " <>
  (if useVector then "Vec" else "BitVector") <> " / " <>
  (if useOneHot then "OneHotCounter" else "IndexCounter")

describe' :: Config -> String
describe' Config {..} =
  (if useExtraRoundStage then "with EndRound stage" else "no EndRound stage") <> " / " <>
  (if useState then "state monad" else "mealy machine") <> " / " <>
  (if useRotation then "rotating" else "indexing") <> " / " <>
  (if useVector then "Vec" else "BitVector") <> " / " <>
  (if useOneHot then "OneHotCounter" else "IndexCounter")

defaultConfig :: Config
defaultConfig = Config {
  useExtraRoundStage = False,
  useModuleFullAdder = False,
  useState = False,
  useVector = False,
  useRotation = False,
  useOneHot = False
}
