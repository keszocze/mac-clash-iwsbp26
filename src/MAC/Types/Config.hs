module MAC.Types.Config where

import Prelude

data Config = Config
  { useModuleFullAdder :: Bool,
    useState :: Bool,
    useVector :: Bool,
    useRotation :: Bool,
    useOneHot :: Bool
  }
  deriving (Show, Bounded)


--allConfigs = [ Config a b c d e | a  <- [True, False], b  <- [True, False] , c  <- [True, False], d  <- [True, False] , e  <- [True, False]]
-- TODO nachher dann wirklich alles unterstützen
allConfigs = [ Config useModuleAdder useState useVector useRotation useOneHot |
  useModuleAdder  <- [False, True],
  useState  <- [False] ,
  useVector  <- [False, True],
  useRotation  <- [False, True] ,
  useOneHot  <- [False, True]
  ]


describe :: Config -> String
describe Config {..} =
  (if useModuleFullAdder then "module adder" else "inline adder") <> " / " <>
  (if useState then "state" else "mealy machine") <> " / " <>
  (if useRotation then "rotate" else "indexing") <> " / " <>
  (if useVector then "Vec" else "BitVector") <> " / " <>
  (if useOneHot then "OneHotCounter" else "IndexCounter")

defaultConfig = Config {
  useModuleFullAdder = True,
  useState = False,
  useVector = False,
  useRotation = False,
  useOneHot = False
}
