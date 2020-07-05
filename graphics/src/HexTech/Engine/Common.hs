module HexTech.Engine.Common where
import           Control.Lens

import           HexTech.Engine.Types

data CommonVars = CommonVars
   deriving (Show, Eq)

makeClassy ''CommonVars

initCommonVars :: CommonVars
initCommonVars = CommonVars
