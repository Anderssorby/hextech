module HexTech.Engine.Common where
import           Control.Lens


data CommonVars = CommonVars
   deriving (Show, Eq)

makeClassy ''CommonVars

initCommonVars :: CommonVars
initCommonVars = CommonVars
