module Robot
    (
      robot
    , getName
    , getStrength
    , getPoints
    , setName
    , printRobot
    , RobotField
    ) where

-- types
data RobotField = String | Int
type RobotMessage = RobotField -> RobotField -> RobotField -> RobotField
type Robot = RobotMessage -> RobotField
type ARobot = RobotMessage -> RobotField

--functions (starts with lowercase)
--acts as a ~type. Actually a sort of constructor
robot :: RobotField -> RobotField -> RobotField -> RobotMessage -> RobotField
robot name strength points message = message name strength points

instance Show RobotField where
  show a = show a::String  -- Defined in ‘GHC.Show’

nameAccessor :: RobotMessage
nameAccessor  name _ _ = name
getName :: ARobot -> RobotField
getName aRobot = aRobot nameAccessor

strengthAccessor :: RobotMessage
strengthAccessor _ strength _ = strength
getStrength :: ARobot -> RobotField
getStrength aRobot = aRobot strengthAccessor

getPoints :: ARobot -> RobotField
getPoints aRobot = aRobot (\_ _ points -> points)

setName :: ARobot -> RobotField -> ARobot
setName aRobot newName = robot newName (getStrength aRobot) (getPoints aRobot)
-- setStrength =
-- setPoints =

printRobot aRobot = name ++  " attack:" ++ strength ++ " points:" ++ points
  where name = show (getName aRobot)
        strength = show (getStrength aRobot)
        points = show (getPoints aRobot)

