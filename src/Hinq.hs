module Hinq where

import Control.Monad
import Control.Applicative

data Name = Name {
                    firstName::String
                 ,  lastName::String }

instance Show Name where
  show (Name first last) = mconcat [first, " ", last]

data GradeLevel = Freshman
  | Sophmore
  | Junior
  | Senior deriving (Eq,Ord,Enum,Show)

data Student = Student
  { studentId :: Int
  , gradeLevel :: GradeLevel
  , studentName :: Name } deriving Show

students :: [Student]
students = [(Student 1 Senior (Name "Audre" "Lorde"))
            ,(Student 2 Junior (Name "Leslie" "Silko"))
            ,(Student 3 Freshman (Name "Judith" "Butler"))
            ,(Student 4 Senior (Name "Guy" "Debord"))
            ,(Student 5 Sophmore (Name "Jean" "Baudrillard"))
            ,(Student 6 Junior (Name "Julia" "Kristeva"))]

data Teacher = Teacher
  { teacherId :: Int
  , teacherName :: Name } deriving Show

teachers :: [Teacher]
teachers = [Teacher 100 (Name "Simone" "De Beauvior")
            ,Teacher 200 (Name "Susan" "Sontag")]
   
data Course = Course
  { courseId :: Int
  , courseTitle :: String
  , teacher :: Int } deriving Show

courses :: [Course]
courses = [Course 101 "French" 100
          ,Course 201 "English" 200]
 
_from::a->a
_from = id

_select::Monad m => (a -> b) -> m a -> m b
_select = fmap -- how to polymorph this to accept touples of functions?

_where::(Monad m, Alternative m) => (a -> Bool) -> m a -> m a
-- _where = filter
_where p table = do
  te <- table
  guard (p te)
  return te 

_join::(Monad m, Alternative m,Eq c) => m a-> m b ->(a->c)->(b->c)-> m (a, b)
-- _join table1 table2 f1 f2 = fmap agg table1 where
--   agg aElem = (aElem, head (filter (\x -> (f1 aElem) == (f2 x)) table2))
_join table1 table2 f1 f2 = do
  t1e <- table1 
  t2e <- table2
  guard (f1 t1e == f2 t2e)
  return (t1e, t2e)

_hinq selClause joinClause whereClause = selClause $ whereClause $ joinClause

-- data HINQ m a b = HINQ _select _join _where | HINQ_ _select _join