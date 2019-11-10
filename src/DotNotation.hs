module DotNotation where

data Grade = F | D | C | B | A deriving (Eq, Ord, Enum, Show, Read)
data Degree = HS | BA | MS | PhD deriving (Eq, Ord, Enum, Show, Read)
data Candidate = Candidate
    { candidateId :: Int
    , codeReview :: Grade
    , cultureFit :: Grade
    , education :: Degree } deriving Show

viable :: Candidate -> Bool
viable candidate = all (== True) tests
    where passedCoding = codeReview candidate > B
          passedCultureFit = cultureFit candidate > C
          educationMin = education candidate >= MS
          tests = [passedCoding
                  ,passedCultureFit
                  ,educationMin]
   
_read::Read a => String -> IO a
_read text = print text >> getLine >>= return . read 

readCandidate::IO Candidate
readCandidate = pure Candidate <*> (_read "Id") <*> (_read "Review") <*> (_read "Fit") <*> (_read "Education")  

assessCandidateIO :: IO String
assessCandidateIO = readCandidate >>= return . (\x -> if x then "passed" else "failed") . viable
  