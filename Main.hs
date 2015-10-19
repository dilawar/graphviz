
import Text.Parsec
import qualified Text.Parsec.Token as P
import qualified Text.Parsec.Language as L

-- 

data Node = Node { nodeName :: String 
    , nAttr :: [ String ]
} deriving Show

data Edge = Edge { src :: Node 
    , tgt :: Node 
    , eAttr :: [ String ]
} deriving Show

data DotGraph = DOT { gName :: String 
    , gType :: String
    , nodes :: [ Node ]
    , edges :: [ Edge ]
} deriving Show

-- The parser

dotParser = do 
    gName <- graphType 
    gType <- graphName 
    (string "{")
    return $ DOT gName gType [] [] 

graphType = do 
    gType <- (try (string "graph") <|> string "digraph") 
    whiteSpace
    return gType

graphName = identifier

-- the lexer and language. reservedNames etc are declared in Token
dotLanguage = L.javaStyle { 
    P.reservedNames = [ "graph", "digraph"] 
    , P.caseSensitive = True
    }

lexer = P.makeTokenParser dotLanguage
identifier = P.identifier lexer
whiteSpace = P.whiteSpace lexer

parseDot :: String -> Either ParseError DotGraph
parseDot input = parse dotParser "(unkown)" input

{-
run p input = case (parse p "(unknown)" input) of 
    Left err -> do { putStrLn "parse error at "; print err }
    Right x -> print x
-}
    
