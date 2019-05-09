module Parser where

import Data.Char

--https://eli.thegreenplace.net/2017/deciphering-haskells-applicative-and-monadic-parsers/

newtype Parser a = Parser (String -> [(a,String)])

parse (Parser p) = p 

instance Functor Parser where
    fmap g p = 
        Parser 
            (\inp -> case parse p inp of
                         []        -> []
                         [(v,out)] -> [(g v,out)])


instance Applicative Parser where  
    pure v = Parser (\cs -> [(v,cs)])  
    pg <*> px = Parser (\inp -> case parse pg inp of
                            []        -> []
                            [(g,out)] -> parse (fmap g px) out)  


instance Monad Parser where
    return v = Parser (\cs -> [(v,cs)])
    p >>= f  = Parser (\cs -> case (parse p) cs of [] -> []
                                                   [(v,out)] -> parse (f v) out)
    p >> q   = p >>= (\_ -> q)



item :: Parser Char
item = Parser (\cs -> case cs of [] -> []
                                 (x:xs) -> [(x,xs)])
failure :: Parser a
failure = Parser (\cs -> [])

sat :: (Char -> Bool) -> Parser Char
sat p = item >>= \x -> if p x then return x else failure

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = Parser (\inp -> case parse p inp of [] -> parse q inp
                                              [(v,out)] -> [(v,out)])
many :: Parser a -> Parser [a]
many p = many1 p +++ return []

many1 :: Parser a -> Parser [a]
many1 p = p >>= \v -> many p >>= \vs -> return (v:vs)

----------------------------------------------------------------------------------------------------

char :: Char -> Parser Char
char c = sat (== c)

string :: String -> Parser String
string []     = return []
string (c:cs) = char c >> string cs >> return (c:cs)

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char 
lower = sat isLower 

upper :: Parser Char 
upper = sat isUpper 

letter :: Parser Char 
letter = sat isAlpha 

alphanum :: Parser Char 
alphanum = sat isAlphaNum 

space :: Parser ()
space = do many (sat isSpace)
           return ()

token :: Parser a -> Parser a  
token p =
    do space 
       v <- p 
       space
       return v 

identifier :: Parser String 
identifier = token ident 
    where 
        ident = 
            do x <- lower 
               xs <- many alphanum
               return (x:xs)

identifierU :: Parser String
identifierU = token ident 
    where 
        ident = 
            do x <- upper 
               xs <- many alphanum
               return (x:xs)

natural :: Parser Int 
natural = token nat 
    where 
        nat =
            do xs <- many1 digit 
               return (read xs) 


symbol :: String -> Parser String 
symbol xs = token (string xs)




test = char '[' >> digit >>= \d -> many (char ',' >> digit) >>= \ds -> char ']' >> return ('(':d:ds++[')'])

--test2 = char '[' >> digit >>= \d -> many (char ',' >> many digit) >>= \ds -> char ']' >> return (d:ds)

----------------------------------------------------------------------------------------------------
ex = item >>= \x-> item >> item >>= \y -> return (x,y)
ex2 = item >>= (\x-> item >> (item >>= (\y -> return (x,y))))

f a b c = a+b+c
g = (\a ->(\b -> (\c -> a+b+c)))