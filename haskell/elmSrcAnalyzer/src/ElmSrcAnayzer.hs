{-# LANGUAGE DeriveGeneric #-}

module ElmSrcAnayzer where

import GHC.Generics
import System.Directory
import Data.List
import Control.Monad
import Data.Aeson
import Parser
-------------------------------------------------------------------------------

data ModuleInfo = 
    ModuleInfo
        { nbrLoc :: Int 
        , imports :: [Import]
        , exports :: Export
        , modName :: String 
        } deriving (Generic, Show)

data Import = 
    Import 
        { impName :: String 
        , impTypes :: [String]
        , impFun :: [String] 
        } deriving (Generic, Show)

data Export = 
    Export 
        { expTypes :: [String]
        , expFun :: [String] 
        } deriving (Generic, Show)


data ElmId = ElmType String | ElmFun String deriving (Generic, Show)

instance ToJSON ModuleInfo where
    toEncoding = genericToEncoding defaultOptions

instance ToJSON Import where
    toEncoding = genericToEncoding defaultOptions

instance ToJSON Export where
    toEncoding = genericToEncoding defaultOptions


processFiles :: IO ([ModuleInfo])
processFiles = 
    let go filename =
            do b <- doesFileExist filename
               if b then 
                   do putStrLn $ "Processing module: " ++ filename
                      processFile filename
               else
                   do ctns <- listDirectory filename
                      res <- mapM go (map (\c -> filename ++ "/" ++ c) ctns) 
                      return $ concat res    
    in 
        go "./src"


processFile :: FilePath -> IO [ModuleInfo]
processFile filename = 
    do content <- readFile filename 
       return $ extractModuleInfo content
        

extractModuleInfo :: String -> [ModuleInfo]
extractModuleInfo content = 
    let loc = length . lines $ content
        
        mainParser =
            do (mn, exps) <- moduleDeclParser
               imps <- many importDeclParser
               return $ ModuleInfo loc imps exps mn 

    in case parse mainParser content of 
           [(modInfo,_)] -> [modInfo] 
           _ -> [] 

moduleDeclParser :: Parser (String, Export)
moduleDeclParser = 
    do symbol "port" +++ return []
       symbol "module"
       mn <- token modNameParser
       (ts, fs) <- (importParser +++ return ([],[]))
       return $ (mn, Export ts fs)



importDeclParser :: Parser Import
importDeclParser = 
    do symbol "import"
       mn <- token modNameParser
       (symbol "as" >> identifierU) +++ return []
       (ts, fs) <- (importParser +++ return ([],[]))
       return $ Import mn ts fs

modNameParser :: Parser String
modNameParser = 
    let modName = identifierU
    in 
        do x  <- modName
           xs <- many (char '.' 
                          >> modName 
                             >>= \m -> return $ '.':m
                      )
           return (concat $ x:xs)

importParser :: Parser ([String], [String])
importParser = 
    (symbol "exposing (..)" >> return ([],[])) +++
        do symbol "exposing"
           symbol "("
           ids <- many1 
                    ( elmIdParser 
                        >>= \id -> 
                            (symbol "," +++ symbol ")"
                                 >> return id
                            )
                    )
           return (splitElmIds ids)   

elmIdParser :: Parser ElmId 
elmIdParser = 
    let parseConstructors = 
            do symbol "("
               many1 (sat (/= ')'))
               symbol ")"
               return []
    in 
    
    do id <- identifierU 
       parseConstructors +++ (return [])
       return $ ElmType id
    
    +++ 
        (identifier >>= \id -> return $ ElmFun id)

splitElmIds :: [ElmId] -> ([String], [String])
splitElmIds = 
    let go (ts,fs) xs =
            case xs of 
                [] -> 
                    (reverse ts, reverse fs)
                (ElmType s : ys) -> 
                    go (s : ts, fs) ys 
                (ElmFun s : ys) ->  
                    go (ts, s : fs) ys
    in go ([],[]) 

(|>) = flip ($)

-------------------------------------------------------------------------------

elmSrcAnayzer :: IO ()
elmSrcAnayzer = 
    do res <- processFiles
       encodeFile "./projectInfo" res
