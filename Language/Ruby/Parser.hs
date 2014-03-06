{-# LANGUAGE OverloadedStrings #-}

module Language.Ruby.Parser where

--import Data.Attoparsec
import Control.Applicative
import Control.Monad
import Data.Attoparsec.Combinator
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString as BS
import qualified Data.Text as T


data SExp =   Symbol T.Text
            | String T.Text
            | Nil
            | Number Number
            | SExp SExp [SExp] deriving Show


sexp :: Parser SExp
sexp = do
          _ <- string "s("
          exps <- (sym <|> num_lit <|> string_lit <|> nil <|> sexp) `sepBy` (skipSpace *> char ',' *> skipSpace)
          _ <- string ")"
          return $ SExp (head exps) (tail exps) 

variable :: Parser T.Text
variable = do
              -- s <-  satisfy (inClass "a-z")
              ss <- many' (satisfy (inClass "_a-zA-Z0-9-+/*@$"))
              return $ T.pack (ss)

nil :: Parser SExp
nil = do
        _ <- string "nil"
        return Nil

sym :: Parser SExp
sym = do
            _ <- char ':'
            ss <- variable
            return $ Symbol (':' `T.cons` ss)

string_lit :: Parser SExp
string_lit = do
                _ <- char '"'
                str <- many (string_escaped_chars <|> (liftA (\x -> [x]) (notChar '"')))
                _ <- char '"'
                return $ String $ T.pack $ concat str

num_lit :: Parser SExp
num_lit = liftA Number number
            
            
string_escaped_chars :: Parser String 
string_escaped_chars = do
                      _ <- char '\\'
                      esc <- ( 
                               liftA (\x -> [x]) (satisfy (inClass "\\\"nsrtvfbae")) <|>
                               replicateM 3 (digit)                              <|> --octal
                               (do --hex literal
                                  _ <- char 'x'
                                  digits <- replicateM 2 (digit)
                                  return ('x' : digits))                         <|>
                               (do --unicode literal
                                  _ <- char 'u'
                                  digits <- replicateM 4 (digit)
                                  return ('u' : digits))                         <|>
                               (do --control
                                  _ <- char 'c'
                                  c <- anyChar
                                  return ('c' : [c]))                            <|>
                               (do --control
                                  _ <- char 'C'
                                  _ <- char '-'
                                  c <- anyChar
                                  return ('C': '-' : [c]))                       <|>
                               (do --meta
                                  _ <- char 'M'
                                  _ <- char '-'
                                  c <- anyChar
                                  return ('M': '-' : [c]))                       <|>
                               (do --meta
                                  _ <- char 'M'
                                  _ <- char '-'
                                  _ <- char '\\'
                                  _ <- char 'C'
                                  _ <- char '-'
                                  c <- anyChar
                                  return ('M': '-' : '\\' : 'C' : '-' : [c]))    <|>
                               liftA (\x -> [x]) anyChar
                             )
                      return ('\\' : esc)
