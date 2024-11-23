module Language where

import Data.Char
import Data.List

type Const = Integer

type Var = Char

class Pretty a where
  pretty :: a -> String

data Bop = Badd | Bsub | Bmul
  deriving (Eq, Show)

instance Pretty Bop where
  pretty Badd = "+"
  pretty Bsub = "-"
  pretty Bmul = "*"

instance Read Bop where
  readsPrec _ ('+' : s) = [(Badd, s)]
  readsPrec _ ('-' : s) = [(Bsub, s)]
  readsPrec _ ('*' : s) = [(Bmul, s)]

data Rel = Rle | Rgt | Rleq | Rgeq
  deriving (Eq, Show)

instance Pretty Rel where
  pretty Rle = "<"
  pretty Rgt = ">"
  pretty Rleq = "<="
  pretty Rgeq = ">="

instance HasNegation Rel where
  negate Rle = Rgt
  negate Rgt = Rle
  negate Rleq = Rgeq
  negate Rgeq = Rleq

instance Read Rel where
  readsPrec _ ('<' : '=' : s) = [(Rleq, s)]
  readsPrec _ ('>' : '=' : s) = [(Rgeq, s)]
  readsPrec _ ('<' : s) = [(Rle, s)]
  readsPrec _ ('>' : s) = [(Rgt, s)]

data Expr
  = Econst Const
  | Evar Var
  | Ebop Bop Expr Expr
  deriving (Eq, Show)

instance Pretty Expr where
  pretty (Ebop op e1 e2) = "(" ++ pretty e1 ++ " " ++ pretty op ++ " " ++ pretty e2 ++ ")"
  pretty (Econst c) = show c
  pretty (Evar v) = [v]

instance Read Expr where
  readsPrec _ s =
    let s' = filter (not . isSpace) s
     in [parseExpr s']

parseExpr :: String -> (Expr, String)
parseExpr str@(c : s)
  | c == '(' =
      let (e1, s1) = parseExpr s
          (op, s2) = head (reads s1) :: (Bop, String)
          (e2, s3) = parseExpr s2
       in (Ebop op e1 e2, tail s3)
  | isAlpha c = (Evar c, s)
  | otherwise =
      let (c, s') = head (reads str) :: (Const, String)
       in (Econst c, s')
parseExpr s = error ("parse error: " ++ s)

type Condition = (Rel, Var, Const)

data Command
  = Cskip
  | Cseq Command Command
  | Cassign Var Expr
  | Cinput Var
  | Cif Condition Command Command
  | Cwhile Condition Command
  deriving (Eq, Show)

instance Pretty Command where
  pretty c = pretty_tabbed 0 c
    where
      pretty_tabbed :: Int -> Command -> String
      pretty_tabbed n Cskip = replicate n ' ' ++ "skip;\n"
      pretty_tabbed n (Cseq c1 c2) = pretty_tabbed n c1 ++ pretty_tabbed n c2
      pretty_tabbed n (Cassign v e) = replicate n ' ' ++ v : " := " ++ pretty e ++ ";\n"
      pretty_tabbed n (Cinput v) = replicate n ' ' ++ "input " ++ [v] ++ ";\n"
      pretty_tabbed n (Cif (r, v, c) c1 c2) =
        let if_str = replicate n ' ' ++ "if " ++ v : " " ++ pretty r ++ " " ++ show c
            then_str = " {\n" ++ pretty_tabbed (n + 2) c1
            else_str = replicate n ' ' ++ "} else {\n" ++ pretty_tabbed (n + 2) c2
         in if_str ++ then_str ++ else_str ++ replicate n ' ' ++ "}\n"
      pretty_tabbed n (Cwhile (r, v, c) c1) =
        let while_str = replicate n ' ' ++ "while " ++ v : " " ++ pretty r ++ " " ++ show c
            do_str = pretty_tabbed (n + 2) c1
         in while_str ++ " {\n" ++ do_str ++ replicate n ' ' ++ "}\n"

instance Read Command where
  readsPrec _ s =
    let s' = filter (not . isSpace) s
     in [parseCommand s']

parseCondition :: String -> (Rel, Var, Const, String)
parseCondition (v : s) =
  let (r, s2) = head (reads s) :: (Rel, String)
      (c, s3) = head (reads s2) :: (Const, String)
   in (r, v, c, s3)

parseCommand :: String -> (Command, String)
parseCommand s
  | "skip" `isPrefixOf` s = chainCommands Cskip (parseCommand (drop 4 s))
  | "input" `isPrefixOf` s = chainCommands (Cinput (s !! 5)) (parseCommand (drop 6 s))
  | ":=" `isPrefixOf` (drop 1 s) =
      let (e, s') = parseExpr (drop 3 s)
       in chainCommands (Cassign (head s) e) (parseCommand s')
  | "if" `isPrefixOf` s =
      let (r, v, c, s1) = parseCondition (drop 2 s)
          (c1, s2) = parseCommand s1
          (c2, s3) = parseCommand s2
       in chainCommands (Cif (r, v, c) c1 c2) (parseCommand s3)
  | "while" `isPrefixOf` s =
      let (r, v, c, s1) = parseCondition (drop 5 s)
          (c1, s2) = parseCommand s1
       in chainCommands (Cwhile (r, v, c) c1) (parseCommand s2)
  | "else" `isPrefixOf` s = parseCommand (drop 4 s)
  | ";" `isPrefixOf` s = parseCommand (drop 1 s)
  | "{" `isPrefixOf` s = parseCommand (drop 1 s)
  | "}" `isPrefixOf` s = (Cskip, drop 1 s)
  | otherwise = (Cskip, s)
  where
    chainCommands :: Command -> (Command, String) -> (Command, String)
    chainCommands c (Cskip, s) = (c, s)
    chainCommands c (c2, s) = (Cseq c c2, s)
