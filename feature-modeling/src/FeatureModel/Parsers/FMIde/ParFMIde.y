-- This Happy file was machine-generated by the BNF converter
{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module FeatureModel.Parsers.FMIde.ParFMIde where
import FeatureModel.Parsers.FMIde.AbsFMIde
import FeatureModel.Parsers.FMIde.LexFMIde
import FeatureModel.Parsers.FMIde.ErrM

}

%name pGrammar Grammar
%name pProduction Production
%name pBaseProd BaseProd
%name pAltProd AltProd
%name pProdName ProdName
%name pTerm Term
%name pOption Option
%name pExpression Expression
%name pListProduction ListProduction
%name pListTerm ListTerm
%name pListOption ListOption
%name pListExpression ListExpression

-- no lexer declaration
%monad { Err } { thenM } { returnM }
%tokentype { Token }

%token
  '*' { PT _ (TS _ 1) }
  '+' { PT _ (TS _ 2) }
  ':' { PT _ (TS _ 3) }
  '::' { PT _ (TS _ 4) }
  ';' { PT _ (TS _ 5) }
  '[' { PT _ (TS _ 6) }
  ']' { PT _ (TS _ 7) }
  '_' { PT _ (TS _ 8) }
  'and' { PT _ (TS _ 9) }
  'implies' { PT _ (TS _ 10) }
  'not' { PT _ (TS _ 11) }
  'or' { PT _ (TS _ 12) }
  '|' { PT _ (TS _ 13) }

L_ident  { PT _ (TV $$) }


%%

Ident   :: { Ident }   : L_ident  { Ident $1 }

Grammar :: { Grammar }
Grammar : ListProduction ListExpression { TGrammar (reverse $1) $2 } 


Production :: { Production }
Production : BaseProd ListTerm '::' ProdName ';' { TBaseProduction $1 (reverse $2) $4 } 
  | AltProd ListOption ';' { TAltProduction $1 $2 }


BaseProd :: { BaseProd }
BaseProd : Ident ':' { TBaseProd $1 } 


AltProd :: { AltProd }
AltProd : Ident '|' { TAltProd $1 } 


ProdName :: { ProdName }
ProdName : Ident { TProdName $1 } 
  | '_' Ident { TProdNameL $2 }
  | Ident '_' { TProdNameR $1 }


Term :: { Term }
Term : Ident { TTerm $1 } 
  | '[' Ident ']' { TOptionalTerm $2 }
  | Ident '+' { TOrTerm $1 }
  | Ident '*' { TXorTerm $1 }


Option :: { Option }
Option : Ident { TOption $1 } 


Expression :: { Expression }
Expression : Ident { BasicExp $1 } 
  | Expression 'or' Expression { OrExp $1 $3 }
  | Expression 'and' Expression { AndExp $1 $3 }
  | 'not' Expression { NotExp $2 }
  | Expression 'implies' Expression { ImpliesExp $1 $3 }


ListProduction :: { [Production] }
ListProduction : {- empty -} { [] } 
  | ListProduction Production { flip (:) $1 $2 }


ListTerm :: { [Term] }
ListTerm : {- empty -} { [] } 
  | ListTerm Term { flip (:) $1 $2 }


ListOption :: { [Option] }
ListOption : {- empty -} { [] } 
  | Option { (:[]) $1 }
  | Option '|' ListOption { (:) $1 $3 }


ListExpression :: { [Expression] }
ListExpression : {- empty -} { [] } 
  | Expression { (:[]) $1 }
  | Expression ';' ListExpression { (:) $1 $3 }



{

returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++ 
  case ts of
    [] -> []
    [Err _] -> " due to lexer error"
    _ -> " before " ++ unwords (map (id . prToken) (take 4 ts))

myLexer = tokens
}
