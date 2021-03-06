-- This Happy file was machine-generated by the BNF converter
{
{-# OPTIONS -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module UseCaseModel.Parsers.BNFC.ParUCM where
import UseCaseModel.Parsers.BNFC.AbsUCM
import UseCaseModel.Parsers.BNFC.LexUCM
import UseCaseModel.Parsers.BNFC.ErrM
}

%name pAdvice Advice
%name pAdviceDec AdviceDec
%name pAdvId AdvId
%name pAdvDesc AdvDesc
%name pFlow Flow
%name pStep Step
%name pAction Action
%name pSystem System
%name pResponse Response
%name pStepId StepId
%name pListStep ListStep

-- no lexer declaration
%monad { Err } { thenM } { returnM }
%tokentype { Token }

%token 
 '{' { PT _ (TS "{") }
 ';' { PT _ (TS ";") }
 '}' { PT _ (TS "}") }
 ':' { PT _ (TS ":") }
 '(' { PT _ (TS "(") }
 ',' { PT _ (TS ",") }
 ')' { PT _ (TS ")") }
 'Proceed' { PT _ (TS "Proceed") }
 'Step' { PT _ (TS "Step") }
 'advice' { PT _ (TS "advice") }
 'after' { PT _ (TS "after") }
 'around' { PT _ (TS "around") }
 'before' { PT _ (TS "before") }
 'description' { PT _ (TS "description") }
 'id' { PT _ (TS "id") }

L_ident  { PT _ (TV $$) }
L_quoted { PT _ (TL $$) }
L_err    { _ }


%%

Ident   :: { Ident }   : L_ident  { Ident $1 }
String  :: { String }  : L_quoted { $1 }

Advice :: { Advice }
Advice : 'before' AdviceDec { TBeforeAdvice $2 } 
  | 'after' AdviceDec { TAfterAdvice $2 }
  | 'around' AdviceDec { TAroundAdvice $2 }


AdviceDec :: { AdviceDec }
AdviceDec : 'advice' '{' AdvId ';' AdvDesc ';' Flow '}' { TAdviceDec $3 $5 $7 } 


AdvId :: { AdvId }
AdvId : 'id' ':' Ident { TAdvId $3 } 


AdvDesc :: { AdvDesc }
AdvDesc : 'description' ':' String { TAdvDesc $3 } 


Flow :: { Flow }
Flow : ListStep { TFlow $1 } 


Step :: { Step }
Step : 'Step' '(' StepId ',' Action ',' System ',' Response ')' { TBasicStep $3 $5 $7 $9 } 
  | 'Proceed' { TProceed }


Action :: { Action }
Action : String { TAction $1 } 


System :: { System }
System : String { TSystem $1 } 


Response :: { Response }
Response : String { TResponse $1 } 


StepId :: { StepId }
StepId : Ident { TStepId $1 } 


ListStep :: { [Step] }
ListStep : {- empty -} { [] } 
  | Step { (:[]) $1 }
  | Step ';' ListStep { (:) $1 $3 }



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
    _ -> " before " ++ unwords (map prToken (take 4 ts))

myLexer = tokens
}

