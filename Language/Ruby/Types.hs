module Language.Ruby.Types where 


--http://www.hokstad.com/ruby-object-model
--
import Data.Map


type Identifier = String
type RValue = ()

type Flag = ()
type FlagValue ()

data Class = Class {
                      klass :: Identifier,
                      instance_vars :: Map Identifier RValue,
                      flags :: Map Flag FlagValue
                   }


