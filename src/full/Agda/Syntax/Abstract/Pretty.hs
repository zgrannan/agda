
module Agda.Syntax.Abstract.Pretty where

import Agda.Syntax.Concrete.Pretty ()
import Agda.Syntax.Fixity
import Agda.Syntax.Translation.AbstractToConcrete
import Agda.TypeChecking.Monad
import Agda.Utils.Pretty

import Debug.Trace (trace)

showA :: (Show c, ToConcrete a c) => a -> TCM String
showA x = show <$> abstractToConcrete_ x

prettyA :: (Pretty c, ToConcrete a c) => a -> TCM Doc
prettyA x = pretty <$> abstractToConcrete_ x

prettyAs :: (Pretty c, ToConcrete a [c]) => a -> TCM Doc
prettyAs x = fsep . map pretty <$> abstractToConcrete_ x

-- | Variant of 'showA' which does not insert outermost parentheses.

showATop :: (Show c, ToConcrete a c) => a -> TCM String
showATop x = show <$> abstractToConcreteCtx TopCtx x

-- | Variant of 'prettyA' which does not insert outermost parentheses.

prettyATop :: (Show a, Pretty c, ToConcrete a c) => a -> TCM Doc
prettyATop x = trace ("Render " ++ show x) $ pretty <$> abstractToConcreteCtx TopCtx x
