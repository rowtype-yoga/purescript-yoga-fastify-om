module TestVTA where

import Type.Proxy (Proxy(..))
import Data.Symbol (class IsSymbol, reflectSymbol)

test :: forall @sym. IsSymbol sym => String
test = reflectSymbol (Proxy :: Proxy sym)

result :: String
result = test @"hello"
