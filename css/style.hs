{-# LANGUAGE OverloadedStrings #-}
import Clay
import Prelude hiding (div, rem)

test :: Css
test = do
  body ?
    do backgroundColor    blue
       fontSize           (px 16)
  div # "#social" ?
    do display        flex
       alignItems     center
       justifyContent flexEnd
       i ? do
         fontSize (rem 2)
         -- margin   (em 1)

main :: IO ()
main = putCss test 
