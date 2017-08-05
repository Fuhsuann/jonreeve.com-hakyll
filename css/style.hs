{-# LANGUAGE OverloadedStrings #-}
import Clay
import Prelude hiding (div)

test :: Css
test = do
  body ?
       do backgroundColor    white
          fontSize           (px 16)
          width              (px 600)
  div # "#header" ?
       do borderBottom       solid (px 2) black

main :: IO ()
main = putCss test
