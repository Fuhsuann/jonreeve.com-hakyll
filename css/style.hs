{-# LANGUAGE OverloadedStrings #-}
import           Clay

test :: Css
test = body ?
       do backgroundColor    blue

main :: IO ()
main = putCss test
