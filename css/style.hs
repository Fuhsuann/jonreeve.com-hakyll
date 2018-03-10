{-# LANGUAGE OverloadedStrings #-}
import Clay
import Prelude hiding (div, rem)

test :: Css
test = do
  body ?
    fontSize (pct 150)
  figure ? do
    textAlign center
    img ? do
      maxWidth (pct 100)
    figcaption ? do
      fontSize (pct 90)
  div # "#content" ?
    sym2 padding (rem 2) (rem 2)
  section # ".info" ?
    sym margin (rem 1)
  footer ? do
    sym padding (rem 3)
    fontSize (rem 1.1)
    div # "#social" ?
      do display        flex
         alignItems     center
         justifyContent flexEnd
         i ? do
           fontSize (rem 2)
           -- `sym` is shorthand and allows for `margin 1 1 1 1`
           sym margin (em 1)

main :: IO ()
main = putCss test 
