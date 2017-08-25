--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid (mappend)
import Prelude hiding ( div, span )
import Control.Monad (forM_)
import Hakyll
import Text.Blaze.Html5 as H hiding (main)
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Pretty      ( renderHtml )
import Data.List (sortBy,isSuffixOf)
import System.FilePath.Posix (takeBaseName,takeDirectory,(</>))
--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*.css" $ do
        route   idRoute
        compile compressCssCompiler

    match "css/main.sass" $ do
        route   $ setExtension "css"
        compile $ getResourceString >>=
            withItemBody (unixFilter "sass" ["-s"]) >>=
            return . fmap compressCss 

    -- I can't figure out how to get this done properly 
    -- match "layouts/*.hs" $ do
    --   route   idRoute
    --   compile $ getResourceString >>= withItemBody (unixFilter "runghc" [])

    match (fromList ["about.rst", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= applyTemplate defaultTemplate defaultContext
            >>= relativizeUrls

    match "test.md" $ do
        route   $ setExtension "html"
        compile $ do
            pandocCompiler >>= applyTemplate defaultTemplate postCtx

    match "cv.md" $ do
        route   $ gsubRoute ".md" (const "/index.html")
        compile $ do
            pandocCompiler >>= applyTemplate defaultTemplate postCtx

    match "posts/*" $ do
        route $ niceRoute
        compile $ pandocCompiler
            >>= applyTemplate postTemplate postCtx
            >>= applyTemplate defaultTemplate defaultContext
            >>= relativizeUrls
            >>= cleanIndexUrls

    create ["archive/index.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= applyTemplate postListTemplate archiveCtx
                >>= applyTemplate defaultTemplate defaultContext
                >>= relativizeUrls
                >>= cleanIndexUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Home"                `mappend`
                    defaultContext

            makeItem ""
                >>= applyTemplate postListTemplate indexCtx
                >>= applyTemplate defaultTemplate defaultContext
                >>= relativizeUrls
                >>= cleanIndexUrls

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------

-- This creates permalinks in the form /YYYY/MM/permalink-to-blog-post/index.html
-- Assuming that post filenames are in the form YYYY-MM-permalink-to-blog-post.md
niceRoute :: Routes
niceRoute = customRoute createIndexRoute
  where
    createIndexRoute ident = takeDirectory p
                                 </> year
                                 </> month
                                 </> drop 11 filename
                                 </> "index.html"
                           where
                             year = take 4 filename
                             month = take 2 $ drop 5 $ filename
                             filename = takeBaseName p
                             p = toFilePath ident

cleanIndexUrls :: Item String -> Compiler (Item String)
cleanIndexUrls = return . fmap (withUrls clean)
    where
        idx = "index.html"
        clean url
            | idx `isSuffixOf` url = take (length url - length idx) url
            | otherwise            = url

postCtx :: Context String
postCtx =
    dateField "date" "%0Y-%m-%d" `mappend`
    defaultContext

defaultTemplate :: Template
defaultTemplate = readTemplate . renderHtml $ defaultTemplateRaw

defaultTemplateRaw :: Html
defaultTemplateRaw = html $ do
    H.head $ do
        meta ! httpEquiv "Content-Type" ! content "text/html; charset=UTF-8"
        H.title "My Hakyll Blog - $title$"
        link ! rel "stylesheet" ! type_ "text/css" ! href "/css/main.css"
    body $ do
        H.div ! A.id "header" $ do
            H.div ! A.id "logo" $ a ! href "/" $ "My Hakyll Blog"
            H.div ! A.id "navigation" $ do
                a ! href "/" $ "Home"
                a ! href "/about" $ "About"
                a ! href "/cv" $ "CV"
                a ! href "/archive" $ "Archive"
        H.div ! A.id "content" $ do
            h1 "$title$"
            "$body$"
        H.div ! A.id "footer" $ do
            "Site proudly generated by "
            a ! href "http://jaspervdj.be/hakyll" $ "Hakyll"

postListTemplate :: Template
postListTemplate = readTemplate . renderHtml $ postListTemplateRaw

postListTemplateRaw :: Html
postListTemplateRaw =
  ul $ do
    "$for(posts)$"
    li $ do
        a ! href "$url$" $ "$title$"
        "- $date$"
    "$endfor$"

postTemplate :: Template
postTemplate = readTemplate . renderHtml $ postTemplateRaw

postTemplateRaw :: Html
postTemplateRaw = H.div ! class_ "info" $ do
  "Posted on $date$\n"
  "$body$"
