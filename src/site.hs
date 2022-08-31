{-# language DeriveGeneric     #-}
{-# language FlexibleContexts  #-}
{-# language OverloadedStrings #-}
{-# language TupleSections     #-}

-- aeson
import           Data.Aeson                  (FromJSON, decode')

-- base
import           Control.Monad               (liftM)
import           Data.List                   (intercalate, isInfixOf, isPrefixOf, isSuffixOf, sort, sortBy)
import           Data.Maybe                  (fromJust, fromMaybe, isJust)
import           Data.Ord                    (comparing)
import           GHC.Generics                (Generic)

-- blaze-html
import           Text.Blaze.Html                 (toHtml, toValue, (!))
import           Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5                as H
import qualified Text.Blaze.Html5.Attributes     as A

-- containers
import qualified Data.Map as M

-- filepath
import           System.FilePath             (takeFileName)

-- hakyll
import           Hakyll

-- MissingH
import           Data.String.Utils           (strip)

-- string-conv
import           Data.String.Conv            (toS)

-- tagsoup
import           Text.HTML.TagSoup           (Tag (..))

-- time
import           Data.Time.Clock             (UTCTime (..))
import           Data.Time.Format            (parseTimeM)

-- time-locale-compat
import           Data.Time.Locale.Compat     (TimeLocale, defaultTimeLocale)

config :: Configuration
config = defaultConfiguration
    { ignoreFile = ignoreFile'
    }
  where
    ignoreFile' path
        | "."     `isPrefixOf` fileName = True
        | "#"     `isPrefixOf` fileName = True
        | "~"     `isSuffixOf` fileName = True
        | ".swp"  `isSuffixOf` fileName = True
        --
        -- For git annoyances related to zsh.
        --
        | "/.git/" `isInfixOf` path     = True
        | otherwise                     = False
      where
        fileName = takeFileName path


main :: IO ()
main = do
  commitDetails <- strip <$> readFile "metadata/gitinfo"
  imageMetaData <- computeImageMetaData

  hakyllWith config $ do
    match (fromList ["CNAME", "favicon.ico"]) $ do
      route idRoute
      compile copyFileCompiler

    match "templates/*" $ do
      route idRoute
      compile templateBodyCompiler

    match "images/**" $ do
      route idRoute
      compile copyFileCompiler

    match "css/*.css" $ do
      route idRoute
      compile copyFileCompiler

    match "css/*.hs" $ do
      -- See: https://jaspervdj.be/hakyll/tutorials/using-clay-with-hakyll.html
      route   $ setExtension "css"

      let cssStr = getResourceString >>= withItemBody (unixFilter "stack" ["runghc"])
      compile $ fmap compressCss <$>  cssStr


    -- ~ Normal updates
    match "updates/**.md" $ do
      route $ setExtension "html"
      tags <- buildTagsWith getTags "updates/**" (fromCapture "tags/*.html")

      let ctx =  constField "commit"  commitDetails
              <> bookContext

      compile $ getResourceBody
                  >>= renderPandoc
                  >>= loadAndApplyTemplate "templates/review.html"  ctx
                  >>= loadAndApplyTemplate "templates/default.html" ctx
                  >>= applyAsTemplate ctx
                  >>= lqipImages imageMetaData
                  >>= relativizeUrls

      tagsRules tags $ \tag pattern -> do
        route idRoute
        compile $ do
          -- books        <- recentFirst =<< loadAll pattern
          tagCloud     <- renderTagCloudWith makeLink (intercalate " ") 90 180 tags

          let tagCtx =  constField "tag"        tag
                     <> constField "tagCloud"   tagCloud
                     <> constField "commit"     commitDetails
                     <> bookContext

          makeItem ""
            >>= loadAndApplyTemplate "templates/tag.html"     tagCtx
            >>= loadAndApplyTemplate "templates/default.html" tagCtx
            >>= lqipImages imageMetaData
            >>= relativizeUrls

    match (fromList
            [ "about.md"
            ]) $ do
      route $ setExtension "html"
      compile $ do
        let ctx =  constField "commit" commitDetails
                <> bbContext

        getResourceBody
          >>= renderPandoc
          >>= applyAsTemplate ctx
          >>= loadAndApplyTemplate "templates/default.html" ctx
          >>= lqipImages imageMetaData
          >>= relativizeUrls


    match "project-selection-committee/*.md" $ do
      compile getResourceBody


    match "committee.md" $ do
      route $ setExtension "html"
      compile $ do
        members <- alphabetically =<< loadAll "project-selection-committee/*.md"

        let ctx =  listField "members" bbContext (return members)
                <> constField "commit" commitDetails
                <> bbContext

        getResourceBody
          >>= applyAsTemplate ctx
          >>= renderPandoc
          >>= loadAndApplyTemplate "templates/default.html" ctx
          >>= lqipImages imageMetaData
          >>= addOutImages
          >>= relativizeUrls


    match "index.md" $ do
      route $ setExtension "html"
      compile $ do
        -- updates <- recentFirst =<< loadAll "updates/**"
        -- shelfUpdates <- byIssueCreationTime =<< loadAll "shelf/*.md"

        let ctx =  constField "commit" commitDetails
                <> bbContext

        getResourceBody
          >>= renderPandoc
          >>= applyAsTemplate ctx
          >>= loadAndApplyTemplate "templates/default.html" ctx
          >>= lqipImages imageMetaData
          >>= addOutImages
          >>= relativizeUrls


byIssueCreationTime :: (MonadMetadata m) => [Item a] -> m [Item a]
byIssueCreationTime = liftM reverse . chronological'


-- TODO: Clean up these hacks.
chronological' :: (MonadMetadata m) => [Item a] -> m [Item a]
chronological' =
    sortByM $ getItemUTC' defaultTimeLocale . itemIdentifier

alphabetically :: (MonadMetadata m) => [Item a] -> m [Item a]
alphabetically = sortByM (pure . itemIdentifier)

sortByM :: (Monad m, Ord k) => (a -> m k) -> [a] -> m [a]
sortByM f xs = liftM (map fst . sortBy (comparing snd)) $
                mapM (\x -> liftM (x,) (f x)) xs

getItemUTC' :: (MonadMetadata m)
            => TimeLocale        -- ^ Output time locale
            -> Identifier        -- ^ Input page
            -> m UTCTime         -- ^ Parsed UTCTime
getItemUTC' locale id' = do
    metadata <- getMetadata id'
    let tryField k fmt = lookupString k metadata >>= parseTime' fmt
    return $ fromMaybe (error "Bad date") (tryField "issueCreatedAt" "%Y-%m-%dT%H:%M:%SZ")
  where
    parseTime' = parseTimeM True locale


makeLink :: Double
         -> Double
         -> String
         -> String
         -> Int
         -> Int
         -> Int
         -> String
makeLink minSize maxSize tag url count min' max' =
    let diff     = 1 + fromIntegral max' - fromIntegral min'
        relative = (fromIntegral count - fromIntegral min') / diff
        size     = floor $ minSize + relative * (maxSize - minSize) :: Int
    in renderHtml $
        H.a ! A.style (toValue $ "font-size: " ++ show size ++ "%")
            ! A.href (toValue url)
            ! A.class_ (toValue $ tag ++ " tag")
            $ toHtml tag


bookContext :: Context String
bookContext
  =  asList "authors"
  <> asList "tags"
  <> bbContext
    where
      asList fieldName
        = listFieldWith fieldName bbContext (\i -> do
            let identifier = itemIdentifier i
            metadata <- getMetadata identifier
            let metas = maybe [] id $ lookupStringList fieldName metadata
            return $ map (\x -> Item (fromFilePath x) x) (sort metas)
          )


bbContext :: Context String
bbContext =
  constField "rootUrl" "https://betweenbooks.com.au"
  <> dateField "date" "%B %e, %Y"
  <> defaultContext


lqipImages :: ImageMetaDataMap -> Item String -> Compiler (Item String)
-- No LQIP
-- lqipImages imageMetaData = return . fmap id
-- Full LQIP
lqipImages imageMetaData = return . fmap (withTags . switchInLqipImages $ imageMetaData)


addOutImages :: Item String -> Compiler (Item String)
addOutImages = return . fmap id


data ImageData = ImageData
  { base64String :: String
  , width        :: Int
  , height       :: Int
  , name         :: String
  } deriving (Generic)


instance FromJSON ImageData

type ImageMetaDataMap = M.Map String ImageData



computeImageMetaData :: IO (ImageMetaDataMap)
computeImageMetaData = do
  items <- lines <$> readFile "./metadata/images.jsonl"

  let decoded' :: [Maybe ImageData]
      decoded' = map (decode' . toS) items
      decoded  = map fromJust (filter isJust decoded')

  return $ M.fromList (map (\i -> (name i, i)) decoded)


-- TODO: Actually do something.
addOutImageInLinks :: Tag String -> Tag String
addOutImageInLinks t@(TagOpen "a" attrs) = t


switchInLqipImages :: ImageMetaDataMap -> Tag String -> Tag String
switchInLqipImages imageMetaDataMap t@(TagOpen "img" attrs) = newTag
  where
    doLqip      = True -- Could be condition on some class.
    -- classes     = splitOn " " (fromMaybe "" $ M.lookup "class" attrDict)
    attrDict    = M.fromList attrs
    nonSrcAttrs = [ (k, v) | (k, v) <- attrs, v /= "src" ]
    --
    src       = fromMaybe (error $ "No source for tag: " ++ show t) (M.lookup "src" attrDict)
    imageData = M.lookup (drop 1 src) imageMetaDataMap
    script    = ("onload", "this.src = '" ++ src ++ "'; this.onload = null;")
    --
    newAttrs  = (\d -> script : ("src", "data:image/png;base64," ++ base64String d) : nonSrcAttrs) <$> imageData
    newTag    = case newAttrs of
                  Nothing -> t
                  Just nt -> if doLqip then (TagOpen "img" nt) else t
switchInLqipImages _ t = t

