module App.MitoSchedule where

import           RIO
import qualified RIO.List            as L
import qualified RIO.List.Partial    as L
import           RIO.Char            (isSpace)
import           Text.HTML.Scalpel   (Scraper, scrapeStringLike, chroot, chroots, hasClass, text, (@:), (@=))
import           System.Process      (readProcess)
import qualified Web.Twitter.Conduit as Tw

import           Util
import           Web.Twitter.API     as TwitterApi

app :: forall m. MonadIO m => m ()
app = do
    state <- newTVarIO Nothing
    twitterApi <- TwitterApi.defaultImpl <$> twConfigFromEnv "MS"
    runRIO Env{twitterApi} $ loopWithDelaySec 60 $ printAnyError $ getSchedule >>= \case
      Nothing -> notifyHogeyamaSlack "MitoSchedule failed"
      Just m  -> do
        old <- readTVarIO state
        atomically $ writeTVar state (Just m)
        when (isJust old && old /= Just m) $ do
          writeTextToFile m "/tmp/a.png"
          r <- tryAny $ call' @Tweet $ Tw.updateWithMedia "🐰" (Tw.MediaFromFile "/tmp/a.png")
          case r of
            Left e -> do notifyHogeyamaSlack $ fromString $ mconcat
                          [ "twitter error: ", show e, "\n"
                          , "schedule: ", m
                          ]
            Right _ -> pure ()
newtype Env = Env { twitterApi :: TwitterApi (RIO Env) }
  deriving stock (Generic)
instance HasTwitterAPI (RIO Env) Env where
  twitterApiL = #twitterApi

writeTextToFile :: MonadIO m => String -> FilePath -> m ()
writeTextToFile contents png = do
    print args
    void $ liftIO $ readProcess "convert" args ""
  where
    args =
      [ "-font", "/usr/share/fonts/opentype/ipaexfont-gothic/ipaexg.ttf" -- XXX 決め打ち
      , "-pointsize", "24"
      , "label:"<>contents<>""
      , png
      ]
-- App.MitoSchedule.writeTextToFile "ほげ\nふが" "a.png"
-- convert
--  -font /usr/share/fonts/opentype/ipaexfont-gothic/ipaexg.ttf
--  -pointsize 24
--  "label:ほ げ\nふが" a.png

scrapeMottoSelfIntro :: Scraper String (Maybe String)
scrapeMottoSelfIntro =
  chroot ("div" @: ["id" @= "container"]) $
  chroot ("div" @: [hasClass "tabs"]) $
  chroot ("div" @: ["id" @= "profile", hasClass "tab-panel"]) $
  fmap L.headMaybe $ chroots ("div" @: [hasClass "box"]) $ do
    header <- text "h2"
    if header == "もっと自己紹介"
    then text ("div" @: [hasClass "pad"])
    else fail "もっと自己紹介 not found"

stripSchedule :: String -> String
stripSchedule =
    lines
    >>> map (dropWhile isSpace)
    >>> dropWhile (\x -> not $ "【" `L.isPrefixOf` x && "スケジュール" `L.isInfixOf` x)
    >>> onCons (takeWhile (\x -> not $ "【" `L.isPrefixOf` x))
    >>> filter (not . null)
    >>> unlines
    >>> removeLastNL
  where
    onCons _ [] = []
    onCons f (x:xs) = x : f xs
    removeLastNL [] = []
    removeLastNL x  = L.init x

getSchedule :: MonadIO m => m (Maybe String)
getSchedule = liftIO $ do
  html' <- readProcess "curl" ["https://twpf.jp/MitoTsukino"] ""
  let mottoSelfIntro = join $ scrapeStringLike html' scrapeMottoSelfIntro
      schedule       = stripSchedule <$> mottoSelfIntro
  case schedule of
    Nothing -> return Nothing
    Just "" -> return Nothing
    Just m  -> return (Just m)

