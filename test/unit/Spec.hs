import qualified NNU.App.TwitterBotSpec        as TwitterBot
                                                ( spec )
import           NNU.Prelude
import           Test.Hspec

main :: IO ()
main = withResourceMap $ \resource -> do
  hspec $ do
    TwitterBot.spec resource
