import Prelude              (IO, ($))
import System.Environment   (withArgs)
import Yesod.Default.Config (fromArgs)
import Yesod.Default.Main   (defaultMain)
import Settings             (parseExtra)
import Application          (makeApplication)

main :: IO ()
main =
  withArgs ["Development"] $
    defaultMain (fromArgs parseExtra) makeApplication
