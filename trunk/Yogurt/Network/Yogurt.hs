-- | Provides Yogurt's version number and re-exports the other modules.
module Network.Yogurt (
  version,
  module Network.Yogurt.Mud,
  module Network.Yogurt.Session,
  module Network.Yogurt.Utils,
  ) where

import Network.Yogurt.Mud
import Network.Yogurt.Session
import Network.Yogurt.Utils
import Data.Version
import qualified Paths_Yogurt as P

-- | The version number of this version of the library.
version :: Version
version = P.version
