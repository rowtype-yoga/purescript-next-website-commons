module Commons where

import Prelude

import Control.Monad.Except (except)
import Data.Either (Either(..))
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Foreign (ForeignError(..))
import Record.Studio ((//))
import Yoga.JSON (class ReadForeign, class WriteForeign, writeImpl)
import Yoga.JSON as YogaJson

type Props =
  { header :: String
  , baseUrl :: String
  }

type DeclarationR =
  ( module :: String
  , title :: String
  , typeOrValue :: String
  , typeText :: Maybe String
  )

type PackageR = (deprecated :: Boolean)

type ModuleR = (module :: String)

data SearchInfo = Declaration { | DeclarationR } | Package { | PackageR } | Module { | ModuleR }

derive instance Generic SearchInfo _
instance Eq SearchInfo where
  eq = genericEq

instance Show SearchInfo where
  show = genericShow

instance ReadForeign SearchInfo where
  readImpl f = do
    { "type": t } :: { "type" :: String } <- YogaJson.readImpl f
    case t of
      "declaration" -> YogaJson.readImpl f <#> Declaration
      "package" -> YogaJson.readImpl f <#> Package
      "module" -> YogaJson.readImpl f <#> Module
      other -> except $ Left (pure $ ForeignError $ "Invalid search info type " <> other)

instance WriteForeign SearchInfo where
  writeImpl (Declaration r) = writeImpl $ { "type": "declaration" } // r
  writeImpl (Package p) = writeImpl $ { "type": "package" } // p
  writeImpl (Module m) = writeImpl $ { "type": "module" } // m

type SearchResult =
  { info :: SearchInfo
  , markup :: String
  , package :: String
  , text :: String
  , version :: String
  }

