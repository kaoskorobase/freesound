module Sound.Freesound.Sound.Search.Query (
	ToQueryString(..)
  , Query
  , include
  , exclude
  , append
  , (&)
) where

import			 Data.Char (isSpace)
import 			 Data.Text (Text)
import qualified Data.Text as Text
import 			 Sound.Freesound.URI (ToQueryString(..))

newtype Query = Query [Term] deriving (Eq, Show)

instance ToQueryString Query where
	toQueryString (Query ts) = toQueryString $ Text.unwords (map f ts)
		where
			quote t
				| Text.any isSpace t = Text.cons '"' (Text.snoc t '"')
				| otherwise = t
			f (Include t) = quote t
			f (Exclude t) = Text.cons '-' (quote t)

data Term = Include Text | Exclude Text deriving (Eq, Show)

include :: Text -> Query
include string = Query [Include string]

exclude :: Text -> Query
exclude string = Query [Exclude string]

append :: Query -> Query -> Query
append (Query xs) (Query ys) = Query (xs ++ ys)

(&) :: Query -> Query -> Query
(&) = append
