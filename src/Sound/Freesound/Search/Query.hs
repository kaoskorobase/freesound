module Sound.Freesound.Search.Query (
    Query
  , include
  , exclude
  , (&)
) where

import			 Data.Char (isSpace)
import 			 Data.Text (Text)
import qualified Data.Text as Text
import 			 Network.HTTP.Types.QueryLike (QueryValueLike(..))

newtype Query = Query [Term] deriving (Eq, Show)

instance QueryValueLike Query where
	toQueryValue (Query ts) = toQueryValue $ Text.unwords (map f ts)
		where
			quote t
				| Text.any isSpace t = Text.cons '"' (Text.snoc t '"')
				| otherwise = t
			f (Include t) = Text.cons '+' (quote t)
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
