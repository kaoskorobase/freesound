{-# LANGUAGE CPP #-}
module Sound.Freesound.Search.Query (
    Query
  , include
  , exclude
  , (&)
) where

import           Data.Char (isSpace)
import           Data.Default (Default(..))
import           Data.String (IsString(..))
import           Data.Text (Text)
import qualified Data.Text as T
import           Network.HTTP.Types.QueryLike (QueryValueLike(..))

#if __GLASGOW_HASKELL__ < 710
import           Data.Monoid
#endif

data Term = Include Text | Exclude Text deriving (Eq, Show)

newtype Query = Query [Term] deriving (Eq, Show)

instance Monoid Query where
  mempty = Query []
  mappend (Query xs) (Query ys) = Query (xs ++ ys)

instance Default Query where
  def = mempty

instance IsString Query where
  fromString [] = mempty
  fromString s = include (T.pack s)

instance QueryValueLike Query where
  toQueryValue (Query ts) = toQueryValue $ T.unwords (map f ts)
    where
      quote t
        | T.any isSpace t = T.cons '"' (T.snoc t '"')
        | otherwise = t
      f (Include t) = T.cons '+' (quote t)
      f (Exclude t) = T.cons '-' (quote t)

include :: Text -> Query
include string = Query [Include string]

exclude :: Text -> Query
exclude string = Query [Exclude string]

(&) :: Query -> Query -> Query
(&) = mappend
