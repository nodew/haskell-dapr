module Dapr.Client.HttpClient.Metadata where

import Control.Monad.IO.Class (MonadIO)
import Dapr.Client.HttpClient.Req
import Dapr.Common
import Data.Bifunctor (bimap)
import Network.HTTP.Req

getMetadata :: MonadIO m => DaprConfig -> m (Either DaprClientError DaprMetadata)
getMetadata config = do
  response <- makeHttpRequest config GET ["metadata"] NoReqBody jsonResponse mempty
  return $ bimap DaprHttpException responseBody response
