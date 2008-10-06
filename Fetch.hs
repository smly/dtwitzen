module Fetch (
  fetchContents
) where

import Control.Arrow
import Control.Arrow.ArrowTree
import Text.XML.HXT.Arrow.ReadDocument
import Text.XML.HXT.Arrow.XmlArrow
import Text.XML.HXT.Arrow.XmlIOStateArrow
import Text.XML.HXT.DOM.XmlKeywords


fetchContents :: String -> IO (([String], [String], [String]))
fetchContents xmldata =
     do contents <- runX (readString [(a_validate, v_0)] xmldata >>> textTag)
        others   <- runX (readString [(a_validate, v_0)] xmldata >>> nameTag)
        statusId <- runX (readString [(a_validate, v_0)] xmldata >>> statTag)
        return (contents, others, statusId)
    where
      textTag = ( deep (isElem >>> hasName "text")
                  >>> getChildren >>> getText )
      nameTag = ( deep (isElem >>> hasName "screen_name")
                  >>> getChildren >>> getText )
      statTag = ( deep (isElem >>> hasName "status")
                  >>> getChildren
                  >>> isElem >>> hasName "id"
                  >>> getChildren >>> getText )
