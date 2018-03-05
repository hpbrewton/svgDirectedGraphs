-- this is just a small xml marshaller

module XML 
( XML (..)
, Attr
) where 

import Data.List

type Attr = (String, String)
data XML = XML { tag      :: String
               , attrs    :: [Attr]
               , children :: [XML]    
               }
	 | Str String

alternate :: [a] -> [a] -> [a]
alternate []     _      = []
alternate _      []     = []
alternate (x:xs) (y:ys) = x:y: alternate xs ys

showAux :: Int -> XML -> String
showAux indents (Str string) = (replicate indents '\t') ++ string ++ "\n"
showAux indents xml = case children xml of
				[] -> heading'
				_  -> heading ++ childrenstring ++ ending
        where
            showattr (key, value) = key ++ "=\"" ++ value ++ "\""
            attrShow xml          = ' ' : (concat $ intersperse " " (fmap showattr $ attrs xml)) 
            heading               = (replicate indents '\t') ++ "<" ++tag xml++attrShow xml++">\n"
            heading'              = (replicate indents '\t') ++ "<" ++tag xml++attrShow xml++"/>\n"
            ending                = (replicate indents '\t') ++ "</"++tag xml++">\n"
            childrenstring        = concatMap (showAux (indents+1)) $ children xml

instance Show XML where
    show = showAux 0

