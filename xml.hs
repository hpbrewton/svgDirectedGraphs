module XML 
( XML (..)
, Attr
, attrShow
) where 

-- this is just a small xml marshaller

type Attr = (String, String)

data XML = XML { tag      :: String
               , attrs    :: [Attr]
	       , children :: [XML]	
	       }

alternate :: [a] -> [a] -> [a]
alternate []     _      = []
alternate _      []     = []
alternate (x:xs) (y:ys) = x:y: alternate xs ys

spaces = repeat " "

showattr :: Attr -> String
showattr (key, value) = key ++ "=\"" ++ value ++ "\""

attrShow :: XML -> String
attrShow xml = concat $ alternate spaces (fmap showattr $ attrs xml) 

showAux :: Int -> XML -> String
showAux indents xml = heading ++ childrenstring ++ ending
		where
			heading        = (replicate indents '\t') ++ "<" ++tag xml++attrShow xml++">\n"
			ending         = (replicate indents '\t') ++ "</"++tag xml++">\n"
			childrenstring = concatMap (showAux (indents+1)) $ children xml

instance Show XML where
	show = showAux 0

