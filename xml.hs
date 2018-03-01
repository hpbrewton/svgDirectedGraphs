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

instance Show XML where
	show xml = "<" ++ tag xml  ++ attrShow xml ++ ">\n" ++ childrenstring ++ ending 
		where
			ending         = "</"++tag xml++">\n"
			childrenstring = concatMap (\x -> "\t" ++x) $ (map show $ children xml)

