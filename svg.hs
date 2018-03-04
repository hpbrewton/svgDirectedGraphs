module SVG (
    grouping,
	rectangle,
	svg,
    svgHtml
) where

import XML

-- | x position, y position, children 
grouping :: Int -> Int -> [XML] -> XML
grouping x y = XML "g" [("transform","translate("++show x++" "++show y++")")] 

-- | x position, y position, w(idth), h(eight)
rectangle :: Int -> Int -> XML
rectangle w h = XML "rect" [("width", show w), ("height", show h)] []

-- | x position, y position, children 
svg :: Int -> Int -> [XML] -> XML
svg w h = XML "svg" [("width", show w), ("height", show h)]

-- | package in html
svgHtml :: XML -> XML
svgHtml = XML "html" [] . return 
