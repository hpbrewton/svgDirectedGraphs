import SVG 


g = grouping 50 50 [rectangle 100 100, rectangle 200 200]

main :: IO ()
main = do
	putStrLn $ show $ svgHtml $ svg 500 500 [g]	

