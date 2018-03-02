import XML

samp1 :: XML
samp1 = XML "p" [("size", "4"), ("shape", "3")] []

samp2 :: XML
samp2 = XML "h" [("dogs", "yes"), ("cats", "pls")] [samp1, samp1]

-- attrShow :: XML -> String
main :: IO ()
main = do
    putStrLn $ show samp2
