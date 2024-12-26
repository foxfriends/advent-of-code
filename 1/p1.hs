import Data.List

main :: IO ()
main = do
    contents <- getContents
    let entries = fmap read (lines contents)
    case find (\x -> (2020 - x) `elem` entries) entries of
        Just answer -> print $ answer * (2020 - answer)
        Nothing -> return ()
