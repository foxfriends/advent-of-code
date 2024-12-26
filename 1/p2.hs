main :: IO ()
main = do
    contents <- getContents
    let entries = fmap read (lines contents)
    let (a, b, c) = head [(x, y, z) | x <- entries, y <- entries, x + y <= 2020, z <- entries, x + y + z == 2020]
    print $ a * b * c
