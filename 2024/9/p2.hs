import Data.Char

data Fs = File Int Int | Gap Int

seconds (x : _ : xs) = x : seconds xs
seconds x = x

move (File id len) = inserting
  where
    inserting (Gap l : rest)
      | len <= l = Gap 0 : File id len : Gap (l - len) : removing rest
      | otherwise = Gap l : inserting rest
    inserting (File i l : rest)
      | i == id = File id len : rest
      | otherwise = File i l : inserting rest
    removing (File i l : Gap m : rest)
      | id == i = Gap (l + m) : rest
      | otherwise = File i l : Gap m : removing rest
    removing [File i l]
      | id == i = []
      | otherwise = [File i l]

compact diskmap = foldr move diskmap $ seconds diskmap

toFs id [file] = [File id file]
toFs id (file : gap : rest) = File id file : Gap gap : toFs (id + 1) rest

checksum = checksum_ 0 0
  where
    checksum_ cs _ [] = cs
    checksum_ cs i (File id 0 : fs) = checksum_ cs i fs
    checksum_ cs i (File id len : fs) = checksum_ (cs + i * id) (i + 1) (File id (len - 1) : fs)
    checksum_ cs i (Gap n : fs) = checksum_ cs (i + n) fs

answer = checksum . compact . toFs 0 . fmap digitToInt . takeWhile isDigit

main = getContents >>= print . answer
