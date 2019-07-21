-- Assignment 5, 159.202, 2018 S2
-- Belkin, Vitaly, 17385402

-- Import Modules needed for reading/writing binary files.
import Data.ByteString (readFile, writeFile, ByteString, pack, unpack)
import Data.Word

-- Takes a file name and returns an IO List of Word8
-- Reads an image from file, produces an IO action with a list of [Word8].
readImage :: String -> IO [Word8]
readImage input = do image <- Data.ByteString.readFile input
                     return (unpack image)

-- Takes a Word8 list and a file name and produces an IO Action
-- Opens the file and write the contents of the list to it.
writeImage :: [Word8] -> String -> IO()
writeImage image output = Data.ByteString.writeFile output (pack image)

-- Takes two file names, input/output files
-- Reads the contents of an image from the input file [Word8]
-- Write the contents to the output file.
copyImage :: String -> String -> IO()
copyImage input output = do image <- readImage input
                            writeImage image output


-- a)
-- encodeA takes a list and returns a list of tuples, first it makes a head of the list the first element in the tuple, 
-- then it counts number of those elements identical to the head with takeUntil and makes that number the second element in a tuple, 
-- then removes used elements with takeAfter and repeats the algorythm
encodeA ::  Eq a=> [a] -> [(a,Int)]
encodeA [] = []
encodeA (h:t) = (h, takeUntil (h:t)) : encodeA (takeAfter (h:t)) 


takeUntil :: Eq a => [a] -> Int 
takeUntil [] = 0 
takeUntil (h:h1:t) = 
                   if h1 == h then 1 + takeUntil (h1:t) else 1 
takeUntil [x] = 1


 
takeAfter :: Eq a => [a] -> [a] 
takeAfter [] = []
takeAfter (h:h1:t) 
                  |h == h1 = takeAfter (h1:t)
                  |otherwise = (h1:t)
takeAfter [x] = []

-- b)
-- decodeA takes list of tuples and returns a list with all elements unfolded, 
-- each tuple is decoded with a separate tupleToList function and then attached to the rest of the list of tuples, 
-- that has decodeA permormed on it first. 
decodeA :: Eq a => [(a,Int)] -> [a]
decodeA [] = []
decodeA ((x,y):t) = tupleToList(x,y) ++ decodeA t


tupleToList :: Eq a => (a,Int) -> [a]
tupleToList (x,y) 
                   |y>0 = x : tupleToList (x,y-1)
                   |y == 0 = []
                   |y<0 = []



-- c) encodeB uses "map packtuple" to create a tuple with each element 
-- and then with "foldr combine" to create compressed tuples with several similar elements in a row
packtuple :: Eq a =>  a -> (a,Int)
packtuple a = (a,1)

combine ::  Eq a => (a,Int) -> [(a,Int)] -> [(a,Int)]
combine (a,b) [] = (a,b):[]
combine (a,b) ((x,y):t) = if a==x then ((x,y+1):t) else (a,b) : ((x,y):t)

encodeB :: Eq a => [a] -> [(a,Int)]
encodeB (h:t) = foldr  combine [] ( map packtuple (h:t))


-- d) using modified functions from c) encodeBx creates a list of Word8 tuples and then using removeTuples list of tuples is unfolded into a list of Word8
encode :: [Word8] -> [Word8]
encode (h:t) =  removeTuples (encodeBx (h:t))

packtuplex :: Word8 -> (Word8,Word8)
packtuplex a = (a,1)

combinex :: (Word8,Word8) -> [(Word8,Word8)] -> [(Word8,Word8)]
combinex (a,b) [] = (a,b):[]
combinex (a,b) ((x,y):t) 
                         | (a==x) && (y<255) = ((x,y+1):t) 
                         | (a==x) && (y>=255) = (a,b) : ((x,y):t)  
                         | (a/=x) = (a,b) : ((x,y):t)  

encodeBx ::[Word8] -> [(Word8,Word8)]
encodeBx (h:t) = foldr  combinex [] ( map packtuplex (h:t))

removeTuples :: [(Word8,Word8)] -> [Word8]
removeTuples ((a,b):t) = a : b : removeTuples t
removeTuples  _   = []

-- decoding uses createTuples and unfoldTuples: createTuples creates tuples from pairs of elements that supposedly represent a number and a counter, 
-- then unfoldTuples creates a list of elements adding each separate number to the list and reducing a counter inside active tuple until reaches zero
createTuples :: [Word8] -> [(Word8,Word8)]
createTuples (h:h1:t) = (h,h1) : createTuples t
createTuples [] = [] 

unfoldTuples :: [(Word8,Word8)] -> [Word8] 
unfoldTuples ((x,y):t) 
                   |y>0 = x : unfoldTuples ((x,y-1):t)
                   |y == 0 = unfoldTuples t
unfoldTuples [(x,y)]
                   |y>0 = x : unfoldTuples [(x,y-1)]
                   |y == 0 = []
                   |y<0 = []
unfoldTuples [] = []

decode :: [Word8] -> [Word8]
decode [] = []
decode (h:t) =  unfoldTuples( createTuples (h:t) )

-- e) for encoding and decoding images, we pass an encoded or decoded image respectively to the writeImage function
copyImageone :: String -> String -> IO()
copyImageone input output = do image <- readImage input
                               writeImage (encode image) output

copyImagetwo :: String -> String -> IO()
copyImagetwo input output = do image <- readImage input
                               writeImage (decode image) output
