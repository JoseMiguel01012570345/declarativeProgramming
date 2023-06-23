module Doc_Analysis
(    
    readTxtFiles,
    doc_Parse,
    queryFormat,
    delTwoPoints
)
where
    import System.Directory
    import Data.Char
    import System.IO.Unsafe (unsafePerformIO)
    import Data.List

    addFullPath:: [String]->[String]->[String]
    addFullPath [] fullPath=fullPath
    addFullPath (r:relativePath) fullPath =addFullPath relativePath (fullPath++[unsafePerformIO getCurrentDirectory++"/content/"++r])

    readTxT::[String]->[String]->[String]
    readTxT [] docContent=docContent
    readTxT (d:docsToRead) docContent = readTxT docsToRead (docContent++[unsafePerformIO (readFile d)])

    readTxtFiles ::String-> [String]
    readTxtFiles dir = unsafePerformIO $ do
        files <- listDirectory dir
        let txtFiles = filter (".txt" `isSuffixOf`) files
        let txtFiles1 = addFullPath txtFiles []
        let contents =  readTxT txtFiles1 []
        let result = delTwoPoints contents 2
            in return result

    delTwoPoints::[String] -> Int -> [String]
    delTwoPoints [] _ = []
    delTwoPoints xs 0 = xs
    delTwoPoints (x:xs) n = delTwoPoints xs (n - 1)
    
    regularExpression :: String -> Int->Int -> String -> String
    regularExpression "" _ _ ys = ys
    regularExpression xs end i ys=  
        if i-1==end 
            then ys
        else
            regularExpression xs end (i+1) (ys++[xs!!i])

    queryFormat::String->[String]
    queryFormat query =txtListFormat query query 0 0 []

    txtListFormat:: String->String->Int->Int->[String]->[String]
    txtListFormat [] _ _ _ doc=doc
    txtListFormat (c:doc) document start end split        
        | not(isAlpha c) && start /= end = txtListFormat doc document (end+1) (end+1) (split++(words (regularExpression document end start [])))        
        | not(isAlpha c) && start == end = txtListFormat doc document (end+1) (end+1) split        
        | (length document)-1 == end = txtListFormat doc document (end+1) (end+1) (split++(words (regularExpression document end start [])))        
        | otherwise = txtListFormat doc document start (end+1) split
    
    doc_Parse :: [String] -> [[String]]-> [[String]]
    doc_Parse [] ys = ys
    doc_Parse (d:docs) ys = doc_Parse docs (ys++[(txtListFormat d d 0 0 [])])