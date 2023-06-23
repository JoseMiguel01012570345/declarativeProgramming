module DataBase(
    delDuplicatedWords,
    getTFSByWord,
    getIdfByWord,
    getMaxTF,
    getTFVectorNormal,
    getDocsVectors,
    isInDoc
)
where
    timesIn::String -> [String] -> Float
    timesIn _ [] = 0
    timesIn word (x:xs)
        | x == word = 1 + timesIn word xs
        | otherwise = timesIn word xs

    getTFSByWord::[String] -> [(String,Float)]
    getTFSByWord words = getTFSByWordAux words (delDuplicatedWords words)

    getTFSByWordAux::[String] -> [String] -> [(String,Float)]
    getTFSByWordAux _ [] = []
    getTFSByWordAux words (x:xs) = ((x,timesIn x words) : getTFSByWordAux words xs)

    delDuplicatedWords::[String] -> [String]
    delDuplicatedWords words = delDuplicatedWordsAux words []

    delDuplicatedWordsAux::[String] -> [String] -> [String]
    delDuplicatedWordsAux [] _ = []
    delDuplicatedWordsAux (x:xs) result
        | isIn x result = delDuplicatedWordsAux xs result
        | otherwise = (x : delDuplicatedWordsAux xs (x:result))

    isIn::String -> [String] -> Bool
    isIn _ [] = False
    isIn word (x:xs)
        | word == x = True
        | otherwise = isIn word xs

    getIdfByWord::[[(String,Float)]] -> [String] -> [(String,Float)]
    getIdfByWord _ [] = []
    getIdfByWord docs (x:xs) =
        let
            docs_with_word = docsWithWord docs x
        in
            ((x,calculateIDF (fromIntegral (length docs)) docs_with_word) : getIdfByWord docs xs)

    calculateIDF::Float -> Float -> Float
    calculateIDF docs docs_with_word
        | docs_with_word > 0 = logBase 10 docs/docs_with_word
        | otherwise = -1

    isInDoc::[(String,Float)] -> String -> Bool
    isInDoc [] _ = False
    isInDoc ((w,value):xs) word
        | w == word = True
        | otherwise = isInDoc xs word

    docsWithWord::[[(String,Float)]] -> String -> Float
    docsWithWord [] _ = 0
    docsWithWord (doc:docs) word
        | isInDoc doc word = 1 + docsWithWord docs word
        | otherwise = docsWithWord docs word

    --Returns a list of lists of tuples String-Float = word,tf
    getDocsVectors::[[String]] -> [[(String,Float)]]
    getDocsVectors [] = []
    getDocsVectors (doc:docs) = (getTFSByWord doc : getDocsVectors docs)

    getMaxTF::[(String,Float)] -> Float
    getMaxTF [] = 0
    getMaxTF ((w,value):xs) =
        let
            v = getMaxTF xs
        in
            if value > v then value else v

    getTFVectorNormal::[(String,Float)] -> Float -> [(String,Float)]
    getTFVectorNormal [] _ = []
    getTFVectorNormal ((w,value):xs) mTF = ((w,value/mTF) : getTFVectorNormal xs mTF)