module SearchEngine(
    mySearch
)
where
    import System.Directory
    import DataBase(delDuplicatedWords, getTFSByWord, getIdfByWord, getMaxTF, getTFVectorNormal, getDocsVectors)
    import QueryWorker(getQueryTFSVector, getQueryIDF_TF_Vector)
    import SearchItem
    import VectorialModel(scalarProduct, idf_tf_Product, applyVectorialModel)
    import Doc_Analysis
    import System.IO.Unsafe (unsafePerformIO)

        --Gets the matrix values of documents
    getMatrixValues::[[(String,Float)]] -> [[Float]]
    getMatrixValues docs = getMatrixValuesAux docs []

    getMatrixValuesAux::[[(String,Float)]] -> [[Float]] -> [[Float]]
    getMatrixValuesAux [] result = result
    getMatrixValuesAux (doc:docs) result = (getRowValues doc [] : getMatrixValuesAux docs result)

    getRowValues::[(String,Float)] -> [Float] -> [Float]
    getRowValues [] result = result
    getRowValues ((word,value):words) result = (value:getRowValues words result)

    getQueryValues::[(String,Float)] -> [Float]
    getQueryValues [] = []
    getQueryValues ((word,value):values) = (value : getQueryValues values)

    --Gets the documets vectors with the words of query
    getDocsVectorsForQuery::[[(String,Float)]] -> [String] -> [[(String,Float)]]
    getDocsVectorsForQuery [] query = []
    getDocsVectorsForQuery (doc:docs) query = (getDocVectorForQuery doc query : getDocsVectorsForQuery docs query)

    getDocVectorForQuery::[(String,Float)] -> [String] -> [(String,Float)]
    getDocVectorForQuery doc [] = []
    getDocVectorForQuery doc (word:words) = (getTfOfWordInDoc word doc : getDocVectorForQuery doc words)

    getTfOfWordInDoc::String -> [(String,Float)] -> (String,Float)
    getTfOfWordInDoc word [] = (word,0.0)
    getTfOfWordInDoc word ((w,value):words)
        | w == word = (word,value)
        | otherwise = getTfOfWordInDoc word words

    applyNormalization::[[(String,Float)]] -> [Float] -> [[(String,Float)]]
    applyNormalization [] _ = []
    applyNormalization _ [] = []
    applyNormalization (doc:docs) (max:maxs) = (getTFVectorNormal doc max : applyNormalization docs maxs)

    getMaxTFByDoc::[[(String,Float)]] -> [Float]
    getMaxTFByDoc [] = []
    getMaxTFByDoc (doc:docs) = (getMaxTF doc: getMaxTFByDoc docs)

    setQueryVector::[String] -> [[(String,Float)]] -> [(String,Float)]
    setQueryVector [] _ = []
    setQueryVector query docs =
        let
            tfs = getQueryTFSVector query
            idfs = getIdfByWord docs query
        in
            getQueryIDF_TF_Vector tfs idfs 0.5 (getMaxTF tfs)

    getParamsForSearch::[String] -> [[(String,Float)]] -> ([(String,Float)],[[(String,Float)]])
    getParamsForSearch query docs_vectors =
        let
            query_vector = setQueryVector query docs_vectors
            docs = getDocsVectorsForQuery docs_vectors query
        in
            (query_vector,applyNormalization docs (getMaxTFByDoc docs_vectors))

    --Returns a tuple idf-query_vector-docs_vectors for the search
    getParamsForVectorialModel::[String] -> [[(String,Float)]] -> ([Float],[Float],[[Float]])
    getParamsForVectorialModel query docs =
        let
            idfs = getIdfByWord docs query
            (query_vector,docs_vectors) = getParamsForSearch query docs
        in
            (getQueryValues idfs, getQueryValues query_vector, getMatrixValues docs_vectors)

    getDocsScores::[Float] -> [Float] -> [[Float]] -> [Float]
    getDocsScores idfs query_vector docs =
        let
            docs_vectors = idf_tf_Product idfs docs
        in
            applyVectorialModel query_vector docs_vectors

    rank::[String] -> [[(String,Float)]] -> [Float]
    rank [] _ = []
    rank query docs =
        let
            (idfs,query_vector,docs_vectors) = getParamsForVectorialModel query docs
        in
            getDocsScores idfs query_vector docs_vectors

    searchAux::[String] -> [[String]] -> [Float] -> [SearchItem]
    searchAux [] _ _ = []
    searchAux _ [] _ = []
    searchAux _ _ [] = []
    searchAux (t:titles) (c:contents) (s:scores) = (getSearchItem t c s : searchAux titles contents scores)

    content::[[String]]
    content=doc_Parse (read_txt_files) []

    read_txt_files::[String]
    read_txt_files=readTxtFiles "./content"

    myData::[[(String,Float)]]
    myData=getDocsVectors (content)

    -- query ---> titulos ---> array de contenidos ---> TF por cada palabra en los documentos
    search::[String] -> [String] -> [[String]] -> [[(String,Float)]] -> [SearchItem]
    search query titles contents docs =
        let
            scores = rank query docs
        in
            searchAux titles contents scores

    minimo::[SearchItem]->SearchItem->SearchItem
    minimo [] item=item
    minimo (item1:items) item2
        | score item1 < score item2 = minimo items item1
        | otherwise =  minimo items item2

    newList::[SearchItem]->SearchItem-> [SearchItem]->Bool ->[SearchItem]
    newList [] _ ys _=ys
    newList (x:xs) item ys found =
        if title x == title item && found==False
            then
                newList xs item ys True
        else 
            newList xs item (ys++[x]) found

    minSort::[SearchItem]->[SearchItem]->[SearchItem]
    minSort [] items = items
    minSort items sorted=
        let minimus=minimo items (items!!0)
            in
                minSort (newList items minimus [] False) (minimus:sorted)
    
    mySearch::[String]->[SearchItem]
    mySearch query =minSort (search query (delTwoPoints (unsafePerformIO (getDirectoryContents "./content")) 2) content myData) []