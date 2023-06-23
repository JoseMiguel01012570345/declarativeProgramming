module QueryWorker(
    getQueryTFSVector,
    getQueryIDF_TF_Vector
)
where
    import DataBase(delDuplicatedWords,getTFSByWord)
    
    getQueryTFSVector::[String] -> [(String,Float)]
    getQueryTFSVector query = getTFSByWord query

    getQueryIDF_TF_Vector::[(String,Float)] -> [(String,Float)] -> Float -> Float -> [(String,Float)]
    getQueryIDF_TF_Vector [] _ _ _ = []
    getQueryIDF_TF_Vector _ [] _ _ = []
    getQueryIDF_TF_Vector ((w1,tf):tfs) ((w2,idf):idfs) a mTF = ((w1,((a + (1 - a)*tf)/mTF)*idf): getQueryIDF_TF_Vector tfs idfs a mTF)