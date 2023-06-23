module VectorialModel(
    scalarProduct,
    idf_tf_Product,
    applyVectorialModel
)
where
    scalarProduct::[Float] -> [Float] -> Float
    scalarProduct _ [] = 0
    scalarProduct [] _ = 0
    scalarProduct (x1:xs1) (x2:xs2) = x1*x2 + scalarProduct xs1 xs2

    --Multiply the idf vector for all the dcuments vectors tf
    idf_tf_Product::[Float] -> [[Float]] -> [[Float]]
    idf_tf_Product _ [] = []
    idf_tf_Product idf (doc:docs) = (idf_tf_ProductRow idf doc : idf_tf_Product idf docs) 

    --Multiply the idf vector and the tf vector of one document
    idf_tf_ProductRow::[Float] -> [Float] -> [Float]
    idf_tf_ProductRow [] [] = []
    idf_tf_ProductRow (x1:xs1) (x2:xs2) = ((x1*x2):idf_tf_ProductRow xs1 xs2)
    
    getSumCuadratic::[Float] -> Float
    getSumCuadratic [] = 0
    getSumCuadratic (x:xs) = x*x + getSumCuadratic xs

    --Apply the vectorial model
    applyVectorialModel::[Float] -> [[Float]] -> [Float]
    applyVectorialModel _ [] = []
    applyVectorialModel query (doc:docs)
        | (getSumCuadratic query)*(getSumCuadratic doc) == 0.0 = (0.0 : applyVectorialModel query docs) 
        | otherwise = (( (scalarProduct query doc) / sqrt((getSumCuadratic query)*(getSumCuadratic doc))) : applyVectorialModel query docs)
