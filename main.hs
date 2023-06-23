import SearchEngine
import System.Directory
import DataBase(delDuplicatedWords, getTFSByWord, getIdfByWord, getMaxTF, getTFVectorNormal, getDocsVectors)
import QueryWorker(getQueryTFSVector, getQueryIDF_TF_Vector)
import SearchItem
import VectorialModel(scalarProduct, idf_tf_Product, applyVectorialModel)
import Doc_Analysis
import System.IO.Unsafe (unsafePerformIO)

concatenar::[String]->String ->String
concatenar [] ys=ys
concatenar (x:xs) ys =concatenar xs (ys++" "++x)
        
exit::String->Bool
exit input=
  if input=="exit"
    then False
  else 
    True

mostrar::[SearchItem]-> [String]->[String]
mostrar [] display = display
mostrar (r:resoult) display=
  if score r>0
    then mostrar resoult (display++["SNIPPET: "]++[concatenar (snippet r) []]++["-------------------"])
  else
    mostrar resoult display

main  = do
  let input=unsafePerformIO getLine
    in 
     print(concatenar (mostrar (mySearch (queryFormat input )) []) "")

  