module SearchItem(
    SearchItem(..),
    getSearchItem
)
where
    data SearchItem = SearchItem{
        snippet::[String],
        score::Float
    }deriving(Show)

    getSearchItem::String -> [String] -> Float -> SearchItem
    getSearchItem doc_title doc_content doc_score = 
        let
            doc_snippet = getSnippet doc_content
        in
            SearchItem{ title = doc_title, snippet = doc_snippet, score = doc_score}

    getSnippet::[String] -> [String]
    getSnippet content = getSnippetAux content [] 20

    getSnippetAux::[String] -> [String] -> Int -> [String]
    getSnippetAux [] snippet _ = snippet
    getSnippetAux _ snippet 0 = snippet
    getSnippetAux (d:doc) snippet count = getSnippetAux doc (snippet++[d]) (count - 1)