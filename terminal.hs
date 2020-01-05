module Terminal where 
    import FileOps
    import qualified Data.Text as T
    
    --fs = Directory "" [] [(File "1" [""] "a"),(File "2" [""] "b"),(Directory "NNN" [""] [(File "z" ["NNN"] "")])]

    
    parseString::String->[String]
    parseString line= map T.unpack (T.splitOn (T.pack " ") (T.pack line))

    op :: Context -> [String] -> (Context,String)
    op c@(Context fs p) (x:xs)
       | x == "pwd" = pwd c
       | x == "cd" = (cd c (xs !! 0))
       | x == "ls" && (length xs == 0) = ls c Nothing
       | x == "ls" && (length xs) /= 0 = ls c (Just (xs !! 0))
       | x == "rm" = rm c xs
       | x == "cat" = cat c xs
       | otherwise = pwd c

    pwd::Context->(Context,String)
    pwd c@(Context r pth) = (c, pathToString pth)

    cd::Context->String->(Context, String)
    cd c@(Context r pth) arg = ((Context r (normalizePath (pathToArr arg) c)), "")

    ls::Context->Maybe String->(Context, String)
    ls c@(Context r pth) Nothing = (c, (foldl (\t c -> t ++ " " ++ (name c)) "" (helper (findFile r pth))) )
      where helper Nothing = []
            helper (Just (Directory _ _ c)) = c
            helper (Just (File _ _ _)) = []
    ls c@(Context r pth) (Just arg) = (c, (foldl (\t c -> t ++ " " ++ (name c)) "" (helper (findFile r (normalizePath (pathToArr arg) c)))))
      where helper Nothing = []
            helper (Just (Directory _ _ c)) = c
            helper (Just (File _ _ _)) = []

    rm::Context->[String]->(Context, String)
    rm c@(Context r pth) files = ((Context (foldl (\cur y -> deleteFile cur (pth ++ [y])) r files) pth),"")

    cat::Context->[String]->(Context,String)
    cat c@(Context r pth) files 
        | (length files) == 1 = (c, getFileContent (findFile r (pathToArr (files !! 0))))
        | files !! ((length files) - 2) == ">" = (Context (addFile (deleteFile r newPath) (init newPath) (last newPath) newContent) pth, "")
           where newPath = (pathToArr (files !! ((length files) - 1)))
                 newContent = foldl (\x y -> x ++ (helper (findFile r (pathToArr y)))) "" (init (init files))
                    where helper Nothing = ""
                         helper (Just (File _ _ cont)) = cont
                         helper (Just (Directory _ _ _)) = ""
          | otherwise = (c, "")

    