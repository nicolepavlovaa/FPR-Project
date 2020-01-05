module FileOps where 
    import qualified Data.Text as T

    type Name = String
    type Path = [Name]

    data File = File {name :: Name, path::Path, content :: String} | Directory {name :: Name, path::Path, children :: [File]} deriving (Show)

    type Root = File
    data Context = Context {root::Root, currPath::Path} deriving (Show)

    fs = (Directory "" [] [])
    example = (Directory "" [] [(File "1" [""] "a"),(File "2" [""] "b"), (File "3" [""] "ss"), (Directory "NNN" [""] [(File "m" ["NNN"] "pp") , (File "z" ["NNN"] "")])])

    getName::File->Name
    getName (File n _ _) = n 
    getName (Directory n _ _) = n

    getFileContent::Maybe File->String
    getFileContent (Just (File _ _ c)) = c
    getFileContent Nothing = ""
    getFileContent (Just (Directory _ _ _)) = ""

    getChildren::File->[File]
    getChildren (File _ _ _) = []
    getChildren (Directory _ _ cont) = cont

    pathToArr::String->[String]
    pathToArr path = map T.unpack (T.splitOn (T.pack "/") (T.pack path))
    
    contains :: File -> Name -> Bool
    contains (File _ _ _) _ = False
    contains (Directory _ _ c) name = any (\f -> (getName f) == name) c

    transform::Root->Path->(File->File)->Root
    transform root [] f = f root
    transform (Directory n p cont) (x:xs) f = (Directory n p (map remap cont))
       where remap arg
               | (name arg) == x = transform arg xs f
               | otherwise = arg
    transform f1 _ _ = f1

    addFile::Root->Path->Name->String->Root
    addFile root path name cont = transform root path func
      where func :: File -> File  
            func f@(File _ _ _) = f
            func d@(Directory n p c)
              | contains d name = d
              | otherwise = Directory n p ((File name (p ++ [n]) cont) : c)

    addDirectory::Root->Path->Name->Root
    addDirectory root path name = transform root path func
         where func :: File -> File  
               func f@(File _ _ _) = f
               func d@(Directory n p c)
                    | contains d name = d
                    | otherwise = Directory n p ((Directory name (p ++ [n]) []) : c)

    
    normalizePath::Path->Context->Path
    normalizePath [] _ = []
    normalizePath (x:xs) (Context r p) = foldl helper [] normalized
       where normalized
               | x == "" = xs
               | otherwise = p ++ (x:xs)
             helper total curr
               | curr == "." || curr == "" = total
               | curr == ".." && total /= [] = fst (splitAt ((length total) - 1) total)
               | curr == ".." && total == [] = []
               | otherwise = total ++ [curr]

    transformContext::String->Context->Context
    transformContext p c@(Context r cp) = (Context r np)
      where np = normalizePath (pathToArr p) c

    pathToString::Path->String
    pathToString p = foldl (\ x y -> x ++ "/"++ y) "" p

    deleteFile::Root->Path->Root
    deleteFile r [] = r
    deleteFile r p = transform r dir f
       where (dir,fl) = splitAt ((length p)-1) p
             f (Directory n pt c) = Directory n pt (filter (\x -> (getName x) /= (fl !! 0)) c)
             f a = a 

    findFile::Root->Path-> Maybe File
    findFile f [] = Just f
    findFile (File _ _ _) _ = Nothing
    findFile d@(Directory n pth cont) (x:xs) = helper l 
       where l = filter (\f -> (name f) == x) cont
             helper [] = Nothing
             helper (y:ys) = findFile y xs

  
        










    




