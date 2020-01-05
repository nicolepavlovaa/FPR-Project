module Main where
    import Terminal
    import FileOps

    
    termMain:: Context-> IO ()
    termMain c = do
        nl <- getLine
        let (cntx, out)  = (op c (parseString nl))
          in do
               print out
               termMain cntx
    
    main = do
        termMain (Context example [])



    