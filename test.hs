import Readline

doCommand "q" = return ()
doCommand x = do putStrLn x
		 loop

loop = do
       l <- readline "test> "
       case l of 
	      Just x -> do
			addHistory x
			doCommand x
	      Nothing -> return ()

main = loop
	       

