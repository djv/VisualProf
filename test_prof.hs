module Main where
f a b
  = ({-# SCC "5" #-}
       ({-# SCC "3" #-}
          (({-# SCC "2" #-} ({-# SCC "0" #-} a) * ({-# SCC "1" #-} 2))))
         + ({-# SCC "4" #-} b))
 
main :: IO ()
main
  = ({-# SCC "14" #-}
       ({-# SCC "10" #-}
          ({-# SCC "6" #-} print) .
            ({-# SCC "9" #-}
               ({-# SCC "7" #-} length) . ({-# SCC "8" #-} lines)))
         =<<
         ({-# SCC "13" #-}
            ({-# SCC "11" #-} readFile)
              ({-# SCC "12" #-} "/etc/dictionaries-common/words")))