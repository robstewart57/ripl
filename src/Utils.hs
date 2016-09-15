module Utils where

pkgToDir str =
  let xs =
        map
          (\c ->
             if c == '.'
               then '/'
               else c)
          str
  in "src/" ++ xs ++ "/"
