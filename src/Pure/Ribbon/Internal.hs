{-# language CPP #-}
module Pure.Ribbon.Internal where

import Pure.Elm

#ifdef __GHCJS__
scrollFull :: Node -> IO ()
scrollFull = scroll_left_full_js

scrollLeft :: Node -> IO ()
scrollLeft = scroll_left_js

scrollRight :: Node -> IO ()
scrollRight = scroll_right_js

moreLeft :: Node -> IO Bool
moreLeft = more_left_js

moreRight :: Node -> IO Bool
moreRight = more_right_js
#else
scrollFull :: Node -> IO ()
scrollFull _ = pure ()

scrollLeft :: Node -> IO ()
scrollLeft _ = pure ()

scrollRight :: Node -> IO ()
scrollRight _ = pure ()

moreLeft :: Node -> IO Bool
moreLeft _ = pure False

moreRight :: Node -> IO Bool
moreRight _ = pure True
#endif


#ifdef __GHCJS__
foreign import javascript unsafe
    "$1.scrollLeft = $1.scrollWidth" scroll_left_full_js :: Node -> IO ()
foreign import javascript unsafe
    "$1.scrollBy({left: -$1.clientWidth / 2, behavior: 'smooth'})" scroll_left_js :: Node -> IO ()
foreign import javascript unsafe
    "$1.scrollBy({left: $1.clientWidth / 2, behavior: 'smooth'})" scroll_right_js :: Node -> IO ()
foreign import javascript unsafe
    "$r = $1.scrollLeft + $1.clientWidth < $1.scrollWidth" more_right_js :: Node -> IO Bool
foreign import javascript unsafe
    "$r = $1.scrollLeft > 0" more_left_js :: Node -> IO Bool
#endif

