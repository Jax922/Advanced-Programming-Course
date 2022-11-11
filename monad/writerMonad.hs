import Control.Monad.Writer

half :: Int -> Writer String Int
half x = do
    tell ("I just halved " ++ (show x) ++ "!")
    return (x `div` 2)

