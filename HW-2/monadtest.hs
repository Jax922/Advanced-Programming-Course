newtype State s a = State { runState :: s -> (s, a) }
let stst = State{ runState = (\y -> (y, y+1)) }
