choose :: String -> [String]
choose a = [[c, d, e] | c <- a, d <- a, e <- a, c /= d, d /= e, c/=e]
