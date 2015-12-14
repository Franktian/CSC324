data MaybeInt = Success Integer | Failure deriving Show

isSuccess :: MaybeInt -> Bool
isSuccess (Success val) = True
isSuccess Failure = False

getValue :: MaybeInt -> Integer -> Integer
getValue (Success val) sec = val
getValue Failure sec = sec

collectSuccesses :: [MaybeInt] -> [Integer]
collectSuccesses [] = []
collectSuccesses ((Success val):xs) = val : collectSuccesses xs
collectSuccesses (Failure:xs) = collectSuccesses xs

addMaybe :: MaybeInt -> MaybeInt -> MaybeInt
addMaybe (Success x) (Success y) = Success (x + y)
addMaybe Failure (Success y) = Failure
addMaybe (Success x) Failure = Failure
addMaybe Failure Failure = Failure

applyMaybe :: (Integer -> Integer) -> MaybeInt -> MaybeInt
applyMaybe f (Success val) = Success (f val)
applyMaybe f Failure = Failure

mapMaybe :: (Integer -> Integer) -> [MaybeInt] -> [MaybeInt]
mapMaybe f = map (applyMaybe f)

filterHelper :: (Integer -> Bool) -> MaybeInt -> Bool
filterHelper f (Success val) = f val
filterHelper f Failure = False

filterMaybe :: (Integer -> Bool) -> [MaybeInt] -> [MaybeInt]
filterMaybe f = filter (filterHelper f)


foldlHelper :: (Integer -> Integer -> Integer) -> MaybeInt -> MaybeInt -> MaybeInt
foldlHelper f (Success val1) (Success val2) = Success (f val1 val2)
foldlHelper f (Success _) Failure = Failure
foldlHelper f Failure (Success _) = Failure
foldlHelper f Failure Failure = Failure

foldlMaybe :: (Integer -> Integer -> Integer) -> MaybeInt -> [MaybeInt] -> MaybeInt
foldlMaybe f = foldl (foldlHelper f)
