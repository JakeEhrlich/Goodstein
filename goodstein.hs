import Data.List

data HN = Atom Integer
        | Sum [HN]
        | Power Integer Integer HN

eval :: HN -> Integer
eval (Atom n)      = toInteger n
eval (Sum hs)      = sum $ map eval hs
eval (Power n b h) = (toInteger n) * (toInteger b) ^ (eval h)

instance Show HN where
  show (Atom n)      = show n
  show (Sum hs)      = intercalate " + " (map show hs)
  show (Power 1 b h) = show b ++ "^(" ++ show h ++ ")"
  show (Power n b h) = show n ++ "*" ++ show b ++ "^(" ++ show h ++ ")"

toSum []  = Atom 0
toSum [x] = x
toSum xs  = Sum xs

toHN :: Integer -> Integer -> [HN]
toHN b 0              = []
toHN b n | power == 0 = [Atom $ fromIntegral n]
         | otherwise  = hterm : (toHN b diff)
  where power = floor . logBase (fromIntegral b) . fromIntegral $ n
        bvalue = b ^ power
        diff = n - times * bvalue
        times | bvalue /= 0 = n `div` bvalue
              | otherwise   = error $ "bvalue = 0: b = " ++ show b ++ " n = " ++ show n
        hterm | eval hpower /= 0 = Power times b hpower
              | otherwise        = Atom b
        hpower = toSum $ toHN b power

toHN' b n = toSum $ toHN b n

doIf p f x = if p x then f x else x

replace :: Integer -> HN -> HN
replace a (Atom b)      = Atom $ doIf (==a) (+1) b
replace a (Sum hs)      = Sum $ map (replace a) hs
replace a (Power n b h) = Power (doIf (==a) (+1) n) (doIf (==a) (+1) b) (replace a h)

step :: Integer -> Integer -> Integer
step a = (+ (-1)) . eval . replace a . toHN' a

goodstein :: Integer -> [Integer]
goodstein n = takeWhile (/= 0) $ n : go 2 n
    where go b n = let r = step b n in r : go (b + 1) r

main = mapM_ print (take 20 $ goodstein 13)
