import Data.List
import System.IO
import Control.Monad.RWS.Class (MonadState(put))
--import Distribution.Simple.Program.HcPkg (list)


-- Diferença entre duas strings 
diffStrings :: String -> String -> Int
diffStrings [] [] = 0
diffStrings (x:xs) (y:ys) = if x == y then diffStrings xs ys else 1 + diffStrings xs ys

-- Diferença de Hamming entre duas strings
-- Casos base: Inserção de caracteres, remoção de caracteres e substituição de caracteres
diffHa :: String -> String -> Int
diffHa s1 s2 = diffAux s1 s2 (length s1) (length s2)
  where
    diffAux [] [] _ _ = 0
    diffAux s1 [] _ _ = length s1
    diffAux [] s2 _ _ = length s2
    diffAux (x:xs) (y:ys) len1 len2
      | x == y    = diffAux xs ys (len1 - 1) (len2 - 1)
      | otherwise = 1 + minimum [diffAux xs (y:ys) (len1 - 1) len2,   -- Remoção
                                 diffAux (x:xs) ys len1 (len2 - 1),   -- Inserção
                                 diffAux xs ys (len1 - 1) (len2 - 1)] -- Substituição


-- Função distanciaEdicaoHamming calcula a distância de Hamming entre duas strings
distanciaEdicaoHamming :: Int -> Int -> Double
distanciaEdicaoHamming 0 0 = 0
distanciaEdicaoHamming x y = fromIntegral x / fromIntegral y

-- Função stringToList devide by \n
stringToList :: String -> [String]
stringToList [] = []
stringToList str = lines str

-- Função EdicaoHamminglistPasser
edicaoHamminglistPasser :: [String] -> [String] -> [Double]
edicaoHamminglistPasser [] [] = []
edicaoHamminglistPasser (x:xs) (y:ys) = distanciaEdicaoHamming (diffHa x y) (max (length x) (length y)) : edicaoHamminglistPasser xs ys

-- Função diffHaToList
diffHaToList :: [String] -> [String] -> [Int]
diffHaToList [] [] = []
diffHaToList (x:xs) (y:ys) = diffHa x y : diffHaToList xs ys

-- Função Formatação
formatter :: Int -> Int -> Double -> String
formatter numLinha diffTotal media = "Numero de erros: " ++ show diffTotal ++ " | " ++ "Distancia de Edicao-Hamming da linha " ++ show numLinha ++ ": " ++ show media


formatOutput :: [Int] -> [Double] -> Int -> String
formatOutput [] [] i = ""
formatOutput (y:ys) (x:xs) i = formatter i y x ++ "\n" ++ formatOutput ys xs (i+1)
      

main = do
    putStrLn "Digite o nome do arquivo 1: "
    l1 <- getLine
    putStrLn "Digite o nome do arquivo 2: "
    l2 <- getLine
    putStrLn ""
    handle1 <- openFile l1 ReadMode
    handle2 <- openFile l2 ReadMode
    contents1 <- hGetContents handle1
    contents2 <- hGetContents handle2
    putStrLn (formatOutput (diffHaToList(stringToList contents1)(stringToList contents2)) (edicaoHamminglistPasser (stringToList contents1) (stringToList contents2)) 1)
    
    hClose handle1
    hClose handle2