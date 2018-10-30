module Main where

import Control.Exception.Base
import Control.Monad.Trans.Either
import Control.Monad.Trans.Class
import Multilinear.Vector
import Multilinear.Matrix

import           Multilinear
import           Multilinear.Generic
import qualified Multilinear.Matrix         as Matrix
import qualified Multilinear.Tensor         as Tensor
import qualified Multilinear.Vector         as Vector

-- PARAMETRY SKRYPTU
fi     = signum  -- funkcja aktywacji perceptronu
layers = 10      -- liczba warstw perceptronu

mlp_input         = "mnist/image_60k.csv"          -- dane uczące dla perceptronu
mlp_expected      = "mnist/label_60k.txt"       -- dane oczekiwane dla percepttronu
mlp_classify      = "mnist/image_10k.csv"       -- dane do klasyfikacji na nauczonym perceptronie
mlp_output        = "mnist/output_10k.txt"         -- wyjście perceptronu
--mlp_expected_output = "mnist/label_10k.txt"         -- spodziewane wyjście perceptronu

-- PERCEPTRON WIELOWARSTWOWY
perceptron :: Int                    -- ns:  liczba neuronów w warstwie
           -> Int                    -- ks:  liczba warstw 
           -> Int                    -- ps:  liczba wektorów uczących
           -> Int                    -- cs:  liczba wektorów do klasyfikacji
           -> (Int -> Tensor Int) -- x t: wejścia uczące w funkcji czasu
           -> (Int -> Tensor Int) -- e t: wyjścia oczekiwane w funkcji czasu
           -> (Int -> Tensor Int) -- c t: dane do klasyfikacji w funkcji czasu
           -> Tensor Int          -- Tensor ("i","t"): zaklasyfikowane dane

perceptron ns ks ps cs x e c =
  let -- wagi startowe
      zero = Tensor.const ("ki",[ks,ns]) ("j",[ns]) 0
      -- wagi w następnym kroku uczącym
      nextWeights w x e =
        let ygen [k] [] = -- tensor wyjść
              if k == 0 then x $| ("j","") 
              else fi <$> w $$| ("k",[k - 1]) $| ("i","j") * ygen [k-1] [] $| ("j","")
            y = Tensor.generate ("k",[ks + 1]) ("",[]) $ \[k] [] -> ygen [k] []
            -- tensor wejścia-wyjścia omega
            om = Tensor.generate ("k",[ks]) ("",[]) $ 
              \[k] [] -> ygen [k + 1] [] $| ("i","") * ygen [k] [] $| ("j","") \/ "j"
            incWgen [k] [] = -- inkrementacyjna propagacja wsteczna
              if k == ks - 1 then x $| ("j","") \/ "j" * (y $$| ("k",[ks-1]) $| ("i","") - e $| ("i",""))
              else Multilinear.transpose (w $$| ("k",[k+1])) $| ("i","b") * 
                   incWgen [k+1] [] $| ("b","c") * 
                   om $$| ("k",[k]) $| ("c","j")
            incW = Tensor.generate ("k",[ks]) ("",[]) $ \[k] [] -> incWgen [k] []
        in  w $| ("ki","j") + incW $| ("ki","j")
      xl = take 2 $ x <$> [0 .. ps - 1]
      el = take 2 $ e <$> [0 .. ps - 1]
      -- uczenie sieci
      learnedNetwork = foldr (\(x,e) w -> nextWeights w x e) zero $ zip xl el
      -- praca nauczonej sieci
      out t = fi <$> learnedNetwork $$| ("k",[ks-1]) $| ("i","j") * c t $| ("j","")
  in  Tensor.generate ("",[]) ("t",[cs]) $ \[] [t] -> out t

  -- OPERACJE WEJŚCIA/WYJŚCIA
prog :: EitherT SomeException IO ()
prog = do
  lift $ print "lLl"

  -- wczytywanie danych
  mlpInput :: Tensor Int <- Matrix.fromCSV "tj" mlp_input ','
  mlpExp :: Tensor Int <- Matrix.fromCSV "tj" mlp_expected ','
  mlpClas :: Tensor Int <- Matrix.fromCSV "tj" mlp_classify ','

  lift $ print mlpInput
  lift $ print mlpExp
  lift $ print mlpClas

  let mx t = Multilinear.transpose $ mlpInput $$| ("t",[t])
  let me t = Multilinear.transpose $ mlpExp $$| ("t",[t])
  let mc t = Multilinear.transpose $ mlpClas $$| ("t",[t])
  let (ns_mlp,ps_mlp,cs_mlp)= 
         (mlpInput `size` "j", mlpExp `size` "t", mlpClas `size` "t")
  -- perceptron
  let mlp_net = perceptron ns_mlp layers ps_mlp cs_mlp mx me mc
  smlp <- lift $ Matrix.toCSV mlp_net mlp_output ';'
  lift $ putStrLn $ "Perceptron: " ++ show smlp ++ " vectors saved to '" ++ mlp_output ++ "'."
  return ()

---- ENTRY POINT
--main :: IO (Either SomeException ())
--main = runEitherT prog


main :: IO (Either SomeException ())
main = do
    let v = Multilinear.Vector.fromIndices "i" 10 id
    print v
    runEitherT prog
    
