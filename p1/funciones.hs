import Data.Binary.Builder (putInt16be)
import Data.List (span, genericLength)



suma :: Integer->Integer->Integer
suma x y =x+y
resta :: Integer->Integer->Integer
resta x y =x-y
multi :: Integer->Integer->Integer
multi x y =x*y

div :: Integer -> Integer -> (Integer, Integer)
div = aux 0
    where aux c x y
            | x >= y    = aux (c+1) (x-y) y
            | otherwise = (c,x)

getnum::IO Integer
getnum = do
  s <- getLine
  return (read s)

  
main = do

  putStr"1.- Sumar\n2.- Restar\n3.- Multiplicar\n4.- Dividir\n>\n"
  print"que quiere hacer: "
  opc <- getnum
  putStr"Ingrese el valor 1 :\n"
  numero1 <- getnum
  putStr"Ingrese el valor 2:\n"
  numero2 <- getnum
   
  let suma1=[suma numero1  numero2]
  let resta1=[resta numero1  numero2]
  let multiplica1=[multi numero1  numero2]
  --let divide=[c `div` d | c <- [numero1], d <- [numero2]]
  let divide1=[Prelude.div numero1  numero2]
  --funcion por pasos
  let resultado
        | opc==1 = suma1
        | opc==2 = resta1
        | opc==3 = multiplica1
        | opc==4 = divide1
        | otherwise = []
  putStrLn ("Resultado:" ++ show(resultado))




  