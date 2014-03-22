-- бесконечный список степеней двойки
power = 1 : (map (*2) power) 
main = print (take 5 power) 