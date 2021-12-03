
#--------funciones--------
#Suma
def Suma(x, y):
    return x + y

#---------Resta
def Resta(x, y):
    return x - y

#------- Multiplica
def Multiplicar(x, y):
    return x * y

#---------División
def Dividdir(x, y):
    return x / y

#-----------Menu
print("Operación a Realizar …….")
print("1.Suma")
print("2.Resta")
print("3.Multiplicar")
print("4.Dividdir")

# Operación a realizar
def main():
    valor = input("Dime que quieres hacer :")
    num1 = int(input("Primer número : "))
    num2 = int(input("Segundo numero: "))
    if valor == '1':
        print(num1,"+",num2,"=", Suma(num1,num2))
    elif valor == '2':
        print(num1,"-",num2,"=", Resta(num1,num2))
    elif valor == '3':
        print(num1,"*",num2,"=", Multiplicar(num1,num2))
    elif valor == '4':
        print(num1,"/",num2,"=", Dividdir(num1,num2))
    else:
        print("Invalid input")
  
main()    


