{-

COMPUTACIÓN AVANZADA - TRABAJO PRÁCTICO: "PRETTY PRINTER"

INTEGRANTES: APROSOFF, FEDERICO - KORNBERG, NICOLÁS - VAINSTEIN, TOMÁS

LENGUAJE DE PROGRAMACIÓN: JAVA.

-- LÓGICA --

1) BORRAR LOS ESPACIOS PREEXISTENTES DE LA CADENA DE CARACTERES.
2) CREAR UNA LISTA CON LA CANTIDAD DE ESPACIOS A INSERTAR EN CADA POSICIÓN EN LA QUE SE INSERTARÁN ESPACIOS.
3) INSERTAR LOS ESPACIOS CORRESPONDIENTES LUEGO DE CADA CARACTER ESPECIAL.
4) INSERTAR LOS SALTOS DE LÍNEA DESPUÉS DE CADA CARACTER ESPECIAL.
5) IMPRIMIR EL CONTENIDO. MUESTRA EN LA CONSOLA EL CÓDIGO TABULADO.

-- EJEMPLO N°1 --

PARA LOS SIGUIENTES DATOS:

• CADENA DE CARACTERES: public class HelloWorld {public static void main(String[] args) {for (int i = 0, i < 10, i++) {System.out.println('Hello World');System.out.println(i);}}}
• LISTA DE ENTEROS: [4, 4, 4]

LA SALIDA SERÁ:

public class HelloWorld {
    public static void main(String[] args) {
        for (int i = 0, i < 10, i++) {
            System.out.println('Hello World');
            System.out.println(i);
        }
    }
}

-- EJEMPLO N°2 --

PARA LOS SIGUIENTES DATOS:

• CADENA DE CARACTERES: abc{def{ghi;jkl;}hola mundo;}
• LISTA DE ENTEROS: [4, 4]

LA SALIDA SERÁ:

abc{
    def{
        ghi;
        jkl;
    }
    hola mundo;
}

-- EJEMPLO N°3 --

PARA LOS SIGUIENTES DATOS:

• CADENA DE CARACTERES: void main() {int p=i=1;while(i<=10) {p*=i++;}printf('%d/n',p);}
• LISTA DE ENTEROS: [2, 2, 1]

LA SALIDA SERÁ:

void main() {
  int p=i=1;
  while(i<=10) {
    p*=i++;
  }
  printf('%d/n',p);
}

-- EJEMPLO N°4 --

PARA LOS SIGUIENTES DATOS:

• CADENA DE CARACTERES: public class primeNumbersFoundber {public static void main(String[] args) {int i;int num = 0;int maxCheck = 100;boolean isPrime = true;String primeNumbersFound = '';for (i = 1, i <= maxCheck, i++) {isPrime = CheckPrime(i);if (isPrime) {primeNumbersFound = primeNumbersFound + i + ' ';}}System.out.println('Prime numbers from 1 to ' + maxCheck + ' are:');System.out.println(primeNumbersFound);}public static boolean CheckPrime(int numberToCheck) {int remainder;for (int i = 2, i <= numberToCheck / 2, i++) {remainder = numberToCheck % i;if (remainder == 0) {return false;}}return true;}}
• LISTA DE ENTEROS: [4, 4, 4, 4]

LA SALIDA SERÁ:

public class primeNumbersFoundber {
    public static void main(String[] args) {
        int i;
        int num = 0;
        int maxCheck = 100;
        boolean isPrime = true;
        String primeNumbersFound = '';
        for (i = 1, i <= maxCheck, i++) {
            isPrime = CheckPrime(i);
            if (isPrime) {
                primeNumbersFound = primeNumbersFound + i + ' ';
            }
        }
        System.out.println('Prime numbers from 1 to ' + maxCheck + ' are:');
        System.out.println(primeNumbersFound);
    }
    public static boolean CheckPrime(int numberToCheck) {
        int remainder;
        for (int i = 2, i <= numberToCheck / 2, i++) {
            remainder = numberToCheck % i;
            if (remainder == 0) {
                return false;
            }
        }
        return true;
    }
}

-}

ejemplo1 = printPrettyPrinter ("public class HelloWorld {public static void main(String[] args) {for (int i = 0, i < 10, i++) {System.out.println('Hello World');System.out.println(i);}}}", [4,4,4])
ejemplo2 = printPrettyPrinter ("abc{def{ghi;jkl;}hola mundo;}", [4,4])
ejemplo3 = printPrettyPrinter ("void main() {int p=i=1;while(i<=10) {p*=i++;}printf('%d/n',p);}", [2,2,1])
ejemplo4 = printPrettyPrinter ("public class primeNumbersFoundber {public static void main(String[] args) {int i;int num = 0;int maxCheck = 100;boolean isPrime = true;String primeNumbersFound = '';for (i = 1, i <= maxCheck, i++) {isPrime = CheckPrime(i);if (isPrime) {primeNumbersFound = primeNumbersFound + i + ' ';}}System.out.println('Prime numbers from 1 to ' + maxCheck + ' are:');System.out.println(primeNumbersFound);}public static boolean CheckPrime(int numberToCheck) {int remainder;for (int i = 2, i <= numberToCheck / 2, i++) {remainder = numberToCheck % i;if (remainder == 0) {return false;}}return true;}}", [4,4,4,4])

printPrettyPrinter :: ([Char], [Int]) -> IO()
printPrettyPrinter (x, y) = imprimir (prettyPrinter (borrarEspaciosPrevios x, y))

imprimir :: [Char] -> IO()
imprimir x = putStr x -- putStr es una función del Prelude de Haskell.

prettyPrinter :: ([Char], [Int]) -> [Char]
prettyPrinter ([], y) = []
prettyPrinter (x, y) = insertarSaltosDeLinea (insertarEspacios (x, cantidadDeEspaciosAInsertar (x, y, 0)))

borrarEspaciosPrevios :: [Char] -> [Char]
borrarEspaciosPrevios [] = []
borrarEspaciosPrevios (x:[]) = [x]
borrarEspaciosPrevios (x:(y:z)) = if x == ' ' && y == ' ' then borrarEspaciosPrevios z else x : borrarEspaciosPrevios(y:z)

insertarSaltosDeLinea :: [Char] -> [Char]
insertarSaltosDeLinea [] = []
insertarSaltosDeLinea (x:y) = if esCaracterEspecial x then append (x : "\n", insertarSaltosDeLinea y) else x : insertarSaltosDeLinea y

insertarEspacios :: ([Char], [Int]) -> [Char]
insertarEspacios ([], y) = []
insertarEspacios (x:[], y) = [x]
insertarEspacios (x:xs, y:ys) = if esCaracterEspecial x
                                then append(x : dameEspacios y, insertarEspacios(xs, ys))
                                else x : insertarEspacios(xs, y:ys)

cantidadDeEspaciosAInsertar :: ([Char], [Int], Int) -> [Int]
cantidadDeEspaciosAInsertar ([], y, n) = []
cantidadDeEspaciosAInsertar (x:[], y, n) = []
cantidadDeEspaciosAInsertar (x:xs, y, n) = if x == '{' then sumaHasta(y, n+1) : cantidadDeEspaciosAInsertar(xs, y, n+1)
                                                       else if cabeza xs == '}' then sumaHasta(y, n-1) : cantidadDeEspaciosAInsertar(xs, y, n-1)
                                                       else if x == ';' || x == '}' then sumaHasta(y, n) : cantidadDeEspaciosAInsertar(xs, y, n)
                                                       else cantidadDeEspaciosAInsertar(xs, y, n)

esCaracterEspecial :: Char -> Bool
esCaracterEspecial x = x == '{' || x == ';' || x == '}'

dameEspacios :: Int -> [Char]
dameEspacios 0 = []
dameEspacios n = ' ' : dameEspacios (n-1)

sumaHasta :: ([Int], Int) -> Int
sumaHasta ([], y) = 0
sumaHasta (x, y) = (if y > longitud x then dameSumaHasta (x, y) else 0) + dameSumaHasta (x, y)

dameSumaHasta :: ([Int], Int) -> Int
dameSumaHasta ([], y) = 0
dameSumaHasta (x, 0) = 0
dameSumaHasta (x:y, n) = x + dameSumaHasta (y, n-1)

append :: ([a], [a]) -> [a]
append ([], y) = y
append (x:xs, y) = x : append (xs, y)

cabeza :: [a] -> a
cabeza (x:y) = x

longitud :: [a] -> Int
longitud [] = 0
longitud (x:y) = 1 + longitud y