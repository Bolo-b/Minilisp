## MiniLisp

Proyecto 1: Lenguajes de programación.

### Integrantes:

- Carrillo Benítez Valentina

- Aguirre Cruz Cassandra

- Vázquez Rincón Oscar

### Creación de archivos de sintaxis concreta y abstracta:

#### Lexer

`$ alex Lexer.x`

#### Parser

`$ happy Parser.y`

### Ejecución:
`$ ghc -package array Main.hs Menu.hs MiniLisp.hs Interp.hs Desugar.hs Parser.hs Lexer.hs -o minilisp
`

`$ ./minilisp`
### Para el buen funcionamiento de las pruebas se debe tener en cuenta lo siguiente:
-Cada aplicación de función debe tener ( ) completos

-Usar lambdas multiparamétricas cuando sea posible
-Verificar que todos los ( tengan su ) correspondiente          
-Let para dependencias secuenciales, Let para paralelas*
-Las listas pueden usar comas o espacios entre elementos
### Dependencias:

[__Happy__](<https://haskell-happy.readthedocs.io/en/latest/>) Compatible con GHC >= 7.0.

[__Alex__](<https://haskell-alex.readthedocs.io/en/latest/>) Compatible con GHC >= 7.0.
