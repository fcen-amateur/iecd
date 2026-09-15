// Latex Look
#set page(margin: 1in, numbering: "1 de 1")
#set par(leading: .75em, first-line-indent: 1.8em, justify: true, spacing: 0.55em)
#set text(font: "New Computer Modern", lang: "es")
#show heading: set block(above: 1.4em, below: 1em)
#show link: underline

// ctheorems
#import "@preview/ctheorems:1.1.3": *
#show: thmrules
// ambientes de teoremas, "preguntas" y "respuesta", junto con "respuestas ocultables" `rtao`
#let thm = thmbox("teorema", "Teorema", base_level: 0)
#let obs = thmbox("observacion", "Observación", base_level: 0)
#let defn = thmbox("definition", "Definición", inset: (x: 1.2em, top: 1em), base_level: 0)
#let prg = thmbox("pregunta", "Pregunta",  fill: rgb("#fffb8691"), base_level: 0)
#let rta = thmproof("respuesta", "Respuesta", fill: rgb("#acf6cc88"), inset: (x: 1.2em, top: 1em, bottom: 1em))
#let rtao(body, hide: false) = {
  if hide { [] } else { rta[#body] }
}

// Algunas reglas de show para bloques de codigo, citas, ecuaciones y el simbolo de test, $phi$
#set quote(quotes: true, block: true)
#show sym.phi: sym.phi.alt
#show raw.where(block: true):  it => pad(top: 0.5em, left: 2em, bottom: 0.5em, 
  block.with(
    fill: luma(240),
    inset: 10pt,
    radius: 4pt,
  )(it)
)
#show math.equation.where(block: true):  it => pad(top: 0.5em, bottom: 0.5em, it)

// operadores útiles en todo el texto
#let ind(x) = $op(bb(1)){#x}$
#let iff = sym.arrow.l.r.double
#let Pr = math.op("Pr", limits: false)
#let ul(x) = $underline(#x)$
#let vv(x) = $bold(#x)$
#let TT = $T^+$

= IECD 2C2024 - Trabajo Práctico

En el trabajo siguiente, estudiaremos el test "de rango signado de Wilcoxon", un test no paramétrico para la mediana de una distribución simétrica. Tendrán que implementarlo de manera compatible con la implementación nativa de $R$, `wilcox.test`, y luego calcular su potencia para alternativas puntuales por el método de _botstrap_. Para ello, introduciremos primero
- `S3`, el paradigma OOP #footnote[Programación Orientada a Objetos, por sus siglas en inglés] más viejo de R (sí, hay varios) y
- las condiciones de validez y propiedades generales del test de Wilcoxon.

== Condiciones de entrega y aprobación
El presente escrito es un apunte sobre OOP y el test de Wilcoxon, con preguntas prácticas diseminadas en medio. Cada pregunta correcta vale tantos puntos como se menciona entre paréntesis en cada una, y *para aprobar es necesario contar con 65 de 114 puntos*.

*El TP ha de resolverse en grupos de 3 integrantes*. El formato de entrega consistirá de dos archivos subidos a través del campus,
- `informe-<apellido1>-<apellido2>-<apellido3>.pdf`, un informe _en formato PDF_ páginas contestando las preguntas teóricas, y
- `codigo-<apellido1>-<apellido2>-<apellido3>.R`, un _script_ de R con las respuestas a las preguntas de código, siguiendo estrictamente el formato requerido en cada una.
Tanto el informe como el código se evaluarán con especial énfasis en la *claridad y concisión de exposición, y prolijidad en la presentación*. El código, además, se evaluará de manera automática, a través de una serie de casos de prueba secretos (pero análogos a los que se ofrecen en cada pregunta), que deberán ejecutarse con éxito.

Los gráficos que se piden, deben ser incluidos en el _informe_, y no es necesario incluir el código utilizado para realizarlos.

Hemos decidido eliminar la restricción de longitud, pero por favor, eviten usarlo como licencia de corso para la perorata.

Para el informe pueden usar el procesador de texto que deseen, aunque sugerimos utilizar formatos amigables a la expresión científica, como LaTeX #footnote[Si no conocen un buen editor, #link("https://www.overleaf.com/")[Overleaf] es una excelente primera opción], #link("https://rmarkdown.rstudio.com/")[RMarkdown] o #link("https://typst.app/")[Typst] #footnote[¡Este TP está escrito en Typst!].

La idea del trabajo es que _aprendan unos temas poco comunes_, no que sufran: si no contestan todas las preguntas, está bien. Si quieren saltearse algunas en una primera pasada y volver más tarde, también.

¡Mucha serte!
#pagebreak()

== Bibliografía #emoji.books
Lamentablemente la enorme mayoría de la bibliografía de calidad de estos temas está en inglés. A quien se le dificulte la lectura, le recomendamos acudir a cualquier buen traductor o _chatbot_ respetable para asistirlo en el proceso.

=== OOP en R
Esta exposición está recortada arbitrariamente y traducida al castellano de "#link("https://adv-r.hadley.nz/")[Advanced R]", de Hadley Wickham y equipo, que recomiendo enfáticamente en su totalidad para quienes deseen profundizar sus conocimientos de R. En particular, les sugerimos leer:
- Cap. 12 - Tipos Base
- Cap. 13 - S3 hasta 13.5 "Object Styles" inclusive

Si no tienen ninguna noción de R más allá de "hice unas cositas sueltas para IECD", recomendamos además leer someramente los capítulos 2 ("Nombres y Valores") y 3 ("Vectores").

Un recurso un poco más viejo pero repleto de amor y odio por las particularidades de R es #link("https://www.burns-stat.com/pages/Tutor/R_inferno.pdf")[R Inferno]; en el Séptimo Círculo, "Tripping on Object
Orientation", cubre someramente y con perpsectiva histórica estos mismos temas.

==== Lectura de verano #emoji.sun #emoji.beach
Para quien desee lectura de verano, los siguientes libros disponibles _online_ son de extrema utilidad para el cientista de datos profesional:
- #link("https://rstudio-education.github.io/hopr/index.html")[Hands-on Programming With R], una excelente guía general al lenguaje, y luego
- #link("https://es.r4ds.hadley.nz/")[R para Ciencia de Datos (¡en español!)] y su #link("https://r4ds.had.co.nz/")[original en inglés], más enfocados en las particularidades de la _data saiens_.

=== Test de Wilcoxon de Rango Signado 
Una introducción somera se puede encontrar en Wikipedia:
 #link("https://en.wikipedia.org/wiki/Wilcoxon_signed-rank_test")[Wilcoxon signed-rank test]. El recurso canónico para tests basados en rangos, es "Statistical Inference Based on Ranks", de Thomas P. Hettmansperger (§2, p. 29, 1984, #link("https://library.lol/main/BEA0BC09D09E879F353F9E70CC859168")[pdf, 6MB]). En este último está basada la exposición que sigue. Además,
- El "libro de recetas de cocina" #footnote[en inglés, _cookbook_, un texto de referencia para practicantes con buenos ejemplos y escasa teoría] por excelencia es _Practical Nonparametric Statistics_, de W.J. Conover (p.352, 1999, #link("https://libgen.is/book/index.php?md5=1BD407E5D82A4F7F87876562491B4337")[djvu - 9MB], #link("https://libgen.is/book/index.php?md5=BD9C08D7983A16854936E94119F77869")[pdf, 124MB]).
- Un recurso más moderno del mismo Thomas Hettmansperger, es _Robust Nonparametric Statistical Methods_, de T.P. Hettmansperger y J.W. McKean (p. 38, 2010, #link("https://libgen.is/book/index.php?md5=81FBA7F57E7C4485B6B33654CAE17D99")[pdf, 5MB]).



== Notación <notacion>
- $[n] = {1, dots, n}$ es el conjunto de los primeros $n$ números naturales
- $C = A slash B = {x : x in A and x in.not B}$ es la operación de sustracción de conjuntos,
- la *negrita* ($vv(X), vv(a), vv(1)$) denota valores vectoriales, y fuente "normal" para escalares
- usamos mayúsculas ($vv(Y), Z$) para elementos aleatorios y minúsculas ($y, vv(a)$) para  no-aleatorios  #footnote[sean conocidas o no].
- $ind(A)$ es la  función indicadora, que vale $0$ cuando $A$ es Falso y $1$ cuando $A$ es Verdadero
- Dado un conjunto $A$, $\# A$ denotará su cardinalidad
- $X bot Y$ indica que las v.a. (potencialmente multivariadas) $X$ e $Y$ son independientes entre sí 



#pagebreak()

== OOP en R


=== Breve intro opinionada 
Que los computólogos me juzguen por las barbaridades que estoy por decir. Muy resumidamente, en el paradigma de "Programación Orientada a Objetos", un programa se compone de una serie de interacciones entre _objetos_, que además de poseer ciertos _atributos_, tiene _métodos_ que les permiten interactuar entre sí y con otras _clases_ de objetos.

En R, hay no una sino al menos 3 "dialectos" para expresarse "en objetos": S3, S4 y R6 #footnote[Wickham explica bien los orígenes de c/u en los capítulos antedichos.], de los cuales "S3" es el (a) el más viejo, y (b) el que está _por todas partes_ en la implementación base de R que todos amamos y sufrimos por partes iguales. Por ello, *nos concentraremos en S3*.

En OOP, un _objeto_ es una _instancia_ de una _clase_ general que se comporta de cierta manera. R tiene una implementación espectacularmente simple de esta convención:

#defn("Objeto en S3")[ Un objeto es una _estructura_ con un atributo de nombre `class` cuyo valor define la clase del objeto.
```R
> objs <- list(mtcars, 1:5, sum, lm(mpg ~ cyl, mtcars), t.test)
> for (obj in objs) { print(class(obj)) }
[1] "data.frame"
[1] "integer"
[1] "function"
[1] "lm"
[1] "function"
```
]

#prg("3 pts.")[¿Qué clase tienen los siguientes vectores: `c(T, F)`, `c(T, F, 1)` y `c(T, F, 1, "1")`? ¿Qué cree que está sucediendo?]
#rtao[Los vectores tienen clase (o "tipo") `logical`, `numeric` y `character`, respectivamente. 
#quote(attribution: link("https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/c")[`help(c)`])[_The output type is determined from the highest type of the components in the hierarchy NULL < raw < *logical* < integer < *double* < complex < *character* < ..._.
]
#v(0.5em)
- Como `T(RUE)` y `F(ALSE)` son ambos de tipo lógico, el primer vector es lógico.
- Como `1` es `numeric` (para que sea un entero habría que escribir `1L`), el vector entero es de tipo `numeric` (o `double`), el tipo del componente más alto en la jerarquía. 
- Finalmente, como `"1"` es una cadena de caracteres, el vector entero será de tipo `character`.
]

Una estructura puede ser _cualquier cosa_, prácticamente, y el atributo se setea con la sintaxis clásica.
```R
> lucas <- 1:5
> class(lucas)
[1] "integer"
> attr(lucas, "class") <- "pato"
> class(lucas)
[1] "pato"
```

Una manera más común de definir la clase de un objeto, es usar el constructor `structure`, que hace literalmente lo que necesitamos, asignarle atributos arbitrarios a un objeto cualquiera.
```R
> donald <- structure(6:10, class="pato")
> class(donald)
[1] "pato"
```
¿Es peligrosa esta filosofía? Sí y no: _puede serlo_, pero sólo si insistimos en asignare clase "petunias" al método `mean` o clase "pato" a un vector de enteros, cosas por el estilo. Aquí, la filosofía de R es "mientras no te taladres los pies, un taladro es una herramienta y no un arma": exponer las "entrañas" del lenguaje tan abiertamente, hace posible que la comunidad de desarrolladores implemente nuevas clases y métodos con un mínimo de conocimiento sobre sus convenciones.

=== Métodos "genéricos"
En R, casi todos los métodos de "base" son _genéricos_, que pueden adaptar su comportamiento según la _clase_ del primer parámetro que reciben. La convención, _a grosso modo_, dice que cuando se llama un método genérico (como `print` #footnote[De `help("print")`:  "`print` prints its argument and returns it invisibly (via `invisible(x)`). *It is a generic function which means that new printing methods can be easily added for new classes*." ]) con primer argumento `obj`, `print(obj)`, R averigua la clase del objeto, `cls <- class(obj)`, y chequea si está definido el método `print.cls`,
  - Si lo está, devuelve `print.cls(obj)`, y
  - si no, "sigue la cadena de herencia" hasta encontrar una coincidencia o salir por la versión `default`.

Luego, podemos tomar confusas decisiones de diseño, que funcionan de pelos:
```r
> print.pato <- function(pato) { "cuac" }
> print(lucas)
[1] "cuac"
```

`sloop::s3_dispatch(llamada)` #footnote[`sloop`, de "S Language OOP", es una librería desarrollada por Wickham et al para siplificar el trabajo con los sistemas de clases en R. Como a cualquier librería, a `sloop` se la instala con `install.packages("sloop")` y se la importa con `library(sloop)`. ] devuelve una sinopsis de cómo S3 "despachó" la llamada `llamada` al de la clase correspondiente, según su cadena de herencia:

```R
> sloop::s3_dispatch(print(lucas))
=> print.pato
 * print.default
> s3_dispatch(mean(1:5))
   mean.integer
   mean.numeric
=> mean.default
```
De igual manera se consigue que `plot(density(1:500))` "automágicamente" plotee la estimación de la densidad por núcleos, sin más:
```R
> s3_dispatch(plot(density(1:500)))
=> plot.density
 * plot.default
```

#prg("3 pts.")[¿Qué clase tiene `density`? ¿Y `density(1:500)`? ¿Dónde está la diferencia?]
#rtao[`density` tiene clase `function`, mientras que `density(1:500)` tiene clase `density`. La documentación es clara: `density` es una función (`function`), y el valor que retorna (que se obtiene al invocarla, con o sin parámetros),es un objeto de clase `density`). De la ayuda,
#quote(quotes:false, attribution: link("https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/density")[`help(density)`])[
  ==== Description
  The (S3) generic *function* density computes kernel density estimates.
  ==== Value
If `give.Rkern` is true, the number $R(K)$ #footnote[el "ancho de banda canónico"], otherwise *an object with class "density"* whose underlying structure is a list containing ...
]
]
==== Introspección: `methods`
Para conocer los métodos a los que sabe despachar cierto genérico `gen`, basta con llamar a `methods("gen")`. Si se quiere conocer todos los métodos asociados con la clase `"cls"`, se invoca `methods(class="cls")`:
```r
> methods("plot")[1:8]
[1] "plot,ANY-method" "plot,color-method" "plot.acf" "plot.data.frame"   
[5] "plot.decomposed.ts" "plot.default" "plot.dendrogram" "plot.density"      
> methods(class="density")
[1] coerce      initialize  plot        print       show        slotsFromS3
see '?methods' for accessing help and source code
```

Las funciones `sloop::s3_methods_generic(gen)` y `s3_methods_class(cls)` retornan la misma información, ordenada más sistemáticamente.

#prg("3 pts.", numbering: "1")[¿A cuántas clases sabe despachar el genérico `print`? ¿Con cuántos métodos cuenta `density`, además de `plot`?]
#rtao[¡Pregunta trampa accidental! En R "base", `print` sabe despachar a 230 (!) clases distintas, lo que se puede obtener evaluando `length(methods("print"))`. Ahora bien, si importamos `sloop` veremos que ahora `nrow(s3_methods_generic("print"))` #footnote[Como `s3_methods_generic` devuelve un "data frame", la cantidad de filas se obtiene con `nrow` y no `length`.] devuelve 248, y `length(methods("print"))` también, porque la librería importa también sus propias clases, que sabe imprimir proficientemente. Si seguimos importando librerías, el número puede seguir creciendo.

Según `methods(class="density")`, la _clase_ `density` (no la función) tiene seis métodos disponibles: `print, plot, coerce, show, initialize, slotsFromS3`. Según `s3_methods_class("density")`, sin embargo, sólo cuenta con 2: `print, plot`. La diferencia radica en que los otros 4 son una especie de "meta-métodos", dsponibles para todas las clases de S3 (y S4) para manipularse a sí mismas, y `sloop` no los considera "propios" de una clase específica.



]
=== Fijando ideas: `mi.t.test`

Supongamos que contamos con una muestra $vv(X) = (X_1, dots, X_n)$ de tamaño $n$ con distribución $X_i ~^"iid" "Normal"(mu, sigma^2)$ y deseamos testear 
$ H_0 : mu = mu_0 quad "versus" quad H_1 : mu != mu_0 $

Para fijar ideas, supongamos que para nosotros tanto $mu$ como $sigma^2$ son desconocidos cuando en realidad, $X_i ~^"iid" "Normal"(1, 1)$, contamos con $n = 30$ y nos interesa testear $mu_0=0, alpha = 0.05$. En estas circunstancias, el "test T" es el adecuado. En R,  
```r
# Genero la muestra
mu <- 1
sigma_sq <- 1
n <- 30
X <- rnorm(n, mean = mu, sd = sqrt(sigma_sq))
# Ejecuto el test
mu_0 <- 0
alfa <- 0.05
test_t <- t.test(
  X,
  alternative = "two.sided",
  mu = mu0,
  conf.level = 1 - alfa
)
```
#prg("3pts.")[ Lea `help(unclass)` y conteste: ¿Qué devuelve `class(unclass(test_t))`?¿Por qué?]
#rtao[ `class(unclass(test_t))` devuelve *`list`*, pues
#quote(attribution: link("https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/class")[`help(unclass)`])[`unclass` returns (a copy of) its argument with its class attribute removed. ]
Al quitarle el atributo `class="htest"`, nos queda el "objeto pelado" sobre el que se construyó un objeto de tipo `htest`, que resulta ser una lista como se ve - una vez más - en la ayuda:
#quote(quotes: false, attribution: link("https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/t.test")[`help(t.test)`])[
  ==== Value
A *list* with class "`htest`" containing the following components: ...]
]

No es muy difícil reimplementar la lógica detrás de un test T a dos colas como ésta con los conocimientos adquiridos este cuatrimestre. Respetando la convención de nombres de la salida de `t.test`, nos queda:

```r
mi.t.test <- function(x, mu0 = 0, alfa = 0.05) {
  n <- length(x)
  parameter <- n - 1
  estimate <- mean(x)
  stderr <- sd(x) / sqrt(n)
  statistic <- (estimate - mu0) / stderr
  conf.int <- estimate + qt(c(alfa / 2, 1 - alfa / 2), df = parameter) * stderr
  p.value.izq <- pt(statistic, df = parameter)
  p.value <- 2 * min(p.value.izq, 1 - p.value.izq)
  list(
    parameter=parameter,
    estimate=estimate,
    stderr=stderr,
    statistic=statistic,
    conf.int=conf.int,
    p.value=p.value
  )
}
```

La desgracia, es que el `t.test` de R tiene una presentación por defecto bastante informativa:
```R
> t.test(X)

	One Sample t-test

data:  X
t = 4.6001, df = 29, p-value = 7.697e-05
alternative hypothesis: true mean is not equal to 0
95 percent confidence interval:
 0.4391649 1.1422807
sample estimates:
mean of x 
0.7907228 
```
... y mucho mejor que la de nuestro test:

```R
> (mi_test_t <- mi.t.test(X))
$parameter
[1] 29

$estimate
[1] 0.7907228

$stderr
[1] 0.1718916

$statistic
[1] 4.600124

$conf.int
[1] 0.4391649 1.1422807

$p.value
[1] 7.697055e-05
```

¿Será que `t.test` es instancia de una clase que `print` entiende? ¡Pues claro!
```R
> help("s3_dispatch")
> s3_dispatch(print(R_test_t))
=> print.htest
 * print.default
> s3_dispatch(print(mi_test_t))
   print.list
=> print.default
```

En lugar de escribir de cero una función específica de `print` para `mi.t.test`, podemos pararnos en los hombros de gigantes. Le daremos a `mi.t.test` la clase de `t.test`, y veremos de respetar sus convenciones, de manera que podamos utilizar la ya bien pulida `print.htest` #footnote[que dicho sea de paso, considera _unos cuantos casos_: #link("https://github.com/SurajGupta/r-source/blob/a28e609e72ed7c47f6ddfbb86c85279a0750f0b7/src/library/stats/R/htest.R#L39")[link al código]]. Vamos de nuevo:

```r
is.scalar <- function(x) { is.numeric(x) && length(x) == 1 }
mi.t.test <- function(x, mu = 0, conf.level = 0.95) {
  stopifnot(is.numeric(x))
  stopifnot(is.scalar(mu))
  stopifnot(is.scalar(conf.level), (conf.level > 0), (conf.level < 1))
  alfa <- 1 - conf.level
  n <- length(x)
  rv <- list(
    parameter = c(df = n - 1),
    estimate = c(`mean of x` = mean(x)),
    stderr = sd(x) / sqrt(n),
    null.value = c(mean = mu),
    alternative = "two.sided",
    method = "One Sample t-test",
    # Stack Overflow: How to convert variable (object) name into String
    # https://stackoverflow.com/a/14577878
    data.name = deparse(substitute(x))
  )
  rv$statistic <- setNames((rv$estimate - mu) / rv$stderr, "t")
  rv$conf.int <- rv$estimate + qt(c(alfa / 2, 1 - alfa/2), df = rv$parameter) * rv$stderr
  attr(rv$conf.int, "conf.level") <- conf.level
  pval_izq <- pt(rv$statistic, df = rv$parameter)
  rv$p.value <- 2 * min(pval_izq, 1 - pval_izq)
  structure(rv, class = "htest")
}
```
Y ahora resulta que:
```R
> mi.t.test(X)

	One Sample t-test

data:  X
t = 4.6001, df = 29, p-value = 7.697e-05
alternative hypothesis: true mean is not equal to 0
95 percent confidence interval:
 0.4391649 1.1422807
sample estimates:
mean of x 
0.7907228 

> stopifnot(capture.output(t.test(X)) == capture.output(mi.t.test(X)))
```

#obs(`stopifnot`)[ Cuando uno desea "testear"" condiciones en medio de un programa, `stopifnot` es sumamente útil: recibe varias expresiones, y si alguna _no_ evalúa a `TRUE`, devolverá un error. Aquí arriba, nos dice que el output de `t.test` y `mi.t.test` son exactamente iguales.]

== Test de Wilcoxon de rango signado para una muestra

=== Introducción
Cuando "la distribución $F$ de $X$ pertenece a la familia normal $cal(N)$" #footnote[es decir, que $X_i ~ F in cal(N) = {F_X : X ~ "Normal"(mu, sigma^2), thick mu in RR, sigma^2 in (0, oo)}$. Diremos indistintamente que $X$ ó $F$ pertenecen a la familia $cal(N)$], el test T que vimos durante la cursada es uniformemente más potente para hipótesis de la forma:
 $
   H_0: mu <= mu_0 quad "versus" quad H_1: mu > mu_0 \
   H_0: mu >= mu_0 quad "versus" quad H_1: mu < mu_0
 $
 cunando $sigma^2$ es desconocido. En el "mundo real", el supuesto de normalidad es una hipótesis sumamente ceñida, en tanto consigna la distribución de $X$ a un _modelo paramétrico_ específico.

Una familia bastante amplia de distribuciones, está dada por "el conjunto de distribuciones absolutamente continuas, con mediana igual a 0"

$ Omega_0 = {F: F "absolutamente continua", F(0) = 1/2} $

Cuando $X ~ G(x) = F(x-theta), F in Omega_0$, el "test del signo" #footnote[confer Práctica 5, ejercicio 22], resulta ser uniformemente más potente para testear la mediana ($G(theta) = F(theta - theta) = F(0) = 1/2$) según $ H_0: theta <= 0 thick "versus" thick H_1: theta > 0 $
, aunque es cierto que no existen _muchos_ tests de nivel dado para esta familia tan amplia. Esta clase de tests, se considera "no paramétricos" #footnote[_nopa_, para los amigos], en tanto la familia de distribuciones en la que funcionan _no admiten obvias parametrizaciones_.

Una familia "a medio camino" entre $cal(N)$ y $Omega_0$, es el de las distribuciones simétricas:

#defn("distribución simétrica")[una v.a. $X$ con densidad $f$ se dice "simétrica alrededor de $theta$" si $f(theta + delta) = f(theta - delta) thick forall delta > 0$]

Esto nos permite definir $Omega_s subset Omega_0$, el conjunto de las distribuciones simétricas alrededor del 0:
$
  Omega_s = {F:F in Omega_0, F(t) = 1 - F(-t) forall t in RR}
$

Cuando $X ~ F in Omega_0$, decimos que "$X$ (o $F$) es simétrica alrededor del cero". Ahora, $vv(X)= (X_1, dots, X_n)$ será una muestra aleatoria tomada de $G(x) = F(x - theta), F in Omega_s$, donde la mediana es única, está bien definida y es igual a $theta$. Sin pérdida de generalidad, nos interesarán, entonces, tests de la forma
$
  H_0: theta <= 0 quad "versus" quad theta > 0 \
  H_0: theta >= 0 quad "versus" quad theta < 0
$

#obs[Para testear $H_0: theta <= theta_0$, basta con definir $Y_i = X_i - theta_0$ y realizar los test definidos aquí arriba sobre $vv(Y)$]

#obs[ Si definimos $cal(N) = {F_X: X ~ "Normal"(0, sigma^2), sigma^2 > 0} arrow.r.double cal(N) subset Omega_s subset Omega_0$.]

Wilcoxon (1945, #link("https://sci2s.ugr.es/keel/pdf/algorithm/articulo/wilcoxon1945.pdf")[link]) planteó un test bastante ingenioso para estas situaciones, que (aunque no lo probaremos), resulta ser uniformemente más potente para distribuciones en $Omega_s$. 

=== Motivación: diseños experimentales apareados

Existen casos completamente válidos en los lo único que sabemos acerca de una distribución es que es simétrica, y nos interesa testear su mediana. Dicho esto, existe un escenario muy común donde la distribución bajo la hipótesis nula pertenece a $Omega_s$.

Consideremos la situación en la que tenemos dos tratamientos de interés, $A$ y $B$ #footnote[Entendido de forma amplia, un placebo - o cualquier otro procedimiento de referencia o "control" - también es un tratamiento, y este mismo _setup_ permite describir diseños del tipo "tratamiento / control".], que se pueden aplicar a sujetos de una población de interés, y estamos interesados ​​en una respuesta particular después de que se hayan aplicado estos tratamientos.

Sea $X$ la respuesta de un sujeto después de que se le haya aplicado el tratamiento $A$ y sea $Y$ la medida correspondiente para un sujeto después de que se le haya aplicado el tratamiento $B$. La hipótesis nula natural será
$
 H_0&: "No hay diferencia en la distribución de " X " e " Y. \
 "es decir, " H_0&: F_X = F_Y
$

Supongamos que tenemos una manera de aparear los sujetos de un estudio. Por ejemplo, disponemos de gemelos idénticos para un estudio en sujetos humanos, compañeros de camada para un estudio en sujetos animales o las dos mitades de una misma pared exterior de una casa para un estudio sobre la durabilidad de pinturas de exterior. En el _diseño por pares_ (o apareado), se seleccionan aleatoriamente $n$ _pares_ de sujetos de la población de interés. Dentro de cada par, un miembro se asigna aleatoriamente al tratamiento A mientras que el otro recibe el tratamiento B.

Este diseño experimental da como resultado una muestra de pares $(X_1 , Y_1), dots , (X_n , Y_n)$. A pesar de que este experimento tiene un vector de respuestas de dimensión dos, el interés está puesto en las diferencias obtenidas: $D_1 = X_1 − Y_1 , dots, D_n = X_n − Y_n$ , y las $D_1,...,D_n$ se convierten en la _única_ muestra de interés para decidir si los tratamientos se diferencian significativamente.

Bajo la hipótesis nula de que no hay diferencia en el tratamiento (es decir, que la distribución de $X$ es la misma que la distribución de $Y$) junto con la asignación aleatoria dentro de cada par a recibir el tratamiento $A$ o el $B$ obtenemos  una distribución simétrica de las diferencias.

#prg("5 pts.")[Bajo $H_0: F_X=F_Y$ (los tratamientos son indistinguibles) y asumiendo que la asignación de cada individuo al tratamiento se realiza de forma aleatoria, la distribución conjunta $F_(X,Y)$ del vector aleatorio $(X,Y)$ es la misma que la del vector $(Y,X)$; es decir, $F_(X,Y)= F_(Y,X)$. Probar que entonces la distribución de $D=X-Y$ es simétrica alrededor del cero.]

#rtao[
  Si $F_(X,Y)= F_(Y,X)$, se sigue que $Pr(X <= x, Y <=y) = Pr(Y <= x, X <= y)$ y por lo tanto $X, Y$ son variables aleatorias intercambiables. Luego,

    $
      F_D (d) &= Pr(D <= d) = Pr(X - Y <= d) = Pr(Y - X <= d) \
      &= Pr(-d <= X - Y) = Pr(-d <= D) = 1 - Pr(D <= -d) \
       &= 1 - F_(D)(-d)
    $

  y por ende $F_D in Omega_s$, y D está simétrica distribuida alrededor del 0).
]

Habiendo expuesto razonablemente la relevancia de la familia $Omega_s$, en particular en el contexto de evaluación empírica de "tratamientos" en diseños muestrales "apareados", pasemos a describir el test de Wilcoxon en sí.

=== Descripción del test
De aquí en más, consideraremos únicamente el escenario de una sola muestra $X$, con distribución  $G$ simétrica.

Consideremos una muestra aleatoria $vv(X) = (X_1, dots, X_n), thick X_i~^("iid") G(t) = F(t - theta)$. Deseamos encontrar un test para la "locación" #footnote[En distribuciones simétricas, las dos locaciones clásicas, media y mediana, coinciden.] $theta$.

$ H_0 : theta = 0 quad "versus" quad H_1 : theta > 0 $

, con $F in Omega_s$.

Antes de introducir el estadístico a emplear, definiremos algunas funciones:
#defn([Función signo])[
$ "signo"(x) : RR -> {-1, 0, +1}, "signo"(x) = cases(-1 thick &"si" thick x < 0, 0 thick &"si" thick x = 0, +1 thick &"si" thick x > 0) $
]
#defn([Función Rango])[
  Sea $vv(X) = (X_1, dots, X_n)$ una muestra aleatoria, y $X^((1)), dots, X^((n))$ la misma muestra, ordenada en forma no decreciente. Llamaremos el _rango_ #footnote[como en el _rango_ militar, donde "general" está por encima de "capitán", que está por encima de "oficial", etc.] de $X_i$, al índice de la posición que ocupa en la muestra ordenada:
  $
    R_i = "Rango"(X_i | vv(X)) = j iff X_i = X^((j))
  $
]

#obs[Una manera de calcular el rango de una observación, es 
$
  R_i = \#{X : X <= X_i, X in vv(X)} = sum_(j=1)^n ind(X_j <= X_i)
$]

Es decir, la función Rango toma como input un vector $vv(x)$ , y devuelve otro vector $vv(r)$ que es una permutación de $[n] = {1, dots, n}$, donde el i-ésimo elemento indica la contidad de elementos de $vv(x)$ menores o iguales a $x_i$, o lo que es lo mismo, su posición ordinal.


Llamemos $|vv(X)| = (|X_1|, |X_2|, dots, |X_n|)$ al vector de valores absolutos de $vv(x)$, y $R_(i) = "Rango"(|X_i|)$ el rango de $|X_i|$ en $|vv(X)|$. Ahora sí, podemos presentar una primera versión del estadístico del test de rangos signados de Wilcoxon:

$ T(vv(X)) = sum_(i=1)^n "signo"(X_i) R_i $

donde por ser $X_i$ v.a. absolutamente continuas e independientes entre sí, 
$ Pr(X_i=0)&=0 thick forall i in [n] \
Pr(X_i=X_j) &= 0 thick forall thick i, j in [n], i!=j $

de manera que no hay empates ni $"signo"(X_i)=0$.

#prg("3 pts.")[Muestre que los siguientes estadísticos:

$ TT &= sum_(i=1)^n ind(X_i>0) R_i \
T^- &= sum_(i=1)^n ind(X_i<0) R_i $

son equivalentes a $T$ (i.e., muestre que a partir de cualquiera de los 3 y conociendo $n$, se pueden computar exactamente los otros dos).
_Sugerencia: calcule $TT + T^-$_] <equivalencia-T-Tmas-Tmenos>

#rtao[ Sabemos que necesariamente, $sum_(i=1)^n R_i = sum_(i=1)^n i = n(n+1)/2$. Luego,
$
  TT + T^- &=  sum_(i=1)^n ind(X_i>0) R_i +
 sum_(i=1)^n ind(X_i<0) R_i \
 &= sum_(i=1)^n ind(X_i>0) R_i + ind(X_i < 0) R_i = sum_(i=1)^n  R_i \
 TT + T^- &= n(n+1)/2 
$
Además, $T = TT - T^-$, así que para todo $n$ y valor observado $t$ de #TT,
$
  TT = t; quad  T^- = n(n+1)/2 - t; quad T = 2t - n(n+1)/2
$
]


De las tres formas, la que más comúnmente se usa para definir el test, es #TT; será la que consideremos de aquí en más. Nuestro test será de la forma
$
  phi(vv(X)) = ind(TT > k)
$
, de manera rechazaremos la hipótesis nula cuando la suma de los rangos de los $X_i > 0$ sea lo suficientemente grande, dándole peso a la hipótesis alternativa de que $theta > 0$.

Para facilitar el estudio de la distribución de #TT bajo $H_0$, introduciremos una última - lo juro - variante en la notación.

#defn("Antirrango")[Sean $R_1, dots, R_n$ los rangos correspondientes a un vector $vv(m) = m_1, dots, m_n$, o sea que $R_i = j iff m_i = m^((j))$. Diremos entonces que el $j$-ésimo _antirrango_ es igual a $i$. En otras palabras, el antirrango es "la inversa" del rango: $D_j = i$ si el $j$-ésimo elemento de la muestra ordenada es el $i$-ésimo en la muestra original:
$ D_j = i iff R_i = j $]

Por ejemplo, si $vv(m) = (m_1, m_2, m_3), thick "con" m_2 < m_3 < m_1 $, resulta que
#columns(2)[$
R_1 &= R(m_1 | vv(m)) = 3 \
R_2 &= R(m_2 | vv(m)) = 1 \
R_3 &= R(m_3 | vv(m)) = 2
$
#colbreak()
$
D_1 &= 2 " pues " m^((1)) = m_2 \
D_2 &= 3 " pues " m^((2)) = m_3 \
D_3 &= 1 " pues " m^((3)) = m_1
$
]

Habiendo definido los antirrangos, podemos reescribir el estadístico #TT de la siguiente manera:

$ TT = sum_(i in [n]) ind(X_i>0) R_i = sum_(D_j in [n])ind(X_(D_j)>0) R_(D_j) = sum_(j in [n]) W_j times j $

donde $W_j = ind(X_(D_j) > 0) = ("signo"(X_(D_j)) + 1 )/ 2 $ y por definición, $R_(D_j) = R(|X_(D_j)|) = j $.

=== Distribución de #TT bajo la hipótesis nula

Ahora sí estamos en condiciones de estudiar la distribución de #TT bajo $H_0$.

#prg("5 pts.")[Demuestre que  bajo $H_0$, $|X_i|$ es independiente de $"signo"(X_i)$.

 _Sugerencia: utilice sus conocimientos sobre $F_X$ bajo $H_0$._] <indepcia-signo-modulo>

#rtao[
  
  Escribamos "la distribución conjunta" de $|X_i|$ y $"signo"(X_i)$ como $g(x, s) = Pr(|X_i| <= x, "signo"(X_i) = s)$. Como $F_X in Omega_s arrow.r.double F_X (0) = 1/2, F_X (x) = 1 - F_X (-x)$. Sin pérdida de generalidad, asumamos un signo positivo:
$
  g(x, 1) &= Pr(|X_i| <= x, "signo"(X_i) = 1) = Pr(-x <= X_i <= x, X_i > 0) \
  &= Pr(0 < X_i <= x) = F_X (x) - F_X (0) = F_X (x) - 1/2 \
  &= 1/2 [2 F_X (x) - 1] = 1/2 [F_X (x) - (1 - F_X (x))] = 1/2 [F_X (x) - F_X (-x)] \
  &= Pr("signo"(X_i)=1) times Pr(|X_i| <= x) 
$
Un resultado análogo muestra que $g(x, -1)$ también se factoriza en $Pr("signo"(X_i)=-1)$ y $Pr(|X_i| <= x)$, con lo cual la probabilidad conjunta de $|X_i|$ y $"signo"(X_i)$ es _siempre_ igual al producto de sus probabilidades marginales, y por ende  son independientes.]

#prg("3 pts.")[A partir del resultado anterior, pruebe que bajo $H_0$ los vectores de rangos $vv(R) = (R_1, dots, R_n)$ y antirrangos $vv(D) = (D_1, dots, D_n)$ correspondientes a $|vv(X)|$ son independientes del vector de signos $vv(S) = ("signo"(X_1), dots, "signo"(X_n))$ de la muestra original $vv(X)$.]
#rtao[Consideremos los vectores
$
vv(abs(X)) &= (abs(X_1), dots, abs(X_n)) \
vv(S) &= (S_1, dots, S_n) = ("signo"(X_1), dots, "signo"(X_n)) \
vv(R) &= (R_1, dots, R_n) = ("Rango"(abs(X_1)), dots, "Rango"(abs(X_n))) \
"y " vv(D) &= (D_1, dots, D_n) = ("Antirrango"(R_1), dots, "Antirrango"(R_n)) \
$
1. Por definición, tanto $S_i$ como $abs(X_i)$ sólo dependen de $X_i$. Luego, para todo $i != j, thick i, j in [n]$, sabemos que (a) $S_i bot abs(X_j)$. Además, por la respuesta a la  @indepcia-signo-modulo, sabemos que (b) $S_i bot abs(X_i)$. Conjuntamente, (a) y (b) implican que todos los elementos de $vv(S), vv(abs(X))$ son independientes "uno con uno", y por ende $vv(S) bot abs(vv(abs(X)))$.
2. El rango $R_i$ del módulo de la i-ésima observación $abs(X_i)$ sí que depende de los valores de $abs(X_j), thick j != i$, _pero_ al considerar el vector completo, $vv(R)$ _sólo depende de_  $abs(vv(X))$ a través de alguna función $f$. Como $vv(S) bot abs(vv(abs(X)))$ y $vv(R) = f(abs(vv(X)))$. Por lo tanto, se sigue que $vv(S) bot vv(R)$.
3. Análogamente a (2), el vector de antirrangos $vv(D)$ es una función del vector $vv(R)$:
$
  vv(D) = g(vv(R)) = g(f(vv(abs(X))))
$, y sólo dependen de $abs(vv(X))$ vía $vv(R)$. Como $vv(S) bot abs(vv(abs(X)))$ , entonces también $vv(S) bot vv(D)$.


]

#prg("5 pts.")[Pruebe que bajo $H_0: theta = 0, F in Omega_s$, las v.a. $W_j = ind(X_(D_j) > 0)$ distribuyen según
$ W_1, dots, W_n ~^("iid") "Bernoulli"(1/2) $
_Sugerencia: Utilice la #link("https://es.wikipedia.org/wiki/Teorema_de_la_probabilidad_total")[Ley de la Probabilidad Total] de $vv(W)$ sobre los posibles valores de $vv(D)$, y utilice los resultados previos para operar y factorizar $Pr(vv(W) = vv(w)) = product_(j in [n]) Pr(W_j = w_j)$._] <prg-dist-W>

#rtao[
  Fijando sin pérdida de generalidad $j=1$, es fácil dar con la distribución de $W_1$, por ejemplo:
  $
    Pr(W_1 = 1) &= Pr({X_(D_1) > 0} = 1) = Pr(X_(D_1) > 0) = 1/2 \
    Pr(W_1 = 0) &= Pr({X_(D_1) > 0} = 0) = Pr(X_(D_1) < 0) = 1/2 \
  $
  ya que $X_i$ es simétrica alrededor de 0 para todo $i in [n]$. Esto alcanza para mostrar que las $W_j$ están _idénticamente_ distribuidas. 
  Como $vv(W)$ depende tanto de $vv(abs(X))$ como de $vv(D)$, la independencia es un poco más delicada.
  
   Sean $vv(w) = (w_1, dots, w_n) in {0, 1}^n$. Llamemos $cal(P)([n])$ al conjunto de permutaciones de los primeros $n$ naturales $[n]$: éste es el _soporte_ de $vv(D)$ #footnote[el conjunto exhaustivo de valores que puede tomar $vv(D)$]. Luego, bajo $H_0: theta = 0, F in Omega_s$, tenemos que
  $
    Pr(vv(W) = vv(w)) &= sum_(vv(d) in cal(P)([n]))  Pr(vv(W) = vv(w) | vv(D) = vv(d)) times Pr(vv(D) = vv(d)) \
    &= sum_(vv(d) in cal(P)([n])) Pr({X_(D_1) > 0} = w_1, dots, {X_(D_n) > 0} = w_n | vv(D) = vv(d)) times Pr(vv(D) = vv(d)) \
    &= sum_(vv(d) in cal(P)([n])) Pr({X_(d_1) > 0} = w_1, dots, {X_(d_n) > 0} = w_n) times Pr(vv(D) = vv(d))
  $
  Por ser simétrica alrededor del cero, 
  $ Pr({X_(d_1) > 0} = 0) = Pr(X_(d_1) < 0) = 1/2 " y " Pr({X_(d_1) > 0} = 1) = Pr(X_(d_1) > 0) = 1/2 $
  así que $ Pr({X_(d_1) > 0} = w_1, dots, {X_(d_n) > 0} = w_n) = (1/2)^n$. Luego,
  $
    Pr(vv(W) = vv(w)) &= sum_(d in cal(P)([n])) (1/2)^n times Pr(vv(D) = vv(d)) \
    Pr(vv(W) = vv(w)) &= (1/2)^n underbrace(sum_(d in cal(P)([n])) Pr(vv(D) = vv(d)), =1) \
    Pr(vv(W) = vv(w)) &= (1/2)^n = product_(i in [n]) Pr(W_i = w_i)
  $
  y resulta que la distribución conjunta de $vv(W)$ es igual al producto de las distribuciones individuales de cada $W_i, thick i in [n]$. Luego,  los $W_i$ son independientes entre sí y la prueba está completa. 
]
De los ítems anteriores, se desprende que bajo $H_0$, *la distribución de #TT es una suma de variables aleatorias independientes, aunque no idénticamente distribuidas*.

#obs[Lo único que utilizamos para hallar la distribución bajo $H_0$ de #TT fue que $X_i ~^"iid" F(x - theta), F in Omega_s$. Como la distribución de #TT será la misma para _cualquier_ $F in Omega_s$, se dice que #TT es de _distribución libre_ ("distribution-free") bajo $H_0$]

==== Distribución exacta de #TT
Aunque la distribución de #TT no tiene forma cerrada, su cómputo exhaustivo no es particularmente difícil. Para simplificar la notación, omitiremos la dependencia de la probabilidad a $H_0$ hasta próximo aviso.

Sabemos que #TT es una v.a. discreta con soporte en los enteros desde $0$ hasta $n(n+1)/2$.  Queremos hallar
$
  p_n (t) = Pr(TT = t) = Pr(sum_(j in [n]) (W_j times j )= t)
$

Llamemos $A_(n,t) = {vv(w) : TT = t}$ al conjunto posibles valores de $vv(w) = (w_1, dots, w_n) in {0, 1}^n $ tal que 
$
 TT = sum_(j in [n]) (w_j times j) = sum_(j:w_j=1) j = t
$.
Ya sabemos de la respuesta a @prg-dist-W que 
$Pr(vv(W) = vv(w)) = 1/2^n thick forall thick vv(w) in {0, 1}^n$, y luego
$
  Pr(TT = t) = Pr(vv(w) in A_(n,t)) = (\# A_(n,t)) / 2^n
$
donde según, $\# A_t$ es la cardinalidad de A "conjunto potencia" de todas las $2^n$ combinaciones posibles de signos.

#obs[Existe una equivalencia natural entre los vectores $vv(w) = (w_1, dots, w_n)$ y los conjuntos $s(vv(w)) = {i : w_i = 1}$ que conservan únicamente los índices no-nulos de $vv(w)$, de manera que $A_(n,t)$ y $S_(n,t) = {s(vv(w)) : vv(w) in A_(n,t)}$ tienen la misma cardinalidad. Luego, podemos escribir 
$ p_n (t) = (\# S_(n,t)) / 2^n $ 

Cuando $n = 4$ por ejemplo, resulta que
#v(0.5em)
#align(center)[#table(columns: 4, align: center, table.header(
    [$t$], [$S_(4, t)$], [$\# S_(4, t)$], [$p_4 (t)$],
    [0], [${emptyset$}], [1], [1/16],
    [1], [${{1}}$], [1], [1/16],
    [2], [${{2}}$], [1], [1/16],
    [3], [${{3}, {1, 2}}$], [2], [2/16],
    [4], [${{4}, {1, 3}}$], [2], [2/16],
    [5], [${{1, 4}, {2, 3}}$], [2], [2/16],
    [6], [${{2, 4}, {1, 2, 3}}$], [2], [2/16],
    [7], [${{3, 4}, {1, 2, 4}}$], [2], [2/16],
    [8], [${{1, 3, 4}}$], [1], [1/16],
    [9], [${{2, 3, 4}}$], [1], [1/16],
    [10], [${{1,2,3,4}}$], [1], [1/16],
  ))]
] <obs-pTmas>

#prg("5pts.")[ Reproduzca la tabla de @obs-pTmas para $n=5$]

#rtao[
  #align(center)[#table(columns: 4, align: center, table.header(
    [$t$], [$S_(5, t)$], [$\# S_(5, t)$], [$p_5 (t)$],
    [0], [${emptyset$}], [1], [1/32],
    [1], [${{1}}$], [1], [1/32],
    [2], [${{2}}$], [1], [1/32],
    [3], [${{3}, {1, 2}}$], [2], [2/32],
    [4], [${{4}, {1, 3}}$], [2], [2/32],
    [5], [${{5}, {1, 4}, {2, 3}}$], [3], [3/32],
    [6], [${{1, 5}, {2, 4}, {1, 2, 3}}$], [3], [3/32],
    [7], [${{2, 5}, {3, 4}, {1, 2, 4}}$], [3], [3/32],
    [8], [${{3, 5}, {1, 2, 5}, {1, 3, 4}}$], [3], [3/32],
    [9], [${{4, 5}, {1, 3, 5}, {2, 3, 4}}$], [3], [3/32],
    [10], [${{1, 4, 5}, {2, 3, 5}, {1, 2, 3, 4}}$], [3], [3/32],
    [11], [${{2, 4, 5}, {1, 2, 3, 5}}$], [2], [2/32],
    [12], [${{3, 4, 5}, {1, 2, 4, 5}}$], [2], [2/32],
    [13], [${{1, 3, 4, 5}}$], [1], [1/32],
    [14], [${{2, 3, 4, 5}}$], [1], [1/32],
    [15], [${{1, 2, 3, 4, 5}}$], [1], [1/32],
  ))]
]
#prg("5 pts.")[Muestre que #TT es simétrica alrededor de $n(n+1)/ 4$]

#rtao[Sólo de observar las tablas, vemos que cuando $n=4$, #TT es simétrica alrededor de $5 = (4 (4 + 1)) / 4$ y cuando $n = 5$, #TT es simétrica alrededor de $7.5 = (5 (5+ 1))/4$. 

Trataremos de mostrarlo para cualquier $n$, usando que para cada conjunto de índices $s$ que suma $t$, su complemento $s^complement$ suma exactamente $n(n+1)/2 - t$, y por ende $p_n$ es simétrica alrededor de  $n(n+1)/2 slash 2$.

Tomemos un elemento $s(vv(w)) = {i : w_i = 1}$ arbitrario, llamémoslo $s_0$ y consideremos su complemento respecto a [n],
$
 s_0^complement = [n] slash s_0
$
$s_0^complement$ y $s_0$ son disjuntos, y su unión es exactamente $[n]$. Luego, si la suma de los elementos de $s_0$ es $t_0$, $s_0 in S_(n,t_0)$,
$
  sum_(i in [n]) i &= sum_(i in s_0) i + sum_(i in s^complement_0) i quad arrow.r.double quad
  n(n+1)/2 &= t_0 + sum_(i in s^complement_0) i quad arrow.r.double quad
  sum_(i in s^complement_0) i &= n(n+1)/2 - t_0
$ 
y resulta que $s_0^complement in S_(n, t_0^complement)$, con $t_0^complement = n(n+1)/2 - t_0$. Para esta equivalencia no impusimos ninguna condición a $n$ ni $t_0$, por lo cual vale para todo $s subset.eq [n]$. Se sigue entonces que 
$S_(n, t_0^complement) = {s^complement : s in S_(n, t_0)}$ y por lo tanto $\# S_(n, t_0^complement) = \# S_(n, t_0)$.

$
  p_n (t_0) = (\# S_(n, t_0)) / 2^n = (\# S_(n, t_0^complement)) / 2^n= (\# S_(n, n(n+1) / 2 - t_0)) / 2^n = p_n (n(n+1) / 2 - t_0)
$
y $p_n$ resulta simétrica alrededor de $ (t_0 + (n(n+1) / 2 - t_0)) / 2 = n(n+1) / 4 $
]
#rtao[(esbozo alternativa 1) Si se mira con cuidado, el argumento por la distribución exacta de #TT no depende en ningún momento del signo positivo; luego, un razonamiento idéntico lleva a concluir que la probabilidad puntual de $T^-$ también es $p_n (t)$. De @equivalencia-T-Tmas-Tmenos sabemos que $TT + T^- = n(n+1)/2$. Luego, $forall t in [n(n+1)/2]$,
$
  Pr(TT=t) = Pr(T^- = n(n+1) slash 2 - t) = Pr(TT = n(n+1) slash 2 - t)
$
Y resulta que TT es simétrica alrededor de $n(n+1) / 4$
]
#rtao[(esbozo alternativa 2) Usando recursión, 
- Cuando $n=0$, #TT vale 0 con probabilidad 1, y #TT es trivialmente simétrica.
- Cuando $n=1$, #TT es simétrica alrededor de $(1(1+1))/4=1/2$, pues
$
  p_1 (t) = cases(1 slash 2 &" si " t in {0, 1}, 0 &" caso contrario")
$
Si podemos probar que "si #TT es simétrica para $m$, entonces es simétrica para $m + 1$", y encontramos alrededor de qué valor se da la simetría, habremos completado la prueba. Pues bien, la fórmula recursiva que sigue sirve exactamente para ello.]
==== Fórmula recursiva para $n$ "grande"
Ya para un valor moderado como $n = 20$, $2^n = 1.048.576$: el crecimiento exponencial de los valores posibles de $vv(W)$ vuelve inconcecible el cómputo "exhaustivo" de $p_TT$. El hecho de que #TT sea simétrica simplificaría algo las cuentas, pero no demasiado. Sin embargo, existe una fórmula recursiva particularmente interesante:

Supongamos que $s in S_(n, t)$, de manera que $sum_(x in s) x = t$. O bien $n in s$, o bien $n in.not s$:
- si $n in.not s$, entonces $s subset.eq [n-1]$ y suma $t$; luego $s in S_(n-1, t)$,
- si $n in s$, entonces $s - {n} subset.eq [n-1]$ y suma $t - n$; luego $s - {n} in S_(n-1, t-n)$.

Como ambas son mutuamente excluyentes, y se cumplen para cada $s in S_(n, t)$, si llamamos $u_n (t)$ a la cardinalidad de $S_(n, t)$,
$
  u_n (t) &= u_(n - 1) (t) + u_(n - 1) (t - n)
$
Con los límites de la recursión en
$
u_0(t) &= cases(1 &"si" t=0, 0 &"caso contrario") \
u_n (t) &= 0 " si " t < 0 " ó " t > n(n+1)/2
$
Luego,
$ 
  p_n (t) &= (\# S_(n, t)) / 2^n = (u_n (t)) / 2^n \
  p_n (t) &= 1/2  [(u_(n - 1) (t)) / 2^(n-1) + (u_(n - 1) (t - n)) / 2^(n-1)] \
  p_n (t) &= 1/2 [p_(n-1)(t) + p_(n-1)(t-n)]
$

#prg("8 pts.")[Programe la rescursión $u_n (t)$ en R. Llámela `particiones`, y dele dos argumentos, $t, n$, ambos enteros.
```R
particiones <- function(t, n) {
  # su código aquí
}
```

La función debe pasar al menos los siguientes tests:
```R
stopifnot(
  particiones(t=3, n=4) == 2,
  particiones(t=24, n=12) == 67,
  particiones(t=55, n=10) == 1,
  particiones(t=45, n=30) == 1938
)
```
]
#rtao[
  ```R
particiones <- function(t, n) {
  if (n == 0) {
    return(ifelse(t == 0, 1, 0))
  } else if ((t < 0) | (t > n * (n + 1) / 2)) {
    return(0)
  } else {
    return(particiones(t, n - 1) + particiones(t - n, n - 1))
  }
}
```
]
#prg("8 pts.")[
Usando `particiones`, implemente `dTmas(x, n)` y `pTmas(x, n)` que toman un vector de enteros `x` y un escalar `n`, y den, respectivamente, la función de probabilidad puntual y la función de distribución de #TT bajo $H_0$ en cada valor de `x`.
```R
dTmas <- function(x, n) {
  ret <- vector(mode = "numeric", length = length(x))
  for (i in seq_along(x)) {
    # Su código aquí
  }
  return(???)
}
pTmas <- function(x, n) { "repita el patrón de dTmas" }
```

Al menos los siguientes casos de test deben pasar:
```R
n <- 15
t <- 34
stopifnot(
  dTmas(24, 12) == 67 / 2 ^ 12,
  dTmas(0:10, 4) == c(1, 1, 1, 2, 2, 2, 2, 2, 1, 1, 1) / 16,
  sum(dTmas(0:21, 6)) == 1,
  dTmas(0:2, 55) == 2 ^ -55,
  dTmas(t, n) == dTmas(n * (n + 1) / 2 - t, n),
  pTmas(t, n) == 1 - pTmas(n * (n + 1) / 2 - (t + 1), n)
)
```
_Nota: ¡Ojo! `particiones` espera un escalar como primer argumento `t`, mientras que `pTmas` y `dTmas` esperan vectores._
]
#rtao[
  ```R
dTmas <- function(x, n) {
  ret <- vector(mode = "numeric", length = length(x))
  for (i in seq_along(x)) {
    ret[i] <- particiones(x[i], n)
  }
  return(ret / 2 ^ n)
}

pTmas <- function(x, n) {
  ret <- vector(mode = "numeric", length = length(x))
  for (i in seq_along(x)) {
    ret[i] <- sum(dTmas(0:x[i], n))
  }
  return(ret)
}
```
]
#rtao[(sobre performance)

Aunque correctas y fáciles de entender, estas versiones son muy poco _performantes_. `dTmas` está OK para calcular un valor puntual de $p_n (t)$, pero si se desea calcular varios valores de $t$ para un mismo $n$ (que es como `pTmas` llama a `dTmas`), se podrían reutilizar los resultados de llamadas a `particiones(t, n)`. Esta técnica se conoce como "cacheado" o "memoización", y vale la pena estudiarla por separado. En `R`, un buen punto de partida para esto es la ayuda del operador `<<-` que deberán buscar ingresando `?"<<-"` en la consola.

En `pTmas`, podemos calcular una sola vez todos los valores de `dTmas` necesarios, guardarlos en una variable, y luego reutilizarlos. Noten cuánto más rápida se siente esta versión:
```R
pTmas <- function(x, n) {
  dTmas_n <- dTmas(0:(max(x)), n)
  ret <- vector(mode = "numeric", length = length(x))
  for (i in seq_along(x)) {
    ret[i] <- sum(dTmas_n[1:(x[i] + 1)])
  }
  return(ret)
}
```
]
=== `mi.wilcox.test`
Ahora sí, estamos en condiciones de implementar el test de Wilcoxon de rango signado. Repasemos:
Para una muestra aleatoria 
$
vv(X) = (X_1, dots, X_n), thick X_i ~^"iid" F(x - theta) thick forall i in [n], thick F in Omega_s
$
, deseamos testear:
$
  "('two.sided') igual contra distinto: " &H_0: theta = theta_0 quad "versus" quad H_0: theta != theta_0 \
  "('greater') menor o igual contra mayor: " &H_0: theta <= theta_0 quad "versus" quad H_0: theta > theta_0 \
  "('less') mayor o igual contra menor: " &H_0: theta >= theta_0 quad "versus" quad H_0: theta < theta_0 \
$


#prg("15 pts.")[
Programe `mi.wilcox.test`, una función con la misma clase que `wilcox.test`, que toma los siguientes parámetros (siguiendo la firma de `wilcox.test`):
- `x`, un vector numérico con la muestra $vv(X)$,
- `alternative`, un escalar de tipo `"character"` representando las hipótesis a testear (una de `c("two.sided", "greater", "less")` ),
- `mu`, un escalar numérico representando $theta_0$, el valor de la mediana bajo la hipótesis nula.
y devuelve un objeto de clase `"htest"`, con (al menos) atributos `statistic`, `p.value` y `alternative` equivalentes a los salida de `wilcox.test`.

No hace falta reportar una región de rechazo, basta con el p-valor. Al menos el siguiente caso de prueba debe pasar:

```R
set.seed(1234)
n <- 20
X <- rnorm(n)
theta0 <- -1
alternative <- "greater"

R_wilcox <- wilcox.test(X, alternative=alternative, mu = theta0)
mi_wilcox <- mi.wilcox.test(X, alternative=alternative, mu = theta0)
stopifnot(
  identical(mi_wilcox$statistic, R_wilcox$statistic),
  identical(mi_wilcox$alternative, R_wilcox$alternative),
  isTRUE(all.equal(mi_wilcox$p.value, R_wilcox$p.value)),
  identical(class(R_wilcox), class(mi_wilcox))
)
```
_Sugerencia: Consulte la ayuda de `match.arg` para manipular el valor de `alternative`_.
]
#rtao[
```R
mi.wilcox.test <- function(x, alternative = c("two.sided", "less", "greater"), mu = 0) {
  stopifnot(is.numeric(x))
  stopifnot(is.scalar(mu))
  n <- length(x)
  X <- X - mu  # centro los datos en la mediana bajo H0
  rv <- list(
    alternative = match.arg(alternative),
    statistic = setNames(sum(rank(abs(X))[X > 0]), "V")
  )
  pval.izq <- pTmas(rv$statistic, n)
  pval.der <- 1 - pTmas(rv$statistic - 1, n)
  if (alternative == "greater") {
    rv$p.value <- pval.der
  } else if (alternative == "less") {
    rv$p.value <- pval.izq
  } else { # alternative == "two.sided"
    rv$p.value <- 2 * min(pval.izq, pval.der)
  }
  return(structure(rv, class = "htest"))
}
```
]
==== Distribución asintótica
Aunque #TT es una combinación lineal de variables independientes e idénticamente distribuidas entre sí (las $W_j$), cada una está pesada por un coeficiente distinto (los $j in [n]$), por lo cual la versión del Teorema Central del Límite que conocemos no nos servirá.


#prg("5 pts.")[Bajo $H_0$, ¿Cuánto vale $EE(#TT)$? ¿Y $"Var"(TT)$?] 
#rtao[Bajo $H_0$,
$
  EE(TT) &= EE(sum_(j in [n]) W_j times j) = sum_(j in [n]) EE( W_j times j) = sum_(j in [n]) EE( W_1 times j) \
  &= EE(W_1) sum_(j in [n]) j = 1/2 dot n(n+1)/2 =  n(n+1)/4 \
  "Var"(TT) &= "Var"(sum_(j in [n]) W_j times j) = sum_(j in [n]) "Var"( W_j times j) = sum_(j in [n]) "Var"( W_1 times j) \
    &= "Var"(W_1) sum_(j in [n]) j^2 = 1/4 dot (n(n+1)(2n+1)) / (6) =  (n(n+1)(2n+1))/24 \
$
]
La "#link("https://en.wikipedia.org/wiki/Lindeberg%27s_condition")[condición de Lindeberg])" nos dota de una forma un poco más general del T.C.L., que ahí adaptamos del Apéndice "A9" de Hettmansperger (1984) #footnote[cf. página 301 del libro o p. 317 del PDF para la prueba]
#thm("TCL de Lindeberg")[Sean 
$W_1, dots, W_n$ v.a. i.i.d. con $EE( W_1) = 0, thick "Var"(W_1) = sigma^2, thick 0 < sigma^2 < oo $.
Defínase $S = sum_(i=1)^n a_(i,) W_i slash sqrt(n)$. Si
$
  (max_i |a_i|) / sqrt(sum_(i=1)^n a_i^2) arrow 0
$
entonces $S / sqrt("Var"(S)) arrow^(cal(D)) "Normal"(0, 1)$, con $"Var"(S) = sigma^2 (sum_(i=1)^n a_i^2) slash n$. 
] <cond-lindeberg>
#prg("8 pts.")[Dé la distribución asintótica de #TT]
#rtao[Consideremos una sucesión de estimadores indexados en el tamaño muestral $n$, $TT_n$. Sabemos ya que $TT_n = sum_(j in [n]) j dot W_j$, 
donde los $W_j$ no tienen esperanza nula, pero sí tienen varianza constante. Consideremos entonces $V_i = W_i - E(W_i) = W_i - 1 slash 2$ :
$
  TT_n &= sum_(j in [n]) j dot (V_j + 1/2) = sum_(j in [n]) j dot V_j + (n(n+1))/4
$

donde $forall j in [n]$,

$
  EE(V_j) &= EE(W_j - 1/2) = 1/2 - 1/2 = 0 \
  "y Var"(V_j) &= "Var"(W_j - 1/2) = "Var"(W_j) = 1/4 = sigma^2 in (0, oo)
  $

Tomando $a_j = j dot sqrt(n)$ tal que $j = a_j slash sqrt(n)$, tenemos que
$
  S = sum_(i=1)^n a_(i,) W_i slash sqrt(n) = sum_(j in [n]) j dot V_j
$
y por ende $TT_n = S + (n(n+1))/4$, ó $ S= TT - EE(TT)$ como uno esperaría. Además,
$
  (max_j |a_j|) / sqrt(sum_(j=1)^n a_j^2) &= (max_j | j slash  sqrt(n)|) / sqrt(sum_(j=1)^n (j slash  sqrt(n))^2) = overbrace(max_j j, =n) /  cancel(sqrt(n)) dot cancel(sqrt(n))/ sqrt(sum_(j=1)^n j ^2) = n / sqrt((n(n+1)(2n+1)) / 6) \
  &= sqrt(6) / cancel(sqrt(n / n)) dot underbrace(1 /sqrt((n+1)/n), < 1) dot 1 / sqrt(2n+1) < sqrt(6 / (2n + 1)) stretch(arrow)^(n -> oo) 0

$
Así que $S/sqrt("Var"(S)) ->^cal(D) "Normal"(0, 1)$. Operando sobre $"Var"(S)$ vemos que:

$
  "Var"(S) = "Var"(sum_(j in [n]) j dot V_j) = sum_(j in [n]) j^2 overbrace("Var"(V_j), = 1/4 thick forall j in [n]) = (n(n+1)(2n+1)) /( 4 dot 6) = "Var"(TT)
$

y finalmente resulta que 
$
  (TT_n - EE(TT_n)) / "Var"(TT_n)->^cal(D) "Normal"(0, 1)
$
]

#prg("8 pts.")[Fije $n_1=4, n_2=10, n_3=20$. Para cada $n$, realice un gráfico de barras con la probabilidad puntual _exacta_ de #TT (válgase de `dTmas`) y superpóngale una línea con la densidad asintótica esperada. ¿Coinciden razonablemente? ¿En toda la distribución, en el centro, en las colas? ¿A partir de qué $n$? ¿Se le ocurre alguna corrección sencilla para los $n$ pequeños?]
#align(center)[#block(width: 125%)[#rtao[
  #figure(image("prg-17.svg"))
Dos decisiones a tomar:
1. si graficar la #TT "original", o su versión "estandarizada", $(TT - EE(TT)) / sqrt("Var"(TT))$.
2. Cómo convertir una densidad continua (`dnorm`) a una probabilidad discreta (`dTmas`). 


En este gráfico, respecto a 
1. elegimos graficar #TT estandarizada para mantener un eje X comparable en todos los los gráficos.

#let dnorm = $f_(cal(N))$
2. 
 Tomamos la "sencilla" decisión de elegir una grilla equiespaciada a intervalos $delta$, $vv(g) = (g_1, dots, g_m)$ #footnote[Es decir, $g_i = g_(i-1) + delta$] que cubre con 10% de margen el rango de #TT, y calcular la probabilidad puntual a graficar en $g_j$ como $delta dot dnorm(g_j)$, donde $f_cal(N)$ es la densidad $"Normal"(0, 1)$.
 
 Una sofisticación, potencialmente importante para $n$ pequeños, sería graficar en $g_j$ la probabilidad de $[dnorm(g_j + delta/2) + dnorm(g_j - delta/2)] slash 2$, centrando el intervalo en $g_j$ y aproximando la densidad con un paralelogramo en lugar de un rectángulo.


 En ningún caso hay sesgos sistémicos, pero para $n=4$ la aproximación es tal vez demasiado burda para ser de utilidad práctica. Aún para $n=20$ la aproximación normal muestra una ligera sobreestimación en el entorno de $t=0$, una ligera sobreestimación en $(-2sigma, +2sigma)$, y - a esta escala - una aproximación bastante certera en las colas.
]]]
=== Distribución bajo la alternativa vía _bootstrap_
Bajo $H_1$, $F$ no es simétrica alrededor de $0$ sino de algún otro valor, con lo cual los rangos no serán independientes de los signos, y la distribución del estadístico #TT no cuenta con forma cerrada.

Sin embargo, conociendo el proceso generador de los datos (o DGP #footnote[_Data Generating Process_, por sus siglas en inglés]), es posible calcular la potencia para una alternativa puntual, con el procedimiento de _bootstrap_ paramétrico. Asuma el ambiente de test ya habitual, $X_i ~^"iid" F(x - theta), F in Omega_s$, y queremos testear a nivel menor o igual a $alpha$ #footnote[Recuerden que como el estadístico #TT es discreto, el nivel que alcance nuestro test no será exactamente $alpha$, sino el mayor $alpha^* <= alpha$ que la distribución permita.]:
$
  H_0: theta =0 "versus" H_1 : theta = theta_1 > 0
$
El test resultará
$
  phi(vv(X)) = ind(TT > k^*), thick EE_0(phi) = alpha^* <= alpha
$
donde elegimos $k^*$ para maximizar la potencia del test, respetando el nivel $<= alpha$.

Sea ahora $H(x) = F(x - theta_1)$ la verdadera distribución del DGP, simétrica y con mediana igual a $theta_1$. Si el DGP es conocido, el siguiente procedimiento nos da un método para estimar la potencia $pi_phi (theta_1)$:
1. Genere $m$ muestras de tamaño $n$ de la distribución $H$; llamémoslas $vv(Y)_1, dots, vv(Y)_(m)$
2. Compute $T^+(vv(Y)_i) thick forall i in [m]$; guarde los resultados en un vector.
3. El estimador por bootstrap de $pi_phi (theta_1)$ está dado por
$
  hat(pi_phi)(theta_1) = hat(EE_(theta_1))(phi) = m^(-1) sum_(i=1)^m ind(T^+(vv(Y)_i) >= k^*)
$

#obs[ En este _setup_, el procedimiento de bootstrap es "paramétrico", en tanto la distribución del DGP está parametrizada y por ende podemos _samplear_ de ella directamente. A diferencia del procedimiento visto en clase en el que había que estimar $hat(theta)_1$ y luego samplear de $F_(hat(theta)_1) = F(x - hat(theta)_1)$, aquí el enunciado provee el $theta_1$ real, así que su estimación es innecesaria y podemos samplear directamente de $H(x) = F(x - theta_1)$. Por lo demás, el procedimiento es el mismo.]
#prg("13 pts.")[Los siguientes datos fueron generados por $D = "Normal"(1, 1), thick n=12$:
```R
set.seed(1984)
n <- 12
theta1 <- 1
sigma_sq <- 1
X <- rnorm(n, mean=theta1, sd=sqrt(sigma_sq)) 
```
Compute $phi_w$, el test de Wilcoxon de rango signado de nivel menor o igual a $alpha = 0.05$ para las hipótesis:

$ H_0: theta=0 quad "versus" quad H_1: theta>0 $
A continuación, fije $theta_1=1, m=10.000$ y estime por bootstrap la potencia $hat(pi_(phi_w))(theta_1)$.
] <prg-bootstrap>
#rtao[
  El paso (1), generar las muestras, es trivial. En el paso (2), no es recomendable usar `mi.wilcox.test` para computar el estadístico, ya que la función carga con la tarea aquí innecesaria de calcular el p-valor correspondiente. Mejor, hacer una función sencilla `Tmas(x)` que compute el estadístico a partir de una muestra. Para el paso (3), es necesario conocer $k^*$, donde por convención definimos la región de rechazo $"RR"$ con un límite cerrado, $[k^*, n(n+1)/2]$.
  ```R
m <- 10000
theta0 <- 0
alfa <- 0.05
Tmas <-function(x) { sum(rank(abs(x))[x > 0]) }
boot.Tmas <- vector(mode = "numeric", length = m)
for (i in seq.int(m)) {
  Y <- rnorm(n, mean = theta1, sd = sigma_sq)
  boot.Tmas [i] <- Tmas(Y)
}
suma.n <- (n*(n+1)/2)
pTobs <- cumsum(dTmas(0:suma.n, n))  # más rápido que pTmas(...)
idx <- which.max(pTobs > 1 - alfa)
# idx es el índice del primer elemento tal que pTobs > 1 - alfa
# pTobs[idx] es la acumulada hasta k = idx-1; pTmas(idx-1, n) > 1 - alfa
# la RR comenzará - inclusive - en el entero sgte a (idx - 1), que es `idx`
k.wil <- idx
alfa.wil <- sum(dTmas(k.star:suma.n, n))
pot.boot.wil <- mean(boot.Tmas >= k.star)
  ```
  Resulta entonces que $k_w^* = 61, thick alpha_w^*=0.04614... " y " hat(pi_(phi_w))(1) approx 0.9324.$
]
#prg("8 pts.")[ Compute para las mismas hipótesis y condiciones de @prg-bootstrap, $phi_n$, un test para el valor de la media (y mediana) de v.a.i.i.d. con varianza conocida, según $D$. 

Calcule (analíticamente, sin estimar) la potencia $pi_(phi_n)(theta_1)$. Compute además $phi_s$, el test del signo para las mismas hipótesis y estime por bootstrap, $hat(pi_(phi_s))(theta_1)$. Compare y contraste los resultados obtenidos. ¿Es el test t efectivamente el más potente? ¿Por cuánto?
#v(1em)
(0pts., sólo para valientes) Sin asumir varianza conocida, hay que recurrir al "test t". Describa $phi_t$, el "test t" correspondiente a esta situación, calcule su potencia para la alternativa $theta_1$ e inclúyalo en la comparación con $(phi_w, phi_n, phi_s)$. 

_Sugerencia: considere la distribución "t de Student no-central". Para el cálculo de potencia, de necesitarlo, sí puede utilizar el verdadero $sigma$_.
#v(1em)
_Nota: Al igual que con $phi_w$, tenga cuidado de proveer un test del signo $phi_s$ exacto #footnote[En la Práctica 5 Ej. 22 se da un test del signo asintótico. Si se le complica deducir el equivalente exacto, en Hettmansperger (1984) la sección §1.2 describe "El test del Signo y Su Distribución".] de nivel tan cercano a $alpha = 0.05$ como pueda, pero sin pasarse._]
#rtao[
  Considerando las mismas hipótesis de siempre, sabemos que $phi_n$ depende del estadístico $Z = sqrt(n)(overline(X) - theta_0)/sigma$, que bajo $H_0$ tiene distribución $"Normal"(0, 1)$. Si $z_alpha$ es el cuantil que para una $"Normal"(0, 1)$ acumula probabilidad $alpha$ _a derecha_, resulta:
  $
    phi_n (vv(X)) &= ind(Z(vv(X)) >= z_(alpha)) \
    EE_0(phi_n) &= EE_0(ind(Z > z_(alpha))) = Pr_0(Z >= z_(alpha)) = alpha
  $
  Para calcular la potencia bajo la alternativa, consideremos que 
  $
    Z(vv(X)) >= z_(alpha) &iff sqrt(n)(overline(X) - theta_0)/sigma >= z_(alpha) \
    &iff sqrt(n)(overline(X) - theta_0)/sigma + sqrt(n) (theta_0 - theta_1) / sigma >= z_(alpha)  + sqrt(n) (theta_0 - theta_1) / sigma \
    &iff sqrt(n)(overline(X) - theta_1)/sigma >=  z_(alpha)  + sqrt(n) (theta_0 - theta_1) / sigma
  $
  y bajo $H_1$, $sqrt(n)(overline(X) - theta_1)/sigma ~ "Normal"(0, 1)$. Luego, si $Phi$ es la función de distribución de una normal estándar, resulta que:
  $
    pi_(phi_n)(theta_1) &= EE_1(ind(Z > z_(alpha))) = Pr_1(Z >= z_(alpha)) = Pr_1(sqrt(n)(overline(X) - theta_1)/sigma >=  z_(alpha)  + sqrt(n) (theta_0 - theta_1) / sigma) \
    pi_(phi_n)(theta_1) &= 1 - Phi(z_(alpha)  + sqrt(n) (theta_0 - theta_1) / sigma)
  $
  Reemplazando con nuestros datos, obtenemos 
  $
    pi_(phi_n)(theta_1) &approx 1 - Phi(1.6449 + sqrt(12) (0 - 1)/sqrt(1)) approx 1 - Phi(-1.8192) approx 0.9656
  $

  ```R
k.norm <- qnorm(alfa, lower.tail=FALSE)
delta <- sqrt(n) / sqrt(sigma_sq) * (theta0 - theta1)
pot.norm <- pnorm(kn + delta, lower.tail=FALSE)
```
Para el test del signo, consideremos las variables $Y_i = ind(X_i > 0)$, y el estadístico $S = sum_(i=1)^n Y_i$. Bajo $H_0, X_i ~ F in Omega_s$ sabemos que $X_i$ es simétrica alrededor del 0, así que,
$
  Y_i ~^("iid") "Bernoulli"(1/2), quad S ~ "Binomial"(n, 1/2)
$
Podemos escrbir un test
$
  phi_s (vv(X)) &= ind(S(vv(X)) >= k^*) \
    EE_0(phi_s) &= EE_0(ind(S(vv(X)) >= k^*)) = Pr_0(S(vv(X)) >= k^*) = alpha^* <= alpha
$
Para la elección de $k^*$, buscamos el _mínimo_ $k^* : Pr_0(S >= k^*) <= alpha$. Cuando $n=12$ y $alpha=0.05$, resulta que podemos tomar
```R
k.sgn <- which.max(pbinom(0:n, n, 1/2, lower.tail=FALSE) <= alfa)
alfa.sgn <- sum(dbinom(k.signo:n, n, 1/2))
Tsigno <- function(x, theta0) { setNames(sum(x > theta0), "S") }
boot.Signo <- vector(mode = "numeric", length = length(x))
for (i in seq.int(m)) {
  Y <- rnorm(n, mean=theta1, sd=sqrt(sigma_sq))
  boot.Signo[i] <- Tsigno(Y, theta0)
}
pot.boot.sgn <- mean(boot.Signo >= k.signo)
```
 Resulta entonces que $k_"s"^* = 10, thick alpha_"s"^*=0.01929... " y " hat(pi_(phi_s))(1) approx 0.7068$. En este caso, la potencia quedó demasiado baja, pues bajar $k_s^* = 9$ subía $alpha_"s"^*approx 0.73$; en ese caso la potencia pasaba a ser $approx 0.89$.

 Finalmente, si quisiéramos construir un "test t", usamos la fórmula que ya conocemos de memoria. Si $t_(n, alpha)$ es el cuantil de una $t$ de Student que acumula _a derecha_ probabilidad de $alpha$, tenemos el estadístico $W$ que distribuye como $t_(n-1)$, con
   $
    W &= sqrt(n) (overline(X) - theta_0) / s(vv(X)) ~ t_(n-1)\
    phi_t (vv(X)) &= ind(W(vv(X)) >= t_(n-1, alpha)) \
    EE_0(phi_s) &= EE_0(ind(W > t_(n-1, alpha))) = Pr_0(W >= t_(n-1, alpha)) = alpha
  $
  Ahora bien, ¿cómo calcular la potencia? Si $Psi_k$ es la función de distribución de una $t_(k)$, uno estaría tentado de seguir la lógica de $phi_n$ y escribir
  $
    pi_(phi_t) (theta_1) = Psi_k (t_(n-1, alpha) + sqrt(n) (theta_0 - theta_1) / s(vv(X)))
  $
  Lamentablemente, $s(vv(X))$ es una variable aleatoria, y por ende $Psi_k (dot)$ también. En su lugar, se puede usar la distribución $t$ de Student no-central. Sean
  $
    A ~ "Normal"(0, 1), quad B ~Chi^2_b, quad A bot B, quad b in NN, quad delta in RR
  $
  entonces $V = (A + delta) / sqrt(B slash b)$ tiene distribución $t$ no-central con parámetro de no-centralidad #footnote["ncp" o _non-centrality parameter_ por sus siglas en inglés] $delta$, y la notamos $V ~ t_(b, delta)$. Sea entonces $Psi_(n, mu) $ la distribución de una $t_(n, mu)$, donde por convención $Psi_(n,0) = Psi_n$. Operando un poco con $W$, se puede reescribir como:

  $
    W = sqrt(n) (overline(X) - theta_0) / s(vv(X)) = [(sqrt(n) (overline(X) - theta_1) /sigma + sqrt(n) (theta_1 - theta_0) / sigma) mid(slash) sqrt(((n-1) s^2) / sigma^2 slash (n-1)) ] = (A + delta) / sqrt(B slash b) \
    "con " A = sqrt(n) (overline(X) - theta_1) / sigma ~ "Normal"(0, 1), quad B = ((n-1) s^2) / sigma^2 ~ chi^2_(n-1), quad A bot B,
    quad delta = sqrt(n) (theta_1 - theta_0) / sigma
  $
  Luego, bajo $H_1, thick W ~ t_(n-1, delta)$  y podemos calcular la potencia según:
  $
    pi_(phi_t) (theta_1) = Pr_1(W >= t_(n-1, alpha)) = 1 - Psi_(n-1, delta) (t_(n-1, alpha)) approx 1 - Psi_(11, -3.464) (1.7958) approx 0.9447
  $
  ```R
  
# A mano
k.t <- qt(alfa, df=n-1, ncp=0, lower.tail=FALSE)
ncp <- sqrt(n) * (theta1 - theta0) / sqrt(sigma_sq)
pot.t <- pt(kt, ncp=ncp, df=n-1, lower.tail=FALSE)
# O usando R
R.pot.t <- power.t.test(
  n=n,
  delta = theta1,
  sd=sqrt(sigma_sq),
  sig.level = alfa,
  type = "one.sample",
  alternative = "one.sided"
)
stopifnot(isTRUE(all.equal(pot.t, R.pot.t$power)))
```
Finalmente, nos quedan los tests ordenados ascendentemente según sus potencias como:
$
"Signo " &< "Wilcoxon" &&< "t-Student" &&< "Normal" \
 0.7068 thick &< quad 0.9324 &&< quad 0.9447 &&< thick 0.9656
$
Ya mencionamos que la potencia del test del signo quedó bastante baja por el $alpha&^*_s approx 0.02$. Dicho esto, la jerarquía de potencias es la esperada, pero - tal vez no tan - sorpresivamente, el test de Wilcoxon tiene una potencia comparable a la de "t-Student" y "Normal", con hipótesis muchísimo más laxas.
  ]