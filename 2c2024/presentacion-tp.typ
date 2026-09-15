// Latex Look
#set page(margin: 1in, numbering: "1 de 1")
#set par(leading: .75em, first-line-indent: 1.8em, justify: true, spacing: 0.55em)
#set text(font: "New Computer Modern", lang: "es")
#show heading: set block(above: 1.4em, below: 1em)
// #set enum(numbering: "a)")
#show link: underline

#import "@preview/ctheorems:1.1.3": *
#show: thmrules
#let defn = thmbox("definition", "Definición", inset: (x: 1.2em, top: 1em))
#let prg = thmbox("pregunta", "Pregunta",  fill: rgb("#f6e993")
)
#let prgh = thmbox("pregunta", "Pregunta", fill: rgb("#f6c193")
)
#let obs = thmbox("observacion", "Observación", fill: rgb("#9ef9a4"))

#let ind(x) = $op(bb(1)){#x}$


#let iff = sym.arrow.l.r.double
#let Pr = math.op("Pr", limits: false)
#let ul(x) = $underline(#x)$

#let vv(x) = $bold(#x)$

#show sym.phi: sym.phi.alt

#show raw.where(block: true): it => pad(top: 0.5em, left: 2em, bottom: 0.5em, it)

// #let ind(x) = $op(bb(1)){#x}$
// #let iff = sym.arrow.l.r.double
// #let Pr = math.op("Pr", limits: false)
// #let vv(x) = $underline(#x)$
// #show sym.phi: sym.phi.alt

#let TT = $T^+$

#heading(outlined: false, depth: 1)[IECD 2C2024 - Trabajo Práctico]

En el trabajo siguiente, estudiaremos el test "de rango signado de Wilcoxon", un test no paramétrico para la mediana de una distribución simétrica. Tendrán que implementarlo de manera compatible pieza-por-pieza con la implementación nativa de $R$, `wilcox.test`, y luego calcular su potencia para alternativas puntuales por el método de _botstrap_. Para ello, introduciremos primero
- `S3`, el paradigma OOP #footnote[Programación Orientada a Objetos, por sus siglas en inglés] más viejo de R (sí, hay varios) y
- las condiciones de validez y propiedades generales del test de Wilcoxon.

== Condiciones de entrega y aprobación
El presente escrito es un apunte sobre OOP y el test de Wilcoxon, con preguntas prácticas diseminadas en medio. Cada pregunta correcta vale tantos puntos como se menciona entre paréntesis en cada una, y para aprobar es necesario contar con $n$ de $m$ puntos #footnote[A definir precisamente en las próximas horas]. Las preguntas en amarillo son, y las naranjas un poco más difíciles.

*El TP ha de resolverse en grupos de 3 integrantes*. El formato de entrega consistirá de dos archivos subidos a través del campus,
- `informe-<apellido1>-<apellido2>-<apellido3>.pdf`, un informe _en formato PDF_ de no más de $X$ #footnote[íbidem] páginas contestando las preguntas teóricas, y
- `codigo-<apellido1>-<apellido2>-<apellido3>.R`, un _script_ de R con las respuestas a las preguntas de código, siguiendo estrictamente el formato requerido en cada una.
Tanto el informe como el código se evaluarán con especial énfasis en la *claridad y concisión de exposición, y prolijidad en la presentación*. El código, además, se evaluará de manera automática, a través de una serie de casos de prueba secretos (pero análogos a los que se ofrecen en cada pregunta), que deberán ejecutarse con éxito.

Para el informe pueden usar el procesador de texto que deseen, aunque sugerimos utilizar formatos amigables a la expresión científica, como LaTeX #footnote[Si no conocen un buen editor, #link("https://www.overleaf.com/")[Overleaf] es una excelente primera opción], #link("https://rmarkdown.rstudio.com/")[RMarkdown] o #link("https://typst.app/")[Typst] #footnote[¡Este TP está escrito en Typst!].

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



== Notación
- $[n] = {1, dots, n}$ es el conjunto de los primeros $n$ números naturales
- $C = A slash B = {x : x in A and x in.not B}$ es la operación de sustracción de conjuntos,
- la *negrita* ($vv(X), vv(a), vv(1)$) denota valores vectoriales, y fuente "normal" para escalares
- usamos mayúsculas ($vv(Y), Z$) para elementos aleatorios y minúsculas ($y, vv(a)$) para  no-aleatorios  #footnote[sean conocidas o no].
- $ind(A)$ es la función indicadora, que vale $0$ cuando $A$ es Falso y $1$ cuando $A$ es Verdadero
- Dado un conjunto $X$, $|X|$ ó $\# X$ denotarán su cardinalidad

