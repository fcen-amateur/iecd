// Latex Look
#set page(margin: 1in, numbering: "1 de 1")
#set par(leading: .75em, first-line-indent: 1.8em, justify: true, spacing: 0.55em)
#set text(font: "New Computer Modern", lang: "es")
#show heading: set block(above: 1.4em, below: 1em)
#set enum(numbering: "a)")
#show link: underline

#let ind(x) = $op(bb(1)){#x}$
#let iff = sym.arrow.l.r.double
#let Pr = math.op("Pr", limits: false)
#let ul(x) = $underline(#x)$
#show sym.phi: sym.phi.alt

#heading(outlined: false, depth: 1)[IECD 2C2024 - Clase 18 - Jueves 24/10]
#outline(depth: 2)
== Ejercicios
=== Ejercicio 1
Se observa una muestra aleatoria $X_1, dots, X_n$ de una población cuya densidad $f(x)$ puede ser

$ f_0(x)= 1 / 50 ind(0<x<50) quad "ó" quad f_1(x)= x / 1250 ind(0<x<50) $

1. Hallar un test para $H_0 := f (x) = f_0 (x)$ contra $H_1 := f(x) = f_1(x)$. _Sugerencia: considerar la distribución de $Y = −ln(X/50)$._
2. Calcular la función potencia del test propuesto en el ítem anterior.
3. Se tomó una muestra de tamaño $n = 100$ obteniendo $− sum_(i=1)^100 ln(x_i/50) = 87.9$. ¿Qué concluye?
4. Hallar el p-valor con la medida resumen de los datos brindada en el ítem anterior.

=== Ejercicio 2
Sean $X_1, dots,X_n$ variables aleatorias independientes tales que $X_i ~ "Lognormal"( mu sqrt(i), 1)$, es decir la función de densidad de $X_i$ es
$ f_mu (x) = 1 / (x sqrt(2 pi) ) exp(-( ln(x) - mu sqrt(i))^ 2 / 2) ind(0<x) $

1. Hallar el test más potente de nivel $alpha$ para contrastar las hipótesis $H_0 := mu = mu_0$ contra #linebreak() $H_1 := mu = mu_1$ para $mu_1 > mu_0$.
2. Obtener la función de potencia del test hallado en el ítem anterior.
3. Probar que el test hallado en a) es el test uniformemente más potente para contrastar las hipótesis $H_0 := mu <= mu_0$ contra $H_1 := mu > mu_0$.
_Ayuda: Si $X ~ "Lognormal"(mu, sigma^2)$, entonces $ln(X) ~ "Normal"(mu, sigma^2)$._

#pagebreak()

== Solución Ej. 1
=== Parte a)
Hay _infinitos_ tests posibles para testear la hipótesis nula: siempre y cuando conozcamos la distribución del estadístico bajo la hipótesis nula, podemos derivar un test del nivel deseado. Aquí por caso, sabemos que bajo $H_0$ las $X_i$ tienen distribución uniforme, y en ejercicios anteriores calculamos la distribución de $Y = max(X_1, dots, X_n), thick F_Y (t) = [F_X (t)]^n$. Pero para no reinventar la rueda, vayamos directo al cociente de verosimilitud para aplicar el Lema de Neyman Pearson.

Un detalle: si algunas de las observaciones $x_i$ _no estuviera contenida en el soporte_ propuesto por cualquiera de las dos hipótesis, el cociente de verosimilitud quedaría mal definido, con una desagradable indeterminación del estilo $0 slash 0$, y poco sentido tendría pensar un test de hipótesis para hipótesis que se pueden rechazar de plano con tal solo mirar al pasar los datos. Por lo tanto, asumiremos que $0 < x_i < 50 thick forall thick i$ e ignoraremos las indicadores (idénticamente iguales a $1$) _antes_ de operar con el cociente de verosimilitud. Ahora sí,
$
  Lambda(ul(x)) &= (f_1(ul(x)) ) / (f_0(ul(x)))
  = (product_(i=1)^n x_i slash 1250) / (product_(i=1)^n 1 slash 50)
  = (product_(i=1)^n x_i) / 25^n = 25^(-n) T(ul(x))
$

$Lambda(ul(x))$ resulta ser monótonamiente creciente en $T$, así que podemos construir un test de la forma

$ phi(ul(X)) = ind(Lambda(ul(X)) > k) = ind(T(ul(X)) > k times 25^n = k') $

¿Pero cuál es la distrubición de $T$? Bajo $H_0, thick X_i ~ "Uniforme"(0, 50)$, pero el _producto_ de $n$ tales v.a.i.i.d. no tiene una distribución obvia. Usemos entonces la sugerencia y busquemos la distribución de $−ln(X/50)$. Podemos
1. usar el teorema de cambio de variable,
2. deducir de la función de distribución de $X, thick F_X$, la análoga de $Y, thick F_Y$, o
3. siendo vagos, usar propiedades de las distribuciones (de algún machete o Wikipedia).

Vamos con la última opción:

- Si $X ~ "Unif"(a, b)$, entonces $c X ~ "Unif"(c  a, c b)$
  - Luego, $X slash 50 ~ "Unif"(0, 1)$
- Si $X ~ "Unif"(0, 1)$, entonces $-ln(X) ~ "Exp"(1)$
  - Luego, $−ln(X/50) ~ "Exp"(1)$
- Si $X_i ~^("iid") "Exp"(lambda)$, entonces $sum_(i=1)^(n)X_i ~ Gamma(n, lambda)$
  - Luego, $V(ul(X)) = -sum_(i=1)^(n)ln(X_i slash 50) ~ Gamma(n, lambda)$.

#v(1em)
Resta operar para transformar $T(ul(X))$:
$
  T(ul(X)) > k' &iff ln T(ul(X))> ln k' iff sum_(i=1)^n ln X_i > ln k' iff - sum_(i=1)^n ln X_i < -ln k' \
  &iff - sum_(i=1)^n ln X_i + n ln 50 < ln k' + n ln 50 = k'' \
  &iff - sum_(i=1)^n ln (X_i / 50) < k'' \
  &iff V(ul(X)) < k''
$

Y nuestro test resulta equivalente a $phi(ul(X)) = ind(V(ul(X)) < k''), thick V ~ Gamma(n, 1)$. Llamemos $Q_(V|H_0) (q), thick q in (0, 1)$ a la función de cuantiles "a izquierda" de $V$ #footnote[es decir, la inversa de su función de distribución: $F_V (Q_V (q)) = q$]. Si elegimos $k'' = Q_V (alpha)$, resulta que

$
  pi_phi (f_0) &= EE_(f_0) (phi) = Pr_(f_0) (phi = 1) = Pr_(f_0)(V < Q_V (alpha)) \
  &= Pr_(f_0)(V <= Q_V (alpha)) - underbrace(Pr_(f_0)(V = Q_V (alpha)), =0) \
  &=F_V (Q_V (alpha)) = alpha
$

Y el test $phi(ul(X)) = ind(V(ul(X)) < k'')$ tendrá nivel $alpha$.

=== Parte b)
Calcular la potencia del test _para la alternativa concreta_ $H_1:= f=f_1$ implica conocer $EE_(f_1) (phi)$, y para ello necesitamos conocer la distribución de $V$ bajo $H_1$, llamémosle $V|_(H_1)$. Esta vez, usemos la función de distribución, y comencemos por el principio, reciclando el consejo:

Sea $g(t) = - ln (t slash 50)$ y definamos $Y = g(X)$, Luego, _bajo la alternativa $H_1$_ la función de distribución de $Y, thick F_Y$ será:
$
  F_Y (t) &= Pr(Y <= t) = Pr(g(X) <= t) = Pr(-ln(X slash 50) <= t) = Pr(ln(X slash 50) >= -t) \
  &=Pr(X/50 >=exp(-t)) = Pr(X >= 50 exp(-t)) = 1- Pr(X< 50 exp(-t)) \
  &= 1 - F_X (50 e^(-t)) + underbrace(Pr(X=50 exp(-t)), =0)
$

Operando, se puede ver que para $0 < t < 50$,
$
  F_X (t) = integral_0^t x / 1250 dif x = lr(x^2 / 2500|)^t_0 = t^2 / 2500
$

Y finalmente,
$
  F_y (t) = 1 - F_X (50 e^(-t)) = 1 - (cancel(50^2) e^(-2t)) / cancel(2500) = 1 - e^(-2t)
$

que no es otra más que la función de distribuxión cpte. a $Y|_(H_1) ~ "Exp"(2)$. Luego, $V|_(H_1) ~ Gamma(n, 2)$. Para evitar ambigüedades con la distribución de V bajo cada hipótesis, escribiremos $V|_H_0$ y $V|_H_1$, y llamaremos $F_(V|H_i), Q_(V|H_i), thick i in {0, 1}$ a las funciones de distribución y cuantiles en cada escenario, resp. Finalmente,

$
  pi_phi (f_1) &= Pr_(f_1) (V(ul(X)) < Q_(V|H_0) (alpha)) \
  &= F_(V|H_1)(Q_(V|H_0) (alpha))
$

#pagebreak()
=== Parte c)
El ejercicio no especifica ningún nivel $alpha$, pero por suerte fuimos generalistas de más y lo resolvimos para cualquier valor posible. Tomemos $alpha = 0.05$ para ser originales, y acudamos a R. Recordemos que
$
  V(ul(X)) &= -sum_(i=1)^(n)ln(X_i slash 50) \
  phi(ul(X)) &= ind(V(ul(X)) < Q_(V|H_0) (alpha))
$

```R
> alfa <- 0.05
> n <- 100
> lambda0 <- 1
> Vobs <- 87.9
> (k <- qgamma(alfa, shape = n, rate = lambda0))
[1] 84.13928
> (phi <- Vobs < k)
[1] FALSE
```
#v(1em)
O sea que $phi(ul(X)) = ind(87.9 < 84.14) = 0$ y *no rechazamos* la hipótesis nula.

=== Parte d)
El "p-valor" es la probabilidad _bajo la hipótesis nula_ $H_0$ de obtener un resultado tan o más extremo que el observado. En otras palabras, para un test $phi = ind(T < k)$,
$
  p_"val" &= Pr_(H_0) (T < T_"obs") \
  &= F_(T|H_0) (T_"obs")
$

En nuestro caso, $V|_(H_0) ~ Gamma(n, 1)$, así que
#v(1em)

```R
> (pval <- pgamma(Vobs, shape = n, rate = lambda0))
[1] 0.109559
```

Lo cual es consistente con la decisión de no rechazar $H_0$.

#pagebreak()

== Solución Ej. 2
=== Parte a)
Por la condición necesaria del lema de Neyman-Pearson, sabemos que de existir, el test más potente dependerá del cociente de verosimilitud. Empecemos por allí:

$
  Lambda(ul(x)) &= (f_(mu_1)(ul(x))) / (f_(mu_0)(
    ul(x)
  )) = product_(i=1)^n frac(
    1 / cancel(x_i sqrt(2 pi) ) exp(-( ln x_i - mu_1 sqrt(i))^ 2 / 2),
    1 / cancel(x_i sqrt(2 pi) ) exp(-( ln x_i - mu_0 sqrt(i))^ 2 / 2)
  ) \
  &= product_(i=1)^n exp(-1/2 [( ln x_i - mu_1 sqrt(i))^ 2 - ( ln x_i - mu_0 sqrt(i))^ 2]) \
  &= product_(i=1)^n exp(-1/2 [-2 mu_1 sqrt(i) ln x_i + i mu_1^2  + 2 mu_0 sqrt(i) ln x_i - i mu_0^2]) \
  &= product_(i=1)^n exp((mu_1 - mu_0) sqrt(i) ln x_i - 1/2 i (mu_1^2 - mu_0^2)) \
  &= exp(sum_(i=1)^n [(mu_1 - mu_0) sqrt(i) ln x_i - 1/2 i (mu_1^2 - mu_0^2)]) \
  &= exp((mu_1 - mu_0)sum_(i=1)^n  sqrt(i) ln x_i - 1/2 (mu_1^2 - mu_0^2) sum_(i=1)^n i ) \
  &= c times exp[(mu_1 - mu_0) T(ul(x))]
$
donde $T(ul(x)) = sum_(i=1)^n  sqrt(i) ln x_i$ y $c = exp(- 1/2 (mu_1^2 - mu_0^2) (n (n + 1))/2)$ no depende de la muestra. El cociente resultante es creciente en $T$, con lo cual podemos plantear un test de la forma $phi(ul(X)) = ind(Lambda(ul(X)) > k)$. Operando, vemos que
$
  Lambda(ul(X)) > k &iff c times exp[(mu_1 - mu_0) T(ul(X))] > k iff exp[(mu_1 - mu_0) T(ul(X))] > k slash c \
  &iff (mu_1 - mu_0) T(ul(X)) > ln (k slash c) iff T(ul(X)) > (ln (k slash c)) / underbrace(mu_1 - mu_0, >0) = k'
$

#let Trulo = $accent(T, tilde, size: #140%)$

Con lo cual podemos plantear un test para $H_0 thick "vs." thick H_1$ a partir de $T$, si tan solo conociésemos su distribución bajo $H_0$. Como sugiere la _Ayuda_ del enunciado,
- Si $X ~ "Lognormal"(mu, sigma^2)$, entonces $ln(X) ~ "Normal"(mu, sigma^2)$.
  - Luego, $ln X_i ~ "Normal"(mu_0 sqrt(i), 1)$
- Si $X ~ "Normal"(mu, sigma^2) arrow.r.double a X ~ "Normal"(a mu, a^2 sigma^2)$.
  - Luego, $sqrt(i) ln X_i ~ "Normal"(i mu_0, i)$
- Si $X ~ "Normal"(mu_x, sigma_x^2), thick Y ~ "Normal"(mu_y, sigma_y^2)$ y X, Y son independientes entre sí, $X + Y ~ "Normal"(mu_x + mu_y, sigma_x^2 + sigma_y^2)$.
  - Luego, $T(ul(x)) = sum_(i=1)^n  sqrt(i) ln x_i ~ "Normal"((n (n+1))/2 mu_0, (n (n+1))/2) $.
  - Finalmente,
  $ Trulo(ul(X)) = (T(ul(X)) - (n (n+1)) / 2 mu_0) / sqrt((n (n+1))/2) ~ "Normal" (0, 1) $

Por lo pronto, si consideramos la notación habitual de $z_alpha$ para la función de cuantiles "a derecha" de la normal estándar #footnote[es decir, si $Q_Z (alpha)$ es la función de cuantiles de una $Z ~ "Normal"(0, 1)$, entonces $z_alpha = Q_Z (1 - alpha)$], podemos escribir

$
  phi(ul(X)) = phi(T(ul(X))) = ind(T(ul(X)) > k') = ind(accent(T, tilde)(ul(X)) > (k' - (n (n+1))/2 mu_0)/ sqrt((n (n+1))/2) = k'')
$

y si tomamos $k'' = z_alpha$, tenemos un test que depende de $ul(X)$ sólo a través de #Trulo. Al tener una hipótesis nula simple, el nivel coincide con la función de potencia evaluada en $mu = mu_0$.
$
  pi_phi (mu_0) &= EE_(H_0) [phi(Trulo(ul(X)))]= Pr_(H_0)[phi(Trulo(ul(X))) = 1]= Pr_(H_0)(Trulo > z_alpha) = alpha
$
#v(1em)
Este es entonces, _por construcción_, "el test más potente de nivel $alpha$ para para contrastar las hipótesis $H_0 := mu = mu_0$ contra $H_1 := mu = mu_1$ para $mu_1 > mu_0$" #text(1.5em, sym.square)

==== Parte b)
Repetimos la lógica del Ejercicio 1.b. Bajo $H_1$, la distribución de #Trulo no es inmediatamente conocida, pero la de $T$ sí lo es. Recordemos que $phi(ul(X)) = ind(Trulo > z_alpha)$ y bajo $H_0:=mu=mu_0$ , #linebreak() $T ~ "Normal"((n (n+1))/2 mu_0, (n (n+1))/2)$. Más generalmente, y tomando $gamma = (n (n+1))/2 > 0$ para "limpiar la notación",
$
  T|_(mu=mu^*) ~ "Normal"(gamma mu^*, gamma)
$
Luego, un reacomodamiento intenligente muestra que:

$
  Trulo = (T - gamma mu_0) / sqrt(gamma)> z_alpha &iff (T - gamma mu_0) / sqrt(gamma) - sqrt(gamma)(
    mu_1 - mu_0
  ) =(T - gamma mu_1) / sqrt(gamma) > z_alpha - sqrt(gamma)(mu_1 - mu_0)
$

Y bajo $H_1:= mu=mu_1, thick (T - gamma mu_1) slash gamma ~ "Normal"(0, 1)$. Luego, la potencia resulta ser

$
  pi_phi ((mu_1) &= EE_(H_1) [phi(Trulo(ul(X)))]= Pr_(H_1)[phi(Trulo(ul(X))) = 1]= Pr_(H_1)(Trulo > z_alpha) \
  pi_phi (mu_1) &= Pr_(H_1)((T - gamma mu_1) / sqrt(gamma) >z_alpha - sqrt(gamma)(mu_1 - mu_0)) \
  pi_phi (mu_1) &= 1 - Phi[z_alpha - sqrt(gamma)(mu_1 - mu_0)]
$
donde $Phi$ es la función de distribución acumulada de una $X ~ "Normal"(0,1)$. Nótese que
- si $g(t) = z_alpha - sqrt(gamma) (t - mu_0)$ , entonces $ pi_phi (t) = 1 - Phi(g(t))$
- $g'(t) = - sqrt(gamma) < 0 thick forall t$
- por ser función de distribución, $Phi$ tiene por primera derivada una función de densidad, estrictamente positiva en todo su soporte, que para la ley Normal es $RR$.

Luego,

$
  diff / (diff t) pi_phi (t) = - underbrace(Phi'(g(t)), > 0) times underbrace(g'(t), <0) > 0
$
y la función de potencia es _estrictamente creciente respecto a $mu$_.

#pagebreak()
==== Parte c)
#emph[*Nota*: El Vídeo 1 de la #link("https://campus.exactas.uba.ar/mod/page/view.php?id=407055")[Clase 16] cubre este argumento en detalle para $X_i ~ "Normal"(mu, sigma_0^2)$. Aquí una demostración equivalente pero ligeramente distinta.]

Para encontrar el test más potente para $H_0 := mu = mu_0 thick "vs." thick H_1 := mu = mu_1 > mu_0$, lo único que utilizamos fue que $mu_1 > mu_0$, lo cual determina la "dirección" #footnote[es decir, si tomar como región de rechazo $T in [k, oo)$ ó $T in (-oo, k]$] del test. Por lo tanto, un razonamiento análogo dicta que $phi(ul(X)) = ind(Trulo > z_alpha)$ es el test más potente para _cualquier_ hipótesis alternativa $H^*:=mu=mu^* > mu_0$. Luego, $phi$ es un test UMP para las hipótesis
#v(0.5em)
$
  H_0 := mu = mu_0 thick "vs." thick H_1 := mu > mu_0
$

Como $pi_phi$ es estrictamente creciente,

$
  sup_(mu <= mu_0) pi_phi (mu) = pi_phi (mu_0) = alpha
$
y $phi$ es un test de nivel $alpha$ para la hipótesis nula compuesta $H_0 := mu <= mu_0$. ¿Es acaso el UMP para $H_0 := mu <= mu_0$ contra $H_1 := mu > mu_0$?

Por la condición _necesaria_ del "Lema de Neyman-Pearson" #footnote[p. 39 del #link("https://campus.exactas.uba.ar/pluginfile.php/599144/mod_page/content/12/Clase_11_15Tests_2024_P2.pdf")[PDF] para la teórica #link("https://campus.exactas.uba.ar/mod/page/view.php?id=416449")[Clase 15]] si un test $psi$ es UMP para nuestras hipótesis, será de la forma 
$
  psi(ul(X)) = ind(Lambda(ul(X)) > accent(k, tilde)) = ind(T(ul(X)) > k^*)
$
Recordemos que la función de potencia de $phi = ind(T > k')$ con $T|_(mu=mu^*) ~ "Normal" (gamma mu^*, gamma)$ resultó ser 
$
    pi_phi (mu_1) &= 1 - Phi[z_alpha - sqrt(gamma)(mu_1 - mu_0)], " con " z_alpha = (k' - gamma mu_0) / sqrt(gamma) \
    pi_phi (mu_1) &= 1 - Phi[(k' - gamma mu_0) / sqrt(gamma) - sqrt(gamma)(mu_1 - mu_0)] \
    pi_phi (mu_1) &= 1 - Phi[(k' - gamma mu_1) / sqrt(gamma)] \

$
Considerando $pi_phi (mu_1)$ como función del "punto de corte" $k'$, $g(t) = 1 - Phi[(t - gamma mu_1) / sqrt(gamma)] $, se puede ver que $g'(t) = -Phi'(dot) 1/sqrt(gamma) < 0$, y la potencia de un test _basado en $T$_, $psi_k = ind(T > k)$ para cierta alternativa fija _es decreciente en k_.
Sea entonces $psi = ind(T > k^*)$,
- si $k^* < k' thick arrow.double pi_psi (mu_0) > pi_phi (mu_0) = alpha$, y $psi$ no alcanza el nivel deseado $alpha$ en la hipótesis nula. Además,
- si $k^* > k'thick arrow.double pi_psi (mu_1) < pi_phi (mu_1)$ para cualquier $mu_1 > mu_0$, y el test no es tan potente como $phi$ para la alternativa.
Luego, el único test $psi$ que cumple al mismo tiempo (a) el nivel deseado bajo $H_0 := mu <= mu_0$ y (b) es igual o más potente que $phi$ bajo $H_1 := mu > mu_0$ es $psi = ind(T > k') = phi$, y $phi$ es UMP para $H_0 := mu <= mu_0$ contra $H_1 := mu > mu_0 thick square$
