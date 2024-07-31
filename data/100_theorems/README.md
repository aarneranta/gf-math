# The 100 Theorems

According to Freek Wiedijk, https://www.cs.ru.nl/~freek/100/ 


## Formalizations in Naproche

https://github.com/naproche/naproche/blob/master/examples/100_theorems.ftl.tex

contains 10 of the 100

## A first test with GF

The grammars in [100thm](../../grammars/100thm/) extend the big WikiMath grammar with constructs
and words that are either
- not found in Forthel.gf
- not found in a particular language


The test presupposes a compilation of the `100thm` grammars:
```
  gf -make Ext100Math???.gf
```
After that, we run a script in the current directory:
```
  cat modif100lexed.txt | python3 ../../grammars/forthel/test_file.py \
  ../../grammars/100thm/Ext100Math.pgf Ext100MathEng TERMINDEX.json
```
Piping and redirecting this with
```
  | grep -v TREE > ex100.tex
```
gives us the following section.


## First results, generated but not cleaned up

-- SUCCESS 1 $ q^{2} = p $ for no positive rational number $ q $ .
-- TRANS Ext100MathFre $ q^{2} = p $ pour aucun nombre rationnel positif $ q $.
-- TRANS Ext100MathGer $ q^{2} = p $ für keine positive rationale Zahl $ q $.

-- SUCCESS 2 the collection of prime natural numbers is infinite .
-- TRANS Ext100MathFre la collection d'entiers naturels primaires est infinie.
-- TRANS Ext100MathGer die Sammlung von unteilbaren natürlichen Zahlen ist unendlich.

-- SUCCESS 3 let x , y be sets . $ x $ and $ y $ are equinumerous iff there exists a injective map from $ x $ to $ y $ and there exists an injective map from $ y $ to $ x $ .
-- TRANS Ext100MathFre soit x , y un ensemble. $ x $ et $ y $ sont équinombreux si et seulement si il existe une correspondance injective de $ x $ à $ y $ et il existe une application injective de $ y $ à $ x $.
-- TRANS Ext100MathGer seien x , y Mengen. $ x $ und $ y $ sind gleichzahlig wenn und genau dann wenn es eine injektive Abbildung aus $ x $ nach $ y $ gibt und es einen injektiven [differentiable_map_Q77989741_der_CN] [differentiable_map_Q77989741_der_CN] aus $ y $ nach $ x $ [differentiable_map_Q77989741_der_CN] [differentiable_map_Q77989741_der_CN] gibt.

-- SUCCESS 4 for all finite sets $ X $ and all natural numbers $ n $ , if $ |X| = n $ , then $ \pow(X) $ is finite and $ |\pow(X)| = 2^{n} $ .
-- TRANS Ext100MathFre pour tous les ensembles finis $ X $ et tous les entiers naturels $ n $, si $ |X| = n $, alors $ \pow(X) $ est fini et $ |\pow(X)| = 2^{n} $.
-- TRANS Ext100MathGer für alle endlichen Mengen $ X $ und alle natürlichen Zahlen $ n $ , wenn $ |X| = n $ , dann ist $ \pow(X) $ endlich und $ |\pow(X)| = 2^{n} $.

-- SUCCESS 5 let $ s , t $ be real numbers such that $ s < t $ . then there exists a real number $ z $ such that $ s < r < t $ .
-- TRANS Ext100MathFre soit $ s , t $ un nombre tel que $ s < t $. alors il existe un nombre $ z $ tel que $ s < r < t $.
-- TRANS Ext100MathGer seien $ s , t $ reelle Zahlen derart dass $ s < t $. dann gibt es eine reelle Zahl $ z $ derart dass $ s < r < t $.

-- SUCCESS 6 let $ M $ be a set . then there exists no surjection from $ M $ onto the powerset of $ M $ .
-- TRANS Ext100MathFre soit $ M $ un ensemble. alors il n'existe aucune surjection de $ M $ sur l'ensemble puissance de $ M $.
-- TRANS Ext100MathGer sei $ M $ eine Menge. dann gibt es keine Surjektion aus $ M $ auf die Potenzmenge $ M $.

-- SUCCESS 7 $ \sumgeom{x}{n} = \frac{1 - x^{n}}{1 - x} $ for all natural numbers $ n $ .
-- TRANS Ext100MathFre $ \sumgeom{x}{n} = \frac{1 - x^{n}}{1 - x} $ pour tous les entiers naturels $ n $.
-- TRANS Ext100MathGer $ \sumgeom{x}{n} = \frac{1 - x^{n}}{1 - x} $ für alle natürlichen Zahlen $ n $.

-- SUCCESS 8 $ \sumarith{a}{d}{n} = n \cdot ( a + \frac{(n + 1) \cdot d}{2}). $ .
-- TRANS Ext100MathFre $ \sumarith{a}{d}{n} = n \cdot ( a + \frac{(n + 1) \cdot d}{2}). $.
-- TRANS Ext100MathGer $ \sumarith{a}{d}{n} = n \cdot ( a + \frac{(n + 1) \cdot d}{2}). $.

-- SUCCESS 9 let $ m , n $ be natural numbers such that $ m < n $ . then the greatest common divisor of $ m $ and $ n $ is the greatest common divisor of $ n-m $ and $ m $ .
-- TRANS Ext100MathFre soit $ m , n $ un entier naturel tel que $ m < n $. alors le plus grand commun diviseur de $ m $ et de $ n $ est le plus grand commun diviseur de $ n-m $ et de $ m $.
-- TRANS Ext100MathGer seien $ m , n $ natürliche Zahlen derart dass $ m < n $. dann ist der größte gemeinsame Teiler $ m $ und $ n $ der größte gemeinsame Teiler $ n-m $ und $ m $.

-- SUCCESS 10 assume $ A \subseteq \mathbb{N} $ and $ 0 \in A $ and for all $ n \in A $ , $ n + 1 \in A $ . then $ A = \mathbb{N} $ .
-- TRANS Ext100MathFre supposons que $ A \subseteq \mathbb{N} $ et $ 0 \in A $ et pour tout $ n \in A $, $ n + 1 \in A $. alors $ A = \mathbb{N} $.
-- TRANS Ext100MathGer wir nehmen an, dass $ A \subseteq \mathbb{N} $ und $ 0 \in A $ und für alle $ n \in A $ , $ n + 1 \in A $. dann $ A = \mathbb{N} $.
success 10 failure 0

