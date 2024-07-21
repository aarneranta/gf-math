# grammars on top of Wikidata

## MathWikidata: lexicon

Bootstrapped with ../get_wikidata.py

Manual editing (so far): just the overload cases of mkQN


## MathText: syntax

Started with some structures for definitions

Functor implementation over RGL Syntax

Exceptions: gender-dependent choice of pronouns

Some adjectives added manually because Wikidata has just nouns

Example runs
```
MathText> p -lang=Eng "a binary relation is an equivalence relation if it is reflexive , symmetric and transitive ." | l -bind -treebank
MathText: ParDefinition (DefIsAIf (KindQN binary_relation_Q130901_QN) (KindQN equivalence_relation_Q130998_QN) (CondHasProp RefIt (PropConj and_Conjunction (ConsProperty reflexive_Property (BaseProperty symmetric_Property transitive_Property)))))
MathTextEng: a binary relation is an equivalence relation if it is reflexive , symmetric and transitive .
MathTextFin: binäärirelaatio on ekvivalenssirelaatio jos se on refleksiivinen , symmetrinen ja transitiivinen .
MathTextFre: une relation binaire est une relation d'équivalence si elle est réflexive , symétrique et transitive .
MathTextGer: eine binäre Relation ist eine Äquivalenzrelation wenn sie reflexiv , symmetrisch und transitiv ist .
MathTextIta: una relazione binaria è una relazione di equivalenza se lei è riflessiva , simmetrica e transitiva .
MathTextSwe: en binär relation är en ekvivalensrelation om den är reflexiv , symmetrisk och transitiv .


MathText> p "an abelian group is a group whose binary operation is commutative ." | l -bind -treebank
MathText: ParDefinition (DefWhose (KindQN abelian_group_Q181296_QN) (KindQN mathematical_group_Q83478_QN) (KindQN binary_operation_Q164307_QN) commutative_Property)
MathTextEng: an abelian group is a group whose binary operation is commutative .
MathTextFin: Abelin ryhmä on ryhmä, jonka binäärioperaatio on kommutatiivinen .
MathTextFre: un groupe abélien est un groupe dont l'opération binaire est commutative .
MathTextGer: eine abelsche Gruppe ist eine Gruppe , deren zweistellige Verknüpfung kommutativ ist .
MathTextIta: un gruppo abeliano è un gruppo di cui operazione binaria è commutativa .
MathTextSwe: en abelsk grupp är en grupp vars binära operator är kommutativ .
```

