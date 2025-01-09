# A Core Language for Informalization

(c) Aarne Ranta 2025

## Informath, Core, and Dedukti

This directory contains
- Core, an English CNL for mathematics
- a grammar, parser, and generator for the proof system [Dedukti](https://deducteam.github.io/)
- a translator from Core to Dedukti and vice-versa

[Dedukti](https://deducteam.github.io/) is a minimalistic logical framework aimed as an interlingual between different proof systems such as Agda, Coq, Isabelle, and Lean.
The purpose is to help share formalizations between these systems.
Dedukti comes with an efficient proof checker and evaluator.
Translations from many other proof system to Dedukti have been built, and this work is ongoing.

Technically Dedukti is described as an implementation of Lambda-Pi-calculus with rewrite rules.
It is similar to Martin-Löf's logical framework from the 1980's, except for a more liberal syntax of rewrite rules.
Thereby, it is also similar to the ALF system of 1990's and to the abstract syntax of GF, Grammatical Framework.

Due to its simplicity and expressivity, together with an existing implementation and conversions, Dedukti is a promising choice for the "core type theory" in the Informath project:

![Informath](./informath.png)

In the same way, the Core language defined in this directory is meant to be the "core abstract syntax" of Informath.

In the current set-up, the translations go between Core and Dedukti, ignoring the extensions shown in the above picture. This may indeed be the best way to go. On the type theory side, translations have already been established between other frameworks and Dedukti, and there is no need to extend the interlingual type theory for the purpose of these translations. On the natural language side, the option we will investigate is to keep Core as "extensions" as two separate languages, where the extensions are provided by the [ForTheL](http://nevidal.org/download/forthel.pdf)-based [grammar](../forthel/) in "version 2" of this project. This gives us an updated picture of Informath:

![Informath](./informath-dedukti-core.png)

This directory covers the "formal" and "informal" boxes and "informalization" and "formalization" operations. ForTheL+ refers to the GF implementation of ForTheL that adds some extensions and is open for new ones to cover more of mathematical language.

## The design of Core

Core is a minimalistic grammar for mathematical English. It is based on the following principles:

- **Completeness**: all Dedukti code can be translated to Core.
- **Non-ambiguity**: all Core text has a unique parse tree and a unique translation to Dedukti.
- **Traceability**: Dedukti code and Core text can be aligned part by part.
- **Grammaticality**: Core text is grammatically correct English (with mathematical symbols and some mark-up to prevent ambiguity). 
- **Naturalness**: Core supports natural expressions for mathematical concepts using nouns, adjectives, verbs, and other structures conventionally used in mathematical text.
- **Extensibility**: Core can be extended with lexical information assigning English verbalizations to Dedukti identifiers.

The following propertes are, however, *not* expected:

- **Type correctness**: Core text can be semantically invalid, leading to syntactically correct Dedukti code that is rejected by Dedukti's type checker.
- **Fluency**: Core text can be repetitive and even hard to read; making it better is delegated to ForTheL+ via the NLG component.
- **Compositionality**: The translation between Dedukti and Core is not compositional in the strict sense of GF, as the two languages have different abstract syntaxes. For example, Core supports the aggregation of conjuncts and function argument lists, without which it would be even less readable; and the basic type system is richer than in Dedukti, for instance distinguishing between expressions that represent kinds, objects, and propositions.
- **Natural language input**: while the grammar of Core is reversible, it is tedious to write Core. It is intended to be produced indirectly: by conversion from Dedukti on one hand and ForTheL+ on the other.
- **Multilinguality**: Core has been implemented by GF RGL and is therefore ready for concrete syntax in other languages than English. But since the purpose of Core is not to be the end point, we deem it sufficient to build CoreEng and cover other languages via ForTheL+.

The rationale of this design is modularity and an optimal use of existing resources:

- Type checking is delegated to Dedukti.
- Conversions to different frameworks are also delegated to Dedukti.
- Variation of natural language input and output is delegated to ForTheL+.
- Support for different natural languages is also delegated to ForTheL+ (and, ultimately, to GF RGL).

## Implementation

The following programming languages have been used so far in Informath:
- **GF**: [Grammatical Framework](https://www.grammaticalframework.org/) used for implementing Core and ForTheL+, maximally using its Resource Grammar Library (RGL).
- **Haskell**: used for writing conversions between formal and informal, via embedded GF grammars in the GADT format (Generalized Algebraic Datatypes) supporting almost compositional functions.
- **BNFC**: [BNF Converter](https://bnfc.digitalgrammars.com/), used for implementing Dedukti. The implementation includes a parser, a printer, and an abstract syntax in Haskell, all generated from [this BNF grammar](./typetheory/Dedukti.bnf).
- **Python**: used for generating GF lexica from term dictionaries, in particular [Wikidata](https://www.wikidata.org/). The lexica are currently available as a part of ForTheL+, but the English version will have a connection to Core as well. 