# Building a lexicon from Wikidata labels

## Building the basic lexicon with just the Wikidata labels

Do
```
  ./build_lexicon.py (-first|-added) <fr> <Fre> <STEPNUM>+
```
with a sequence of some of the following steps:
-  Step 0: preparations  
-  Step 1: extract wikidata for that language into qlist  
-  Step 2: parse with UDPipe  
-  Step 3: use the UDPipe parse to clean up corpus and add to lexicon
-  Step 4: build a lexicon extension
-  Step 5: parse the terms with the extended lexicon  
-  Step 6: (if -first) generate GF modules for abstract and the first concrete  
-  Step 7: (if -add) add a new concrete syntax
-  Step 8: test your grammar in GF\n


To start the build (with no abstract syntax available):
```
   ./build_lexicon.py -first en Eng 1 2 3 4 5 6 8
```
To add a new language (when one language and an abstract syntax are in place):
```
  ./build_lexicon.py -add de Ger 1 2 3 4 5 7 8
```
For more information, take a look at `build_lexicon.py`.


## Building a derived lexicon

This is an experimental method to extract parts of Wikidata labels.
The current implementation extracts APs and CNs from adjectivally modified nouns.
You can run it with
```
  ./derived_lexicon.py <lang>+
```
3-letter language codes are used.
The procedure requires that you have the basic lexicon for the mentioned languages.
The first language (typically Eng) is used in the abstract syntax function names.
