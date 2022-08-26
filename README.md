# Spellcorrect

Writing a toy spell correction, for the german language, to figure out how it works.

Following [Norvig: How to Write a Spelling Corrector](https://norvig.com/spell-correct.html) and [Jurafsky: Spelling Correction and the
Noisy Channel](https://web.stanford.edu/~jurafsky/slp3/B.pdf)

The error model is not implemented yet, therefore the results are not really useful. To get the error model the next steps are: 
1. Getting a list of common german misspellings
2. Create a model of the error frequencies 
3. Integrate that model in the current implementation.
