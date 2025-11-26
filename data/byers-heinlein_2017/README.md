# byers-heinlein_2017 README

1. **Reference.**
Byers-Heinlein, K., Morin-Lessard, E., & Lew-Williams, C. (2017). Bilingual infants control their languages as they listen. Proceedings of the National Academy of Sciences, 114, 9032-9037.

2. **Abstract.**

Infants growing up in bilingual homes learn two languages simultaneously without apparent confusion or delay. However, the mechanisms that support this remarkable achievement remain unclear. Here, we demonstrate that infants use language-control mechanisms to preferentially activate the currently heard language during listening. In a naturalistic eye-tracking procedure, bilingual infants were more accurate at recognizing objects labeled in same-language sentences (“Find the dog!”) than in switched-language sentences (“Find the chien!”). Measurements of infants’ pupil size over time indicated that this resulted from increased cognitive load during language switches. However, language switches did not always engender processing difficulties: the switch cost was reduced or eliminated when the switch was from the nondominant to the dominant language, and when it crossed a sentence boundary. Adults showed the same patterns of performance as infants, even though target words were simple and highly familiar. Our results provide striking evidence from infancy to adulthood that bilinguals monitor their languages for efficient comprehension. Everyday practice controlling two languages during listening is likely to explain previously observed bilingual cognitive advantages across the lifespan.

3. **Original study info.**

Data included is across two experiments (Exp 1 and 3). Conditions in both experiments were originally coded as "same" if the entire prompt is within the same language or "switch" if the target word is in a different language. 
In experiment 1, this manipulation is within-sentence, while in experiment 3 the manipulation is across-sentence. This manipulation was originally stored as the switch type. The switch type and the condition are used to construct the full phrases and collapsed to recode the condition.

- condition: Prompts were Within-sentence_same (e.g. "Look! Find the dog!"), Within-sentence_switch (e.g. "Look! Find the chien!"), Across-sentence_same (e.g. "That one looks fun! The dog!"), or Across-sentence_switch (e.g. "That one looks fun! Le chien!")
- Within-sentence_same and Across-sentence_same are treated as vanilla because the general design follows a canonical LWL structure (non-informative carrier phrases, familiar words, typical event timing)

4. **Importing decisions.**
- trial order datafiles includes filler trials that we do not have eye tracking data for and so are ignored. 

5. **Importing ambiguity.**
none 