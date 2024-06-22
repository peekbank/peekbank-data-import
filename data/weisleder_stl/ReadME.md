#Weisleder & Fernald 2013 ReadME

1. Reference
Weisleder, A., & Fernald, A. (2013). Talking to Children Matters: Early Language Experience Strengthens Processing and Builds Vocabulary. Psychological Science, 24(11), 2143–2152. https://doi.org/10.1177/0956797613488145

2. Abstract
Infants differ substantially in their rates of language growth, and slow growth predicts later academic difficulties. In this study, we explored how the amount of speech directed to infants in Spanish-speaking families low in socioeconomic status influenced the development of children’s skill in real-time language processing and vocabulary learning. All-day recordings of parent-infant interactions at home revealed striking variability among families in how much speech caregivers addressed to their child. Infants who experienced more child-directed speech became more efficient in processing familiar words in real time and had larger expressive vocabularies by the age of 24 months, although speech simply overheard by the child was unrelated to vocabulary outcomes. Mediation analyses showed that the effect of child-directed speech on expressive vocabulary was explained by infants’ language-processing efficiency, which suggests that richer language experience strengthens processing skills that facilitate language growth.

3. Original study info
"Participants were 29 Spanish-learning infants (19 females, 10 males) tested at the ages of 19 and 24 months...

Infants were presented with pairs of images (e.g., of a dog and a baby) while hearing sentences naming one of the pictures. Children were tested on words that are frequent in child-directed speech and are familiar to most children in the participants’ age range, based on the MCDI lexical norms (Dale & Fenson, 1996). When children were 19 months old, the eight target nouns were el perro (dog), el libro (book), el jugo (juice), el globo (balloon), el zapato (shoe), el plátano (banana), la pelota (ball), and la galleta (cookie). When children were 24 months old, four additional familiar words were included: el caballo (horse), el pájaro (bird), la cuchara (spoon), and la manzana (apple). All of the words were presented in simple sentence frames ending with the target noun, for example, “Mira el perro” (“Look at the dog”)... 

Visual stimuli consisted of digital pictures of objects presented in yoked pairs. The pairs were matched for visual salience, the grammatical gender of the object name, and lexical familiarity on the basis of MCDI lexical norms (Dale & Fenson, 1996). Each object was presented an equal number of times as a target and as a distracter... 

When children were 19 months old, the 8 target nouns were presented four times each for a total of 32 test trials; when children were 24 months old, the 12 target nouns were presented three times each for a total of 36 test trials. Side of target presentation was counterbalanced across trials, and trial order was counterbalanced across participants."

4. Importing decisions

The dataset captures its exclusion reasons on a trial level in the "prescreen_notes". Some trials have the prescreen note "e", which is ambiguous. As these prescreen notes are typically used to indicate exclusion, we interpret this "e" to be a short form of "excluded" and mark trials accordingly when importing.

We interpret the aoi coding in the raw data as following:

"offs" and "aways" ("." and "-" in the raw data) were coded as missing. Below is an overview of what each coded value represents in iCoder data:
* “0” ~“distractor” (look to distractor)
* “1” ~ “target” ( look to target )
* “0.5” ~ “other” (child is fixating the center of the screen)
* “.” ~ “missing” (This entry means a shift/ “off” in the language of iCoder. This was coded as “missing” because it can mean multiple things, including that gaze was not codable)
* “-” ~ “missing” (This entry means “away” in the language of iCoder. In practice, this can also correspond to multiple looking situations; e.g., child is turned away from the screen; very long blinks. All of these situations should correspond to the situation “missing”)
left/ right locations were reversed to refflect the participant's perspective, because iCoder files encode left/ right from the perspective of the coder (who has a frontal view of the infant)
* NA ~ missing

Participant 6231 has different entries for sex in the raw data.
Based on numbers reported in original article, this participant should be female.

The translations for the stimulus labels into english were taken from the paper.

Some administrations have significantly less trials than others of the same age group (e.g. 18 months for 5624, 5634 ). These were already missing in the raw data, and we decided to include all the trials that were there, regardless how many trials there were overall for a given administration.

5. Importing ambiguity


 

