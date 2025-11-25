# Luchkina Waxman README

1. Reference
Luchkina, E., & Waxman, S. R. (2021). Semantic priming supports infants’ ability to learn names of unseen objects. PloS one, 16(1), e0244968.

2. Abstract

Human language permits us to call to mind representations of objects, events, and ideas
that we cannot witness directly, enabling us to learn about the world far beyond our imme-
diate surroundings. When and how does this capacity emerge? To address this question,
we evaluated infants at 12 and 15 months, asking whether they establish a representation
of a novel noun’s meaning in the absence of any visible referents, and use this representa-
tion to identify a candidate referent when it later becomes available. During training, infants
(67 12-month-olds; 67 15-month-olds) were primed with words and images of objects from
a particular semantic neighborhood (e.g., fruits) and were also introduced to a novel noun
(e.g., “a modi”), used to name a hidden object. During test, infants heard that noun again,
this time with two unfamiliar objects present—one from the primed neighborhood (e.g., a
dragon fruit) and the other from an unrelated semantic neighborhood (e.g., an ottoman).
If infants can represent something about the meaning of the novel noun in the absence of
a visible referent and then use such a representation when a candidate referent appears,
then at test, they should prefer the object from the primed semantic neighborhood. At 15
months, infants succeeded. In contrast, 12-month-olds did not succeed on this task even
after a full week of vocabulary training designed to boost the effect of priming. It is possible
then that 12-month-olds’ representations of novel nouns’ meaning are not yet sufficiently
rich (if any at all) to guide their choice of referent when one does appear. Together, these
findings suggest that the capacity to establish a representation of a novel noun’s meaning
in the absence of any visible referent and use this representation later to identify a candi-
date referent object emerges between 12 and 15 months.

3. Original study info

This data is from the "vocabulary test" of Experiment 2. 
Children had been trained over a preceding week on the names of these 12 objects. 

Subjects were 67 12 month olds 

4. Importing decisions 

These trials were classified as non-vanilla given the double onset of the target nouns. 
For phrases like "Look, a dog! Where is the dog?" the POD was set to the start of the first time 
the target noun "dog" was said. 

The data in this dataset is unbalanced towards the target side, since the targets were almost always more salient/familiar than the distractors.
The trials themselves are fine and usable in the context of the greater peekbank, but looking at the data on a dataset level might be initially confusing due to this imbalance.

Fixed issues:
subject 55 is missing CDI data
subject 5 has duplicate (exactly the same) CDI data

5. Importing ambiguities
none