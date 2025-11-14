# ferguson eyetrackingr readme

1. **Reference.**
Ferguson, B., Graf, E., & Waxman, S. R. (2014). Infants use known verbs to learn novel nouns: Evidence from 15-and 19-month-olds. Cognition, 131(1), 139-146.

2. **Abstract.**

Fluent speakers’ representations of verbs include semantic knowledge about the nouns
that can serve as their arguments. These ‘‘selectional restrictions’’ of a verb can in principle
be recruited to learn the meaning of a novel noun. For example, the sentence He ate the car-
ambola licenses the inference that carambola refers to something edible. We ask whether
15- and 19-month-old infants can recruit their nascent verb lexicon to identify the refer-
ents of novel nouns that appear as the verbs’ subjects. We compared infants’ interpretation
of a novel noun (e.g., the dax) in two conditions: one in which dax is presented as the sub-
ject of animate-selecting construction (e.g., The dax is crying), and the other in which dax is
the subject of an animacy-neutral construction (e.g., The dax is right here). Results indicate
that by 19 months, infants use their representations of known verbs to inform the meaning
of a novel noun that appears as its argument.

3. **Original study info.**

30 19-month olds and 29 15 month olds were in the final sample. 

On each trial (familiar):
* 6 seconds of preview of 2 images (1 animal, 1 artefact)
* 9 seconds of screensaver while dialogue mentions one of the images by name
* 6 seconds of test phase with the images back on the screen, with "find the [object]" (target noun onset = 500ms)

On each trial (unfamiliar): informative & neutral are between subjects conditions
* 6 seconds of preview of 2 images (both novel still animal, artefact)
* 9 seconds of screensaver while dialogue mentions a nonce word 
  * in "informative" uses a animate-restricted verb "the dax is crying"
  * in neutral, just uses "the dax is right here"
* 6 seconds of test phase with the images back on the screen, with "find the [object]" (target noun onset = 500ms)

Each infant does 6 familiar trials *then* 6 unfamiliar trials. 

Except that according to eyetrackingr documentation, this is on 19 and 24 months...and only the familiar trials. 

4. **Importing decisions.**

I think this is the familiar data from 28 of the 30 19(ish) month olds.
We do not have data from subjects who were excluded, or from other age groups, or for the unfamiliar trials.

Target and distractor information for each trial manually transferred from ancat-aoi.txt.

Because there is a pre-mention of the target (alone) in the "dialogue" phase, these are classed as non-vanilla.
But putting pod as the onset of the noun in the "find the [noun]" while the images are displayed. 

Monitor size from the tobii eyetracking documentation. ("Tobii T60XL Eye Tracker is integrated into a high resolution 24-inch
 1920 x 1080 pixels widescreen monitor")

full AOI XY-coordinates are taken from ancat-aoi.txt

Decision that we will only import data during the "test" phase when the images are back on screen.

5. **Importing ambiguity.**

none 