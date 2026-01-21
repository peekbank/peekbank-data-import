# Fernald and Marchman 2012 ReadME

1. Reference
Fernald, A., & Marchman, V. A. (2012). Individual differences in lexical processing at 18 months predict vocabulary growth in typically developing and late‐talking toddlers. Child development, 83(1), 203-222.  https://doi.org/10.1111/j.1467-8624.2011.01692.x


2. Abstract
Using online measures of familiar word recognition in the looking-while-listening procedure, this prospective longitudinal study revealed robust links between processing efficiency and vocabulary growth from 18 to 30 months in children classified as typically developing (n = 46) and as “late talkers” (n = 36) at 18 months. Those late talkers who were more efficient in word recognition at 18 months were also more likely to “bloom,” showing more accelerated vocabulary growth over the following year, compared with late talkers less efficient in early speech processing. Such findings support the emerging view that early differences in processing efficiency evident in infancy have cascading consequences for later learning and may be continuous with individual differences in language proficiency observed in older children and adults.

3. Original study info
"Participants were 82 children (41 female), recruited from families with 15- to 17-month-old infants identified through birth records. ...

Parents were sent a questionnaire asking about demographic information, language use at home, health history, and family history of language disorders. Parents also completed a MB-CDI: Words and Gestures (Fenson et al., 2006). ...

At 18 months, parents completed the MB-CDI: Words and Sentences. Vocabulary size was the number of words reported to be produced on a checklist of 680 words. ...

Infants’ speed and accuracy in recognizing familiar spoken words were assessed at 18 months in the LWL procedure. ... 

On each trial, children viewed pictures of two familiar objects and listened to speech naming one of the pictures. ... 

Children were tested in two 5-min sessions ca. 1 week apart. ... Prior to the first session, parents indicated on a 

questionnaire whether or not their child understood each of the target words.

Stimuli. Speech stimuli for each session consisted of 32 simple sentences ending with a familiar target noun (e.g., “Where’s the doggy?”),

recorded by a female native speaker of English. "

4. Importing decisions

NOTES: 
the dataset has LWL data longitudinally on 18 / 24 / 30 mo (same kids),
there are non-vanilla trials at 24 and 30 (the LWL at 24 and 30 is not reported in the paper)

For the manju and tempo trials, some were exposure where the object was on a 
background and some were tests where they were not on the background.

IMPORTANT: for related/unrelated prime noun/verb trials, the trials are
represented in the raw data TWICE - once centered on the onset of the verb and
once centered on the onset of the noun. We only keep the trial representation
centered on the onset of the noun.


Decisions on what conditions constitute vanilla trials:

vanilla:
* familiar
* Vanilla
* UnrelPrime-Noun
* UR-primeNoun

non-vanilla:
* Familar-Medial / medial -- contains a novel object that the named entity is on top of (this is true whether the phrase has "on the deebo" or "over there")
* AdjSize
* Adjective 
* AdjColor
* AdjAbsSize
* AbsSize
* RelPrime-Verb
* Relprime-Verb
* R-primeNoun
* Novel-Test       
* FerretExposure
* FerretImmTest
* FerretTest
* FerretRetention
* FerretVerbComp 
                  
(all of the adjectives are contrastive, so non-vanilla)

Right now, the Points of Disambiguation for non-vanilla trials have not been fully 
checked - so the timings of these trials could be inconsistent. TODO, 
if we ever find a use for these non vanilla trials 
In v_2024 this was not solved because we are focusing only on vanilla trials

Carrier phrases:
The file manually_compiled_trial_info_2430.csv in the raw data was manually put
together by peekbank members based on the data in TL2-30A.txt, TL2-30B.txt, 
visit B.txt, visitA.txt. 
The resulting file is used to look up the carrier phrases for 24mo and 30mo.

IMAGES:
Some images were mirrored depending on left/right positioning - image labels L 
and R are from the participants' perspective.

Every stimulus target label has multiple images associated with it, which differ 
in multiple dimensions per target label: version (e.g. there are multiple dogs), 
age (differing versions per age group), side they were shown on, and whether a 
background was present or not. We treated these the following way in the imports:

* version: we treated different versions as different stimuli (e.g. bird1 != birdy2). 
Note: some the images only specify versions using numbers, 
while the datasets sometimes uses letters (A,B) to indicate differing versions. 
We assume that A == 1 and B == 2 for image linking purposes (e.g. kittyA (data) == kitty1 (image path))

* age: most of the images are the same across ages, however, there are some (e.g. book)
that differ between age groups. For consistency, we have decided to keep them as 
different stimuli for all targets (e.g. baby1_18 != baby1_24; book2_18 != book2_24).
Exceptions: Some images were shown to an age group but are missing their specific 
version in the stimulus images folder. In these cases, we replaced the stimulus 
image path to make it point to a version of another age (since most images can be 
assumed to be identical). These are:

  * shoered_24 missing -> we point to shoered_30
  * ballred_24 missing -> we point to ballred_30
  * shoeblue_24 missing -> we point to shoeblue_30
  * ballblue_24 missing -> we point to ballblue_30
  * kitty1_30 and kitty2_30 is missing -> we point to kitty1_24 and kitty2_24
  * birdy1_30 and birdy2_30 is missing -> we point to birdy1_24 and birdy2_24
  
* side: we have decided to only keep the left version of every image, to stay 
consistent with the majority of peekbank

* background present: This only applies to the novel items. The raw data gives 
no indication which version was shown, so we include the image path to the version
WITHOUT a background. The items "novelA" and "novelB" have two versions each: 
one with the moon as a background image, and one with a tangled rope as a background image. 
There is no indication what background was used in the data, so we link the image with the moon background.

* V_2024: trial_order column in the previous code was not reflecting the original tr_num values.
This was fixed, but there is still one administration (sub_num == 10080, session == B, months == 18)
which has repeated trials because two orders were administered (TL2-1, TL2-2).
For the moment, I solved this by adding "order" as a grouping factor for administrations.
(i.e. this subject has two administrations in the same session)

5. Importing ambiguity

Point of disambiguation is tricky for verb and adjective trials - should this be 
the first informative moment (e.g. when an informative verb was mentioned) or at the onset of the noun?

In the raw data, point of disambiguation:
- exposure novel trials: F0 is the onset of the verb
- 24mos: adjective: word onset is the adjective
- 30mos: hard adjective trials: onset of the color/ size