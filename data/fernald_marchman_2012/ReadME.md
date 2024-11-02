#Fernald and Marchman 2012 ReadME

1. Reference
Fernald, A., & Marchman, V. A. (2012). Individual differences in lexical processing at 18 months predict vocabulary growth in typically developing and late‐talking toddlers. Child development, 83(1), 203-222.  https://doi.org/10.1111/j.1467-8624.2011.01692.x

2. Abstract
Using online measures of familiar word recognition in the looking-while-listening procedure, this prospective longitudinal study revealed robust links between processing efficiency and vocabulary growth from 18 to 30 months in children classified as typically developing (n = 46) and as “late talkers” (n = 36) at 18 months. Those late talkers who were more efficient in word recognition at 18 months were also more likely to “bloom,” showing more accelerated vocabulary growth over the following year, compared with late talkers less efficient in early speech processing. Such findings support the emerging view that early differences in processing efficiency evident in infancy have cascading consequences for later learning and may be continuous with individual differences in language proficiency observed in older children and adults.

3. Original study info
"Participants were 82 children (41 female), recruited from families with 15- to 17-month-old infants identified through birth records. ...

Parents were sent a questionnaire asking about demographic information, language use at home, health history, and family history of language disorders. Parents also completed a MB-CDI: Words and Gestures (Fenson et al., 2006). ...

At 18 months, parents completed the MB-CDI: Words and Sentences. Vocabulary size was the number of words reported to be produced on a checklist of 680 words. ...

Infants’ speed and accuracy in recognizing familiar spoken words were assessed at 18 months in the LWL procedure. ... On each trial, children viewed pictures of two familiar objects and listened to speech naming one of the pictures. ... Children were tested in two 5-min sessions ca. 1 week apart. ... Prior to the first session, parents indicated on a questionnaire whether or not their child understood each of the target words.

Stimuli. Speech stimuli for each session consisted of 32 simple sentences ending with a familiar target noun (e.g., “Where’s the doggy?”), recorded by a female native speaker of English. 
"

4. Importing decisions

NOTES: 
the dataset has LWL data longitudinally on 18 / 24 / 30 mo (same kids), there are non-vanilla trials at 24 and 30 (the LWL at 24 and 30 is not reported in the paper)

Note: for images, some images were shown in slightly different versions or mirrored in some conditions. Only one version is included here. 

For the manju and tempo trials, some were exposure where the object was on a background and some were tests where they were not on the background.

Some images were mirrored depending on left/right positioning - image labels L and R are from the participants' perspective.

IMPORTANT: for related/unrelated prime noun/verb trials, the trials are represented in the raw data TWICE - once centered on the onset of the verb and once centered on the onset of the noun. We only keep the trial representation centered on the onset of the noun.


Decisions on what conditions constitute vanilla trials:

vanilla:
* familiar
* Vanilla
* UnrelPrime-Noun
* UR-primeNoun
* Familiar-Medial

non-vanilla:
* medial
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
                  

Right now, the Points of Disambiguation for non-vanilla trials have not been fully checked - so the timings of these trials could be inconsistent. TODO, if we ever find a use for these non vanilla trials  

5. Importing ambiguity

Point of disambiguation is tricky for verb and adjective trials - should this be the first informative moment (e.g. when an informative verb was mentioned) or at the onset of the noun?

In the raw data, point of disambiguation:
- exposure novel trials: F0 is the onset of the verb
- 24mos: adjective: word onset is the adjective
- 30mos: hard adjective trials: onset of the color/ size