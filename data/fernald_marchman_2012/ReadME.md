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

there is concern about some of the the word onset columns being wonky 

there is variation between whether word onset is 18/19 or 78 (so there's an extra 2 seconds for 87a-102a) (ask Virginia about this)

extract_col_names seems to do reasonable things

some of the 18 mo data (raw) has false and true instead of 0 and 1 (presumably?)

trying to understand cols in d_processed:
sub_num is a subid (122 values)
session is A or B
months seems to be child age (17-32)
sex is M or F
order is ?? some code?
tr_num ranges 2-42 (trial num)
l_image, r_image, target_side, target_image
condition has a few values ??
word_onset is NA or an integer
response is A,T, or D ??
gap is NA or an int
target_rt_sec and dis_rt_sec is a small number or NA (possibly consistent with 30hz sampling)
shifts is 0 to 7 or NA (my guess is that target_rt_sec, dis_rt_sec and shifts are derived measures)

TODO verify that switching L & R as is done in the code is correct!
TODO should some of the stimuli be "novel" in stimulus table
TODO what is the right empty field for aux data
TODO what is stimulus image path supposed to be 

TODO there are multiple labels (ex. "baby" "baby1") associated with some images (ex "baby1") -- what do? 


5. Importing ambiguity
