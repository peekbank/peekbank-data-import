# Reflook socword  Readme

1. Reference
Yurovsky, D., Wade, A., & Frank, M. C. (2013). Online processing of speech and social information in early word learning. In Proceedings of the 35th Annual Meeting of the Cognitive Science Society.

Yurovsky, D., Wade, A., Kraus, A. M., Gengoux, G. W., Antonio, Y., & Frank, M. C. (2017). Developmental changes in the speed of social attention in early word learning. (unpublished)
(available at https://callab.uchicago.edu/papers/ywkghf-ur.pdf)

2. Abstract

Although word learning unfolds over days, weeks, and months,
individual naming events are over in a matter of seconds. To
benefit from a naming event, children must at least hear the la-
bel and see the referent. We tested 1-, 2- , 3-, and 4-year old
children in a naturalistic word learning task with two condi-
tions: one that taxed both speech processing and rapid gaze-
following, and one in which a social cue-to-reference was
available for an extended time. The development of word-
learning in the extended condition paralleled the development
of speech processing, but learning in the brief condition lagged
behind. However, learning from both the brief and extended
cues was predicted by individual differences in speech pro-
cessing and cue-following together. Thus, even through the
4th year, real-time processing of social and linguistic informa-
tion are a critical bottleneck for word learning.

(Abstract from developmental changes...)
How do children learn words so rapidly? A powerful source of information about a new
word’s meaning is the set of social cues provided by its speaker (e.g. eye-gaze). Studies of
children’s use of social cues have tended to focus on the emergence of this ability in early
infancy. We show, however, that this early-emerging ability has a long developmental
trajectory: Slow, continuous improvements in speed of social information processing occur
over the course of the first five years of life. This developing ability to allocate social
attention is a significant bottleneck on early word learning—continuous changes in social
information processing predict continuous changes in children’s ability to learn new words.
Further, we show that this bottleneck generalizes to children diagnosed with autism
spectrum disorder, whose social information processing is atypical. These results describe a
route by which increases in social expertise can lead to changes in language learning ability,
and more generally highlight the dependence of developmental outcomes not on just the
existence of particular competencies, but on their proficient use in complex contexts.

3. Original study info

This study is reported in the cogsci paper, and also as expt 3 in an unpublished ms. 
The unpublished ms includes more children and also describes more trials. (only the first 
part of each expt is in the cogsci paper). 

(from the unpublished ms)
425 children ages 125 were tested, with 200 excluded for a final sample of 225 children:
61 1-year-olds (25 girls); 57 2-year-olds (30 girls); 51 3-year-olds (25 girls); and 50 4-year-olds (24 girls))

Each child did 8 familiar trials, learning trials (not included here),
8 test trials with the novel object shown against a consistent distractor
(4 in brief, 4 in extended)  and 4 trials with the two novel objects against each other. 


4. Importing decisions

Note that some of the import processing code is in functions in import_helpers.R.

We do not have access to all the child-level info needed to match the exclusions reported in the paper. 
We have marked the exclusions for prematurity (assuming this lines up with atypical
developmental trajectories) and for <75% English. We have not re-recreated exclusions
based on eye-tracker calibration. 

We excluded trials that are length 0. We exclude children without age information. 

We do not have the image stimuli, although we might be able to in the future. 

5. Importing ambiguity
no known ambiguities
