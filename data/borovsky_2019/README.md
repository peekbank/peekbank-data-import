# Borovsky readme

1. Reference 

Borovsky, A., & Peters, R. E. (2019). Vocabulary size and structure affects real-time lexical recognition in 18-month-olds. PloS one, 14(7), e0219290.

2. Abstract

The mature lexicon encodes semantic relations between words, and these connections
can alternately facilitate and interfere with language processing. We explore the 
emergence of these processing dynamics in 18-month-olds (N = 79) using a novel 
approach that calculates individualized semantic structure at multiple granularities
in participants' productive vocabularies. Participants completed two interleaved
eye-tracked word recognition tasks involving semantically unrelated and related
picture contexts, which sought to measure the impact of lexical facilitation and
interference on processing, respectively. Semantic structure and vocabulary size
differentially impacted processing in each task. Category level structure facilitated
word recognition in 18-month-olds with smaller productive vocabularies, while overall
lexical connectivity interfered with word recognition for toddlers with relatively
larger vocabularies. The results suggest that, while semantic structure at multiple 
granularities is measurable even in small lexicons, mechanisms of semantic
interference and facilitation are driven by the development of structure at 
different granularities. We consider these findings in light of accounts of 
adult word recognition that posits that different levels of structure index 
strong and weak activation from nearby and distant semantic neighbors. We also
consider further directions for developmental change in these patterns. 


3. Original study info

79 18-20 month olds participated.

There were two words for each of 6 domains (vehicle, clothing, etc)

During each trial, once children fixed a central fixation, 
the images appeared for 2000ms. Then the child heard "look". Once the child fixated 
the center fixation again for 100ms, the center fixation disappeared and 
the target noun played followed by an encouragement phrase. Images remained on screen
for 4000ms post disambiguation.

Each child saw 24 trials -- each target twice, once with a related distractor and once with
an unrelated distractor. 

4. Importing decisions

Classifying trials in both conditions as vanilla. Semantic relatedness is whether the 
distractor item is from the same super-ordinate category as the target 
(ex: car v airplane or car versus shoe.), but these are still real distinguishable nouns. 


We do not have exclusion data, it seems we have only post-exclusion participants.

The full phrase is incomplete: It contains the carrier phrase, but there is an encouragement phrase after the carrier phrase that was not contained in the data.
According to the paper, these encouragement phrases were things like "That's cool!" and "Do you like it?"

Sampling rate was reverse-engineered from the timestamps. 

We have data only from a fairly short trial duration (max 1700 ms). 
Given that this is the length of the range of the analysis discussed in the paper
(300 - 2000 ms), we are assuming this data is that time window. 

5. Importing ambiguities
none
