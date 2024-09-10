#Yoon and Frank 2015 README

1. Reference
Yoon, E. J., Wu, Y. C., & Frank, M. C. (2015). Children's Online Processing of Ad-Hoc Implicatures. In CogSci.

2. Abstract
Language comprehenders routinely make pragmatic inferences
that go beyond the literal meanings of utterances. If A said “I
ate some of the cookies,” B should infer that A ate some but
not all. Children perform poorly on experimental tests of scalar
implicatures like this, despite their early-emerging sensitivity
to pragmatic cues. Our current work explores potential factors
responsible for children’s successes and failures in computing
pragmatic inferences. In two experiments, we used an eye-
tracking paradigm to test children’s ability to compute impli-
catures when they have access to contextual alternatives to the
target word (Experiment 1), and when they hear prosodic cues
that emphasize the contrast between the target and alternative
(Experiment 2). We found that by the time children are four
years old, they successfully identify the inferential target ref-
erent in this paradigm; with supportive prosodic cues, we saw
evidence of success in three-year-olds as well. In sum, with
sufficient contextual support, preschool children are capable
of making online pragmatic inferences.


3. Original study info
There are two experiments, that according to the paper differ only in the prosody of the target word in the inference condition. 

Paper says children 2-5 for expt 1 and 3-4 for expt 2 (although some kids outside that age range are present in the datafiles, and a wider range of ages is present for expt 2) There are adult controls, which we filter out of the data. 

Children saw 16 trials (4 inference, 4 control-double, 8 control-single, note that there seems to be a typo in the paper). 
Each trial has 3 phases:
* 8.5 seconds (initial phase) -- two images presented and recording with carrier phrase + target word
* 1.5 seconds (anticipatory phase) -- sound plays to induce ""anticipatory gaze"" (unclear what this means)
* 1.5 seconds (feedback phase) -- sesame street character appears by target with sound effect

There were also intermixed attention getters. 

Inference trials show ex. plate with apple and orange versus plate with apple. The question is whether children interpret "Elmo's plate has an apple" to mean the apple-only plate. 

Control-single is plate with apple, plate with orange for phrases like "Elmo's plate has an apple"

Control-double is plate with apple & orange versus plate with apple for phrases like "Elmo's plate has an orange". 


4. Importing decisions
The raw SMI files were available and the available processed files did not have full information. The processing scripts available did not run, but we rewrote equivalent pre-processing. (Note that the old_data / new_data split in raw files is not quite parallel to expt 1 / expt 2)

Less a decision and more a warning -- the stimulus file names for initial and anticipatory are the same, so one has to only use the onset for the anticipatory. 

There are 100 stimuli file names from the (kid only) data. 50 of these seem to be hashes. 32 of these are 2 lists of 16 which align with the image and audio files we have. It looks like the others are duplicates (including onset times) and so we assume that given that all the given information is the same, the actual targets and carrier phrases were as well. 

Trial level exclusions were not present in the data we got, so we applied what is described in the paper. Trials with less than 50% valid looking were dropped, and then kids with less than half valid trials were dropped. There may be slight differences from original as we don't know over what time period per trial the trial level exclusions were calculated (we did over the 10 seconds). 

There are more kids in the data here than are reported in the paper. 
Some of these kids are out of the age range (2-5 in first, 3-4 in second), but also there's just more kids, even after we implement their reported exclusions. 

Study 1:
* 1 yos - 0 reported, 4 in data
* 2 yos -  24 reported, 26 in data
* 3 yos - 28 reported, 30 in data
* 4 yos - 24 reported, 29 in data
* 5 yos - 32 reported, 35 in data
* 6 yos - 0 reported, 2 in data

Study 2:
* 1 yos - 0 reported, 1 in data
* 2 yos - 0 reported, 25 in data
* 3 yos - 17 reported, 24 in data
* 4 yos - 31 reported, 33 in data
* 5 yos -  0 reported, 25 in data

5. Importing ambiguity

It is unclear if the control-single trials count as "vanilla" -- the carrier phrases are atypical, but they are otherwise normal. Decision has been made to label them as vanilla. 
