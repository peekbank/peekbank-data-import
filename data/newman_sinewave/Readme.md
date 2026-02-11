# Newman sinewave  Readme

1. Reference
Newman, R. S., Chatterjee, M., Morini, G., & Remez, R. E. (2015). Toddlers' comprehension of degraded signals: Noise-vocoded versus sine-wave analogs. The Journal of the Acoustical Society of America, 138(3), EL311-EL317.

2. Abstract
Recent findings suggest that development changes the ability to comprehend 
degraded speech. Preschool children showed greater difficulties perceiving 
noise-vocoded speech (a signal that integrates amplitude over broad frequency 
bands) than sine-wave speech (which maintains the spectral peaks without the 
spectrum envelope). In contrast, 27-month-old children in the present study 
could recognize speech with either type of degradation and performed slightly
better with eight-channel vocoded speech than with sine-wave speech. 
This suggests that children's identification performance depends critically
on the degree of degradation and that their success in recognizing unfamiliar
speech encodings is encouraging overall.

3. Original study info
Participants: 24 27-month-olds 
Practice trials were cat/dog, critical trials were keys/blocks and car/ball

Each child did 16 trials
* 2 practice trials, 
* 4 test trials in the nondegraded condition 
* 4 in the eight-channel noise-vocoded condition, 
* 4 in the sine-wave analog condition, 
* 2 baseline nondegraded trials that did not name the objects (not included here)

4. Importing decisions

We do not have the data for children who were excluded. 

We observe that the fraction looking to ball and cat are both lower than we might 
expect (naively and in comparison to other targets). I don't think this is an error
but it could be a combination of few trials/kids (especially for cat) and maybe image salience/preference?

We marked this as non-vanilla due to a triple onset of the target word.

5. Importing ambiguity

There are some (~30) frames in this dataset which have two conflicting look. We resolve
these by just taking the preceding look for these conflicts.