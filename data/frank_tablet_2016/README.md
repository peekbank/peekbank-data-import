# frank_tablet_2016 dataset

## Reference
Frank, M. C., Sugarman, E., Horowitz, A. C., Lewis, M. L., & Yurovsky, D. (2016). Using tablets to collect data from young children. Journal of Cognition and Development, 17(1), 1-17.

## Abstract
Mobile, touch-screen devices are increasingly ubiquitous in children’s lives. The extensive use of such devices presents an exciting opportunity for data collection. We describe a simple method for creating cross-platform, interactive tablet experiments using open Web-based resources. We illustrate this method by collecting data from 1- to 4-year-old children in a word-recognition paradigm, using 3 different techniques: tablets, eye tracking, and an in-person storybook paradigm. Both accuracy and reaction-time data from the tablet compared favorably with the other methods. Tablets should be considered as a viable method for collecting low-cost, well-controlled developmental data.

## Original study info
The data are from the eye-tracking portion of the study, collected using a SMI RED 120Hz system. 
Children first completed a simple two-point calibration and then saw each trial presented on a static slide, accompanied by audio (played over speakers). 
Children were seated in a car seat mounted in an adjustable-height barber chair.

Exclusion reasons included (a) insufficient exposure to English ("lang") and (b) parental interference ("interference").

The trials included (a) familiar-word trials (two familiar objects), (b) mutual exclusivity control trials (familiar target, novel distractor), and (c) mutual exclusivity inference trials (novel target, familiar distractor).

## Importing decisions

### AOI region sets
The AOI coordinates are hard-coded, based on the original MATLAB analysis script:
https://github.com/langcog/tablet/blob/44edf4c4a96d73d88b5440eba9e1347ee0033881/eye_tracking/MATLAB/CONSTANTS_TAB_COMP.m#L34

The original values (in screen coordinates, top-left origin) were:
- Left AOI: `[0 533 300 700]`
- Right AOI: `[1067 1800 300 700]`

The y-values were flipped to match the Peekbank coordinate origin, and `r_x_max` was clamped from 1800 to 1600 (likely a typo in the Matlab script). The lower y-bound was also extended from 500 to 400 to capture legitimate the activity below the original boundary.


## Importing ambiguity
Some participants were not recorded in the demographics.
As a result, these participants had NAs for exclusion; these were replaced with 0s (i.e., not excluded).

Carrier phrase: The paper states 

"Audio stimuli consisted of a carrier phrase (“Can you find the [target]?”), into which we
spliced recordings of the target words"

This makes it slightly ambiguous as to whether there is only one carrier phrase or different ones, but we assume that there is only one carrier phrase when importing.
