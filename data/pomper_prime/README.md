# pomper prime dataset

## Reference
Pomper, R. & Saffran, J. R. (unpublished). Unpublished "Prime" study: Modulating attention to different features of objects during word learning.

## Original study info

* Short summary from the author: "We taught children the names of novel objects and then tested if they noticed changes in color. Before they were taught the names of each novel object, however, there were 4 prime trials (see two familiar objects, hear a sentence identifying one by name or color). This was counter-balanced between subjects, so each kid had either 16 color trials or 16 name trials. These were the same familiar objects (and stimuli) from Pomper & Saffran (2016). This was a mixture of both Tobii and handcoding data. I have a .csv or .txt file that I can export where the data were combined into a single data frame for analyses."
* NX vs. CX orders track whether the familiar trials are color words or target labels.

## Importing decisions

* there is not associated CDI data

* decision to only import the familiar trials and not the teaching or test trials. 
We may want to revisit this. 

* vanilla should be only object noun trials, color word trials non-vanilla

## Importing ambiguity

Audio files (in osf) contain the phrases, but we don't know which phrase maps to 
which trial, as all combinations are present. Thus full phrase has been left blank.
