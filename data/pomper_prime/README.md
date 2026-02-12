# pomper prime dataset

## Reference
Pomper, R. & Saffran, J. R. (unpublished). Unpublished "Prime" study: Modulating attention to different features of objects during word learning.

## Original study info

* Short summary from the author: "We taught children the names of novel objects and then tested if they noticed changes in color. Before they were taught the names of each novel object, however, there were 4 prime trials (see two familiar objects, hear a sentence identifying one by name or color). This was counter-balanced between subjects, so each kid had either 16 color trials or 16 name trials. These were the same familiar objects (and stimuli) from Pomper & Saffran (2016). This was a mixture of both Tobii and handcoding data. I have a .csv or .txt file that I can export where the data were combined into a single data frame for analyses."
* NX vs. CX orders track whether the familiar trials are color words or target labels.

## Importing decisions

* There is no associated CDI data.

* Only "Fam" trials are imported. Teaching, test, filler, and end trials
are excluded. Test trials involve novel objects with counterbalanced novel names
(tever, jang, pifo, sprock) so the same visual object gets different names across
orders, and some novel object images (novel4-blue, novel2-green) are never taught. We would need 
additional study info to properly import the trials. As they are low utility, we exclude test trials for now.

* Condition coding: `fam_color` are trials with color word labels (CX orders), and `fam_noun` are trials with object noun labels (NX orders)

* We mark the color trials non-vanilla, only noun trials are vanilla.

## Importing Ambiguity

* Target stimulus labels are the object name, never color words. Color trial
phrases use "one" as the noun (e.g., "the orange one") rather than the object name.
(this info is not present in this dataset, but pomper_saffran_2016 uses the same stimuli, making this the best guess)