# adams-marchman readme

1. Reference

Adams, K. A., Marchman, V. A., Loi, E. C., Ashland, M. D., Fernald, A., & Feldman, H. M. (2018). Caregiver talk and medical risk as predictors of language outcomes in full term and preterm toddlers. Child Development, 89(5), 1674-1690. https://doi.org/10.1111/cdev.12818

See also Virginia A. Marchman, Melanie D. Ashland, Elizabeth C. Loi, Katherine A. Adams, Anne Fernald & Heidi M. Feldman (2019) Predictors of early vocabulary growth in children born preterm and full term: A study of processing speed and medical complications, Child Neuropsychology, 25:7, 943-963 

Both papers report on subsets of the data from this large, longitudinal dataset. 

2. Abstract

This study examined associations between caregiver talk and language skills in full term (FT) and preterm
(PT) children (n = 97). All-day recordings of caregiver–child interactions revealed striking similarities in
amount of caregiver talk heard by FT and PT children. Children who heard more caregiver talk at 16 months
demonstrated better knowledge- and processing-based language skills at 18 months. The unique contributions
of caregiver talk were tempered by medical risk in PT children, especially for processing speed. However,
there was no evidence that birth status or medical risk moderated the effects of caregiver talk. These ﬁndings
highlight the role of caregiver talk in shaping language outcomes in FT and PT children and offer insights into
links between neurodevelopmental risk and caregiver–child engagement.

3. Original study info

The dataset we have contains 69 unique subject ids, with up to 12 admins/child, 
corresponding roughly to 

* twice at 16 months
* twice at 18 months
* twice at 22 months
* twice at 24 months
* twice at 30 months
* twice at 36 months 




4. Importing decisions

Notes: import_outdated is written using the legacy variant and does not target the
entirety of the dataset (so not equivalent for regression testing purposes) 

Phrases and images are taken from the stimuli folder provided on OSF, titles Totlot 3 - 18

These images go with the age 16 and 18 testing sessions. Stimuli from later testing sessions
are likely the same if they share the same lab label (ex "baby1"), but we aren't totally
sure about this (and so image stim is left blank & they are given their own stimuli ids). 

There are a few cases where multiple trial types have the same phrase, targets, 
and sidedness, but had different internal lab ids because there was associated with
different test sessions (ex 24A and 24B). 

There is one 18mo child who saw the same trial order administration


## conditions and their effect on whether a trial is considered vanilla:

vanilla:
* all trials
* familiar
* name
* VanURP
* UR-primeNoun


non-vanilla:
* medial
* R-primeVerb (filtered out)
* UR-primeVerb (filtered out)
* R-primeNoun
* location - gives hint
* adjective - gives hint
* noveltest
* vcompEat
* vcompDrive
* panjuExp1
* panjuExp2
* panjuExp3
* panjuExp4
* panjuImTest1
* panjuTest2
* panjuTest3
* panjuTest4
* panjuTest5
* modiExp1
* modiExp2
* modiExp3
* modiExp4
* modiImTest1
* modiTest2
* modiTest3
* modiTest4
* modiTest5

