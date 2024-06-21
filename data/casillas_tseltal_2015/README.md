# casillas_tseltal_2015 dataset

## reference
Casillas, M., Brown, P., & Levinson, S. C. (2017). Casillas HomeBank Corpus. [https://homebank.talkbank.org/]()

## description
This dataset comes from a looking-while-listening experiment run with children and adult native speakers of Tenejapan Tseltal in a rural subsistence farming village in the highlands of Chiapas, Mexico, in 2015. The data were collected by Marisa Casillas (mcasillas@uchicago.edu) as part of a larger study on language development in this context. The experiment comprised 30 trials total in which pairs of images were shown at the upper left and right corners of a laptop screen while children heard something like, "Look, [object-label], wow" from the laptop speakers. Children's eye movements throughout the task were recorded on a video camera with a wide-angle lens positioned above the laptop. The experiment was run inside a 2m tall, unlit camping tent with children sitting in a guardian's lap on a small foldable stool, between 0.5m and 1m from the camera lens. Most sessions were recorded with the tent inside a shed, though several were recorded with the tent in the outdoors. The experimenter controlled the laptop and video camera from the back exterior of the tent where she could monitor the session on a miniature external monitor and the camera's LCD screen. The looking-while-listening experiment was only run with children ages 13 months and older. All children participated in the experiment as part of a short (~15 min) battery of eye- and head-movement based experiments; looking-while-listening was always the second-to-last experiment in the session, always following other eye-movement experiments. The items were presented in two semi-random orders; starting from participant P27 onward, the order flips from version 1 to version 2.

The experiment data was annotated by a small group of human annotators in [ELAN](https://archive.mpi.nl/tla/elan). LS annotated the onset times and duration of each trial in the entire test battery. LS marked the looking-while-listening trials as "LWL#" with the number going in order from 1 to 30 across trials, regardless of stimulus. IAvT, ME, AB, and SI annotated visual fixations that appeared to focus on the left and right regions of the laptop screen the participant was looking at. IaVT is the primary annotator for the looking-while-listening trials, but many trials have annotations from a second or third coder in order to compute reliability. Annotators coded fixations frame by frame as "L" or "R" from their own perspective (i.e., "R" indicates the child's eyes are fixated on the part of the screen to the annotator's right). Annotators added fixation information without access to the audio or to other annotators' work, using only the task annotations from LS to know when to start and stop annotating fixations. Despite this, sometimes the annotators started or stopped annotations outside of the precise task boundaries; these extra milliseconds can be ignored as they are not linked to the actual trial.

The ELAN files with the trial onset and gaze information were exported to tab-delimited text for read-in and analysis in R. Primary annotator is listed in the metadata table. Other annotator data is ignored for now.

## original study information
Items were chosen by generating a list of imageable household sights and objects which were then informally presented to parents of young children to ask whether a 2-year-old would be able to name the object. Items were pictured on natural surfaces found around homes in this village.

The speaker in the audio stimuli is a young female native speaker of Tenejapan Tseltal who lives in the village. The audio was spliced together such that the prompt was identical across all trials, "Look! ... ". After the object label was given, the trials ended in one of three completions to keep infants' attention.

Design decisions were made following [Fernald et al.'s (2008) chapter](https://psycnet.apa.org/record/2007-18520-004) as closely as possible in this context.

In a first wave, we recruited children between 0;0 and 4;0 for a daylong recording + test session, of which many were between 1;1 and 4;0 (and thereby participated in looking-while-listening trials). In a second wave, we recruited a group of four-year-olds for the test session only. In a third wave, we recruited a group of adults, also for the test session only. As of August 2020, we have only processed data from the first wave. Daylong recording data are reported on elsewhere for these children (Casillas et al., 2019 _DevSci_; in prep; [http://github.com/marisacasillas/Tseltal-CLE]()).

## importing decisions
Importing followed hand-compiled metadata (here on OSF) closely. Recoding decision was flipping the human annotator's codes to make them come from the child's perspective rather than the coder's perspective, thereby matching them to the stimulus Left and Right AOIs. Note that some participants have lower trial counts due to early termination of coding (recorded session was deemed unfit).

Some items are nouns referring to local cultural objects: `bojch` "bowl", `chojak'` "purse", `moch` "basket", `p'in` "pot", `tzek'` "skirt". These were labelled as their ordinary English labels, although they may fully correspond to the more general category label.

# importing ambiguity
We only provide age in round months here, though more fine-grained age data are available.

11 participants (P27 onwards, as noted above) have trial orders that are mismatched between the TXT files and the LOG files in the raw data. We assumed that the LOG files reflect the stimuli, while the TXT files simply reflected trial order, based on the description provided. Note also that some of the participant namings diverge.

## miscellaneous technical details
monitor_size_x = 1316  
monitor_size_y = 768  

aoi_region_sets  
l_x_max = 395  
l_x_min=359  
l_y_max = 754  
l_y_min = 359  
r_x_max = 1366  
r_x_min = 971  
r_y_max = 754  
r_y_min = 359  

We don't have eyetracking data, just aoi data, so for any timepoint t, center x y in image is approximated as
L image, y = 557 x = 198  
R image y = 557, x = 1168

The manual codes specify AOIs as "left", "right", or NA, so the above x/y/pixel is purely supplemental information

The videos of the children's eye movements are recorded at 25fps, which is used as the basis for the time bins by trial
