
1. **Reference.**

TODO: This needs an update, this citation is outdated and the one in the code points to an osf link that is no longer valid

Yurovsky, D., Wade, A., Kraus, A. M., Gengoux, G. W., Hardan, A. Y., Frank, M. C. (under review). Developmental changes in the speed of social attention in early word learning.

2. **Abstract.**

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

3. **Original study info.**

TODO: Clear this up

This dataset seems to correspond to a superset of participants from Experiment 1 of the paper. 

Stimuli:
- 2 novel words: "toma" and "fep"
- 8 familiar object words (dog, car, lamp, carrot, book, clock, frog, banana)

Design:
- Learning phase: dialogues/monologues in which speakers introduced novel objects using social cues like gaze or reaching
- Test phase: 16 test trials total in a forced choice lwl setting
  - 8 familiar word trials
  - 8 novel word trials (pairing the two novel objects)

Conditions:
- familiar: Trials testing comprehension of familiar words
- novel: Trials testing learning of novel words "toma" and "fep"


4. **Importing decisions.**

- We removed trials from the data that ended before the point of disambiguation.
- Negative left x-coordinates in AOI definitions are clamped to 0 instead of being treated as errors or
  excluded.
- All participants are hardcoded as English native speakers. This assumes the dataset doesn't include the <75% English exposure participants that the paper mentions excluding (there is no exclusion information given by the paper).
- Only the test trials are included, the learning phases where participants watched videos that featured the stimuli are not included.

5. **Importing ambiguity.**

TODO: exclusion information exists but is not used due to lack of documentation about what the codes mean.
