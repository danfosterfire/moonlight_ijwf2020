# Raw Data from 2017 Field Measurements

## Datasheet Explanation

**FILENAMES** are formatted as "Moonlight [block]-[plot]-[transect]" by default. In 2017, we were only working on the early-seral project, so all of these datasheets are for the early seral project.

### Header Fields (1x per datasheet)

**BLOCK** is the block number (sometimes coded as "B[number]", sometimes as just the number) for the randomized-complete-block study design. Each of the six plots within a block receives a unique treatment combination. There are five blocks total within the early seral study: Two located along Pierce Creek, one along Hungry Creek, and two in the Moonlight Valley. Three of these blocks were measured in 2017 (B1, B2, and B3), and the remaining two were measured in 2018. 

**PLOT** is a two-letter code describing the planned treatment regime for the plot. All plots receive the same initial treatment, which is a mechanical removal of the shrubs, ripping them out by the stems rather than masticating. This work began in May 2018.

**TRANSECT** has three levels (North, Middle, and South), which are coded as text. In the raw data, the formatting is not consistent (e.g. "M" vs. "mid" vs. "MID"). There were three 90m transects within each plot, and each transect is recorded on an individual datasheet (.xlsx file).

**DATE** records the date (in MM/DD/YYYY format) the transect was measured.

**CREW** is not consistently filled, but where present gives the initials of the crewmembers which measured the transect.

**TOTAL TIME** is rarely filled and can be safely ignored. The intention was to record the amount of time spent measuring each transect.

**SLOPE** would be the transect-level slope. In the field, we recorded slope for each 30m sub-transect instead, and this field is usually blank.

**PLOTNOTES** text describing relevant qualitative observations and idiosyncracies

### SUBTRANSECT Fields (3x per datasheet)

Crews moved from west to east when measuring all transects. So subtransect 1 (0m-30m) is always the westernmost 30m and subtransect 3 (60m-90m) is always the easternmost 30m.

**SLOPE** is the average slope (in %) across the 30m subtransect, recorded in the field with clinometers.

**Vegetation** cover is described in a wide-format table. Each column is an observation, and for each observation (segment of continous cover along the transect) we recorded the species, start point (meters along the transect), end point (meters along the transect), and average height (in meters). Cover type is generally recorded with a four-letter code for the dominant plant species, the first two letters of the genus followed by the first two letters of the species name. See notes for more details on species codes.

**Fuels** table records measurements of litter, duff, and fuelbed depths alongside tallies for 1- 10- and 100-hour fuels.  Rows are labeled by measurement type (e.g. LITTER3 for the litter depth at 3m, 1HOUR for the tally of 1-hour fuels between 3m and 6m). Each portion of the 30m subtransect (0-10m, 10-20m, or 20-30m) is a column, so (for example) the fuelbed depth at 17m along the subtransect can be found in the second column (10-20m) and bottom row (FUEL7). Litter, duff, and fuel are depths in cm; 1HOUR, 10HOUR, and 100HOUR are counts.

**1000-HOUR** fuels are recorded as a wide one-row table. Each cell represents a single observation, with diameter (cm) and a letter signifying sound or rotten status. Crews followed Brown (1974)'s general distinction for sound/rotten fuels: "Consider pieces rotten when the piece at the intersection is obviously punky or can be easily kicked apart."

**TREE SEEDLINGS AND SAPLINGS** is a wide-format table. The species (4-letter code) and height of seedlings or saplings >10cm height within 1m on either side of the transect tap were recorded. The protocol calls for height recorded in meters, but heights may be recorded in cm (especially in 2017 data). The distinction should be obvious. An observation of species "X" and height "X" indicates that there were no seedlings or saplings in the 30m x 2m belt transect we searched. However, the "X's" were not rigorously recorded, and blanks can also be safely interpreted as "no seedlings" rather than "no data".

## Field Protocol

For now, see the top-level README doc for the Moonlight repository and 2017-early_seral_protocol-stevens.pdf in the context/planning/ folder.

## Notes

### Establishing plot corners and transects on the ground

In 2017, the crew established plot locations using corner points established
in GIS by Jens Stevens (see "context/maps/Reforestation Corner Points" for the 
final corner points) and located in the field with a high-precision Trimble 
GeoXT GPS. Due to a projection error, the initial set of corner points defined 
plots which were only ~73m to a side, rather than 90m. The field crew treated 
the NW corner of each plot as canonical, and established transect starts from 
the northwest corner for all plots. We continued this practice even after 
getting corrected corner points, and in 2018. 

All transect starts were placed by using the GPS to navigate to the plot's
NW corner, and then using a compass and hypsometer rangefinder to navigate
22.5m due south three times, thus establishing three transect starts along the
west edge of the plot. Crews used compasses and reel tapes to navigate and make
measurements from west to east along each transect.

In 2017, the crew installed rebar at *most* of the corrected corner points, but
due to concerns about the rebar posing a hazard to heavy equipment  used during
the shrub-removal treatment, we removed all but two of the rebar points in 
2018. (The remaining two points were extensively flagged, and coordinates 
provided to PNF to give to the contractor.) 

### DWD as a cover type

When recording vegetation cover, if the dominant (highest) type of cover was
down woody debris (a stump, large down log, bark, etc.) we recorded it as a
segment of cover by species "DWD" (down woody debris), with a start/end point
and average height (e.g. of a large log). We maintained this protocol in 2018
for consistency, but segments of DWD should be interpreted as bare ground for
the purposes of describing vegetation cover. Data cleaning will apply this
conversion.

### Cover length wonkiness

The first draft datasheet the crew used in 2017 used species, height, and
length of intersection (rather than start/end points) to describe cover. We
quickly realized that this was a poor decision (doing the start-end=length
arithmetic in the field was slow and error-prone) and changed the format of the
datasheets for other measurements. However, many of the transect lengths from
the first week of 2017 (B1-CG, B1-CN, and B1-EN) and some later transects
do not add up to 30m due to arithmetic errors and/or typos in the field.
(Most of these total-length errors are minor, on the scale of 30m +/- 1m. 
However, a few datasheets describe transects which are 30m +/- 7m.) The
statistically-sound way to use these data would be to convert everything to
% cover = recorded section length / total recorded length (rather than % cover = 
recorded section length / 30m).

*To be clear:* In all cases, crews actually
measured 30m transects in the field. Datasheets indicating more or less than
30m of cover are due to transcription errors in the field. In 2018, datasheets 
included validation and conditional formatting measures to 
avoid or mitigate this problem, and it is much less of a problem.

### First week cover formatting

Major changes were made to the datasheet formatting after the first week of
data collection in 2017, and minor changes were made at the start of the 2018
season.