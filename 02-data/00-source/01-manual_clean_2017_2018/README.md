# Changelog for hand-processing of datasheets

In an ideal world, all processing would be done via scripts, but it will save a
ton of time on this project to at least prep the datasheets for the scraper
script. This directory stores the hand-processed sheets, which are the raw
datasheets + the changes recorded below.

## 2017 Data

### Moonlight B1-CG-Mid.xlsx

Original uses the 2017 week 1 format, which only has cover length (instead of
recording actual start and end points on each transect). This creates two
issues: First, all of the cell positions in the datasheet are slightly different
and need to be re-located to be read by the scraping script. More imporatntly,
doing the start-end=length calculations in the field was very error-prone (note
how poor the week 1 cover data are).

Hand-processing copy-pasted values from this sheet into a blank template file
based on the 2018 datasheets. Cover start/end points were calculated by starting
with 30m, and successively subtracting the recorded length for each cover
section. This results in some nonsensical values (e.g. negative values for 
start/end of some sections, where more than 30m of length are recorded), but by
taking the lengths at face value we avoid biasing the measurements. Only
precision should be affected.

### Moonlight B1-CG-North.xlsx

Original uses the week 1 format, see notes for "Moonlight B1-CG-Mid.xlsx"

### Moonlight B1-CG-South.xlsx

Original uses the week 1 format, see notes for "Moonlight B1-CG-Mid.xlsx"

### Moonlight B1-CH-Mid_FULL.xlsx

No changes.

### Moonlight B1-CH-North.xlsx

No changes.

### Moonlight B1-CH-South.xlsx

Filled in transect ID "South" in cell B3 based on the filename.

### moonlight B1-CN-mid.xlsx

Original uses the week 1 format, see notes for "Moonlight B1-CG-Mid.xlsx"

### Moonlight B1-CN-North.xlsx

Original uses the week 1 format, see notes for "Moonlight B1-CG-Mid.xlsx"

### Moonlight b1-cn-south.xlsx

Original uses the week 1 format, see notes for "Moonlight B1-CG-Mid.xlsx"

### moonlight b1-eg-mid.xlsx

No changes.

### moonlight b1-eg-north.xlsx

No changes.
### moonlight b1-eg-south.xlsx

Cleared any formatting info from seedlings rows (C29:Z30, C54:Z55, and C79:Z80)
by highlighting and pressing delete key. (Some blank cell was getting scraped.)

### moonlight B1-EH-mid.xlsx

No changes.

### Moonlight b1-eh-north.xlsx

No changes.

### Moonlight B1-EH-South.xlsx

No changes.

### Moonlight B1-EN-Mid.xlsx

Original uses the week 1 format, see notes for "Moonlight B1-CG-Mid.xlsx"

### Moonlight B1-EN-North.xlsx

Original uses the week 1 format, see notes for "Moonlight B1-CG-Mid.xlsx"

### Moonlight B1-EN-South.xlsx

Original uses the week 1 format, see notes for "Moonlight B1-CG-Mid.xlsx"


### Moonlight B2-CG South.xlsx

No changes.

### moonlight b2-cg-mid.xlsx

No changes.

### Moonlight b2-cg-north.xlsx

No changes.

### moonlight b2-ch mid.xlsx

No changes.

### moonlight b2-ch-north.xlsx

No changes.

### moonlight b2-ch-south.xlsx

No changes.

### Moonlight B2-CN-Mid.xlsx

No changes.

### Moonlight B2-CN-North.xlsx

No changes.

### Moonlight B2-CN-South.xlsx

No changes.

### moonlight b2-eg mid.xlsx

No changes.

### moonlight b2-eg-south.xlsx

No changes.

### moonlight b2-eh-mid.xlsx

No changes.

### moonlight b2-eh-north.xlsx

No changes.

### moonlight b2-eh-south dup.xlsx

No changes.

### moonlight b2-en north.xlsx

No changes.

### moonlight b2-en-mid.xlsx

No changes.

### Moonlight B2-EN-South.xlsx

No changes.

### moonlight b3-cg-mid.xlsx

No changes.

### Moonlight b3-cg-north.xlsx

No changes.

### moonlight B3-CG-South.xlsx

No changes.

### moonlight b3-ch-mid

No changes.

### moonlight b3-ch-north.xlsx

No changes.

### Moonlight b3-ch-south.xlsx

No changes.

### Moonlight B3-CN-MID.xlsx

Filled in missing fuels (cells B66:B74 and J76:O76) using on-paper plot notes.

### moonlight b3-cn-north.xlsx

No changes.

### Moonlight b3-cn-south.xlsx

No changes.

### Moonlight B3-EG-MID.xlsx

No changes.

### moonlight b3-eg-north.xlsx

No changes.

### Moonlight b3-eg-south.xlsx

No changes.

### Moonlight b3-eh-mid.xlsx

No changes.

### moonlight b3-eh-north.xlsx

No changes.

### Moonlight b3-eh-south.xlsx

No changes.

### moonlight b3-en-mid.xlsx

No changes.

### moonlight b3-en-north.xlsx

No changes.

### moonlight b3-en-south.xlsx

No changes.


## 2018 Data

### MoonlightB4-CG-MID.xlsx

Changed "0" to "" (highlight and delete key) for cells Q11:Z11 and W36:AB36

### MoonlightB4-CG-N.xlsx

Moved note "???? probably prunus subordata" from cell B5 and appended it to
the existing text in cell C5. *(Note: After examining pictures online, I 
actually am skeptical now that it was prunus subordata, but am waiting on
the ipad photos to make a more positive ID.)*

### MoonlightB4-CG-S.xlsx

Fixed typo in cell J61 from 18.75 to 18.25 (to be consistent with cover end of
previous segment).

Changed "0" to "" (highlight and delete key) for cells S11:Z11, R36:Z36, and
R61:Z61.

### MoonlightB4-CH-MID.xlsx

Changed "Unknown 1" in cell D10 to "UFO1".

Changed ".6m" in cell B80 to "0.6"

Changed "0" to "" (highlight and delete key) for cells G11:Z11, P36:Z36, and
Q61:Z61.

### MoonlightB4-CH-N.xlsx

Changed "0" to "" (highlight and delete key) for cells S11:Z11, Q36:Z36, and 
P61:Z61.

### MoonlightB4-CH-S.xlsx

No changes.

### MoonlightB4-CN-Mid.xlsx

Changed "0" to "" (highlight and delete key) for cells T11:Z11, T36:Z36, and
R61:Z61.

### MoonlightB4-CN-NORTH.xlsx

This had been a .numbers file in icloud (as was MoonlightB5-EG-Mid), which
is probably the issue, I think this is what's causing the import issues. I
have manually copied the data into a template sheet developed by clearing 
the values from MoonlightB4-CG-MID.xlsx, copy/pasting (matching destination
formatting) the values from MoonlightB4-CN-NORTH.xlsx, and re-saving the
filled template over MoonlightB4-CN-North.xlsx. I did the same for 
MoonlightB5-EG-Mid.xlsx

### MoonlightB4-CN-SOUTH.xlsx

Changed "0" to "" (highlight and delete key) for cells T11:Z11.

### MoonlightB4-EG-M.xlsx

Re-ordered cells C60:G63 to start at 30m and work down to 0m (standard
format for cover) to avoid scraping issues. Cleared conditional formatting from 
cells D61:N61, because it was getting screwy with the copy-pasting to re-order 
the cells.

### MoonlightB4-EG-N.xlsx

Changed "0" to "" (highlight and delete key) for cells F11:Z11, U36:Z36, and
U61:AC61.

### MoonlightB4-EG-S.xlsx

No changes.

### MoonlightB4-EH-mid.xlsx

Changed cell E33 from "See notes" to blank. Changed cell E58 from 
"15 * see notes" to "15"

### MoonlightB4-EH-NORTH.xlsx

Changed cell L12 from "O" to "0".

Changed "0" to "" (highlight and delete key) for cells M11:Z11, O36:Z36, and
D11:Z11.

### MoonlightB4-EH-South.xlsx

Changed "0" to "" (highlight and delete key) for cells X11:Z11, M36:Z36, and
X61:Z61.

### MoonlightB4-EN-mid.xlsx

Cleared conditional formatting from D11, F36, and D61.

Swapped cells C11/C12 and C61/C62 to have transects run from 30-0.

### MoonlightB4-EN-N.xlsx

Re-ordered cells C10:H13 to start at 30m and work down to 0m (standard
format for cover) to avoid scraping issues. Cleared conditional formatting from
cells C11:H11, because it was getting screwy with the copy-pasting to re-order 
the cells.

Changed "0" to "" (highlight and delete key) for cells C11:Z11, F36:Z36, and
O61:Z61.

*(Note that species SIMO is a typo, should be SYMO. Will be fixed in 
scripted data cleaning.)*

### MoonlightB4-EN-S.xlsx

No changes.

### MoonlightB5-CG-Mid.xlsx

Removed underline formatting in cell C11, and swapped values of C11 and 
C12 to have the cover start at 30 and end at 0.

Re-ordered cells C35:G38 to start at 30m and work down to 0m (standard
format for cover) to avoid scraping issues.

### MoonlightB5-CG-N.xlsx

Swapped values of C11/C12 and C36/C37 to have cover start at 30 and end
at 0. 

Changed E8 from percentage format to general format.

Changed "0" to "" (highlight and delete key) for cells D11:Z11, D36:Z36, and
J61:Z61.

### MoonlightB5-CG-S.xlsx

Re-ordered cells C10:E13 and C60:G63 to start at 30m and work down to 0m (standard
format for cover) to avoid scraping issues. Swapped values of cells C36 and C37 for the same reason.

Changed cell E58 from "X" to blank.

### MoonlightB5-CH-Mid.xlsx

Swapped cells C11/C12, C36/C37, and C61/C62 to start at 30 and end at 0.

### moonlightB5-CH-North

Re-ordered cells C60:E63 to start at 30m and work down to 0m, cleared conditional formatting from C61:E61.

Swapped values of cells C11/C12 to start at 30m and work down to 0.

Deleted contents of cell D11.

Changed cell B21 from "X" to 0 - there was confusion in the field about
how to record no intersections.

Changed "0" to "" (highlight and delete key) for cells H36:Z36.

### MoonlightB5-CH-S.xlsx

Swapped values of C11/C12, C36/C37, and C61/C62
to start at 30 and end at 0.

Changed "0" to "" (highlight and delete key) for cells D11:Z11, D36:Z36, 
and D61:Z61.


### MoonlightB5-CN-MID.xlsx

Re-ordered cells C10:E13 to start cover at 30m and work down to 0. Cleared
conditional formatting from cells C11:E11.

### MoonlightB5-CN-N.xlsx

Changed "0" to "" (highlight and delete key) for cells F11:Z11, H36:Z36, and
F61:Z61.

### MoonlightB5-CN-S.xlsx

Changed cell H60 from "" to "CECO" (23.25m of cover and 0.75m height is
definitely CECO)

### MoonlightB5-EG-Mid.xlsx

This had been a .numbers file in icloud (as was MoonlightB5-EG-Mid), which
is probably the issue, I think this is what's causing the import issues. I
have manually copied the data into a template sheet developed by clearing 
the values from MoonlightB4-CG-MID.xlsx, copy/pasting (matching destination
formatting) the values from MoonlightB4-CN-NORTH.xlsx, and re-saving the
filled template over MoonlightB4-CN-North.xlsx. I did the same for 
MoonlightB5-EG-Mid.xlsx

Filled in cell D63 from "" to "NA"

### MoonlightB5-EG-N.xlsx

Changed "0" to "" (highlight and delete key) for cells E11:Z11, D36:Z36, and
F61:Z61.

### MoonlightB5-EG-S.xlsx

Filled in missing cover in cell E10 - assume CECO. (25m chunk of 1.25m 
height could only be CECO.) 

### MoonlightB5-EH-Mid.xlsx

Filled in missing cover in cell G10, assume CECO. (12m chunk of 1.25m
height is, again, definitely CECO.)

Swapped cells C36/C37 to start cover at 30m and end at 0m.

In cells B29, B30, B54, B55, B79 and B80, replaced "0" with "X".

Changed "0" to "" (highlight and delete key) for cells H11:Z11, D36:Z36, and
F61:Z61.

### MoonlightB5-EH-N.xlsx

Re-ordered cells C10:G13 to have cover go from 30m to 0m, after changing
height in cell G13 from 0.7 to 0.75 (for height).

Changed "0" to "" (highlight and delete key) for cells F61:Z61.

### MoonlightB5-EH-S.xlsx

Swapped cells C11/C12 to have cover start at 30 and end at 0.

In cells B29, B30, B54, B55, B79 and B80, replaced "0" with "X".

Changed formatting of cell E58 from percentage to general, and value from
0.07 to 7.

Changed cells D35:E35 from "" to "NA"

Changed "0" to "" (highlight and delete key) for cells D11:Z11 and H61:Z61.

### MoonlightB5-EH-middle.xlsx

Changed "0" to "" (highlight and delete key) for cells D11:Z11, H36:Z36, and
D61:Z61.

### MoonlightB5-EN-N.xlsx

Changed cell B36 from "60" to "30", and cell E37 from "30" to "0", which
is the obviously-correct way to interpret the table.

Swapped cells C61 and C62.

### MoonlightB5-EN-S.xlsx

Cell C38 from "" to "NA".

Changed "0" to "" (highlight and delete key) for cells F11:Z11, D36:Z36, and
D61:Z61.

