## Re-resubmission

This is a re-submission. In this version I have fixed two notes yielded by invalid URLs in the documentation of the files R/plant_richness_df.R and R/rf_spatial.R. There was nothing wrong with these URLs, but I have removed them and added the titles, authors and years of the papers instead.

── R CMD check results ───────────────────────────── spatialRF 1.1.4 ────
Duration: 48.3s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

## Re-resubmission

This is a re-submission. In this version I have:

In plant_richness_df.Rd I replaced "[human footprint](https://doi.org/10.1641/0006-3568(2002)052[0891:THFATL]2.0.CO;2)" with "human footprint \doi{10.1641/0006-3568(2002)052[0891:THFATL]2.0.CO;2}" 

## Resubmission

This is a resubmission. In this version I have:

* Removed \donttest{} from five functions that had it combined with if(interactive()).

* Added a @return field to the file print_performance.Rd.

* Ensured that all examples don't use more than two cores.

## Test environments

  + local Ubuntu 20.04, R 4.0.3
  PENDING:
  + rhub
    + "macos-highsierra-release-cran": 0 errors ✓ | 0 warnings ✓ | 0 notes ✓
    + "debian-clang-devel": 0 errors ✓ | 0 warnings ✓ | 1 note x
    + "debian-gcc-devel": PREPERROR (docker issues)
    + "debian-gcc-release": PREPERROR (docker issues)
    + "solaris-x86-patched": 0 errors ✓ | 0 warnings ✓ | 2 notes x
    + "solaris-x86-patched-ods":  errors ✓ | 0 warnings ✓ | 2 notes x
    + "windows-x86_64-devel": 0 errors ✓ | 0 warnings ✓ | 1 note x
    + "windows-x86_64-release": 0 errors ✓ | 0 warnings ✓ | 1 note x
  
## R CMD check results
0 ERRORs | 0 WARNINGs | 1 NOTE.

NOTE (all builds):

Possibly misspelled words in DESCRIPTION:
  al (12:515)
  et (12:512)
  Hengl (12:506)
  MEMs (12:351)
  multicollinearity (12:815)
  Neto (12:383)
  Peres (12:377)
  RFsp (12:500)
  transferability (12:924)
  
NOTE (solaris):

Files ‘README.md’ or ‘NEWS.md’ cannot be checked without ‘pandoc’ being installed.
  
## Pre-release checks

**DESCRIPTION**

  + Proof read Title: and Description:                               YES
  + Title has title case:                                            YES
  + Title is not redundant:                                          YES
  + Software / package names are in quotes:                          YES
  + Description: is not a single sentence:                           YES
  + Only publication titles are double quoted:                       YES
  + Authors@R: includes a copyright holder (role 'cph'):             YES
  + Acronyms are fully expanded the first time they are mentioned:   N/A
  + There are references describing the methods:                     YES
  + References are correctly formatted:                              YES
  + License year is updated:                                         N/A 
  
**DOCUMENTATION**
  
  + Passed documentation through spellcheck:                        YES
  + All links are https rather than http:                           YES
  + Links to CRAN packages are canonical:                           YES
  + Relative links (file URIE) exist or are not broken:             YES
  + All exported functions have @returns and @examples              YES
  + The tag \dontrun is not used in any example:                    YES
  + Examples running for more than 5s are wrapped in donttest:      YES
  + There is no commented code in the @examples sections:           YES
  + Check for un-exported functions with roxygen examples:          DONE
  + Examples run on one thread:                                     YES


**CODE**

  + Check that no function modifies the user's options     DONE
