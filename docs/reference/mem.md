# Compute Moran's Eigenvector Maps from distance matrix

Computes Moran's Eigenvector Maps (MEMs) from a distance matrix. Returns
only eigenvectors with positive spatial autocorrelation, which capture
broad to medium-scale spatial patterns.

## Usage

``` r
mem(distance.matrix = NULL, distance.threshold = 0, colnames.prefix = "mem")
```

## Arguments

- distance.matrix:

  Numeric distance matrix between spatial locations.

- distance.threshold:

  Numeric value specifying the maximum distance for spatial neighbors.
  Distances above this threshold are set to zero. Default: `0` (no
  thresholding).

- colnames.prefix:

  Character string used as prefix for column names in the output.
  Default: `"mem"`.

## Value

Data frame where each column is a MEM (spatial predictor) representing a
different scale of spatial pattern. Columns are named with the pattern
`<prefix>_<number>` (e.g., "mem_1", "mem_2").

## Details

Moran's Eigenvector Maps (MEMs) are spatial variables that represent
spatial structures at different scales. The function creates MEMs
through the following steps:

1.  Double-centers the distance matrix using
    [`double_center_distance_matrix()`](https://blasbenito.github.io/spatialRF/reference/double_center_distance_matrix.md)

2.  Computes eigenvectors and eigenvalues using
    [`base::eigen()`](https://rdrr.io/r/base/eigen.html)

3.  Normalizes eigenvalues by dividing by the maximum absolute
    eigenvalue

4.  Selects only eigenvectors with positive normalized eigenvalues

**Positive vs. negative eigenvalues:**

Eigenvectors with positive eigenvalues represent positive spatial
autocorrelation (nearby locations are similar), capturing broad to
medium-scale spatial patterns. Eigenvectors with negative eigenvalues
represent negative spatial autocorrelation (nearby locations are
dissimilar) and are excluded. The returned MEMs are ordered by
eigenvalue magnitude, with the first columns capturing the broadest
spatial patterns.

These MEMs are used as spatial predictors in
[`rf_spatial()`](https://blasbenito.github.io/spatialRF/reference/rf_spatial.md)
to account for spatial autocorrelation in model residuals.

## See also

[`mem_multithreshold()`](https://blasbenito.github.io/spatialRF/reference/mem_multithreshold.md),
[`rf_spatial()`](https://blasbenito.github.io/spatialRF/reference/rf_spatial.md),
[`double_center_distance_matrix()`](https://blasbenito.github.io/spatialRF/reference/double_center_distance_matrix.md)

Other spatial_analysis:
[`filter_spatial_predictors()`](https://blasbenito.github.io/spatialRF/reference/filter_spatial_predictors.md),
[`mem_multithreshold()`](https://blasbenito.github.io/spatialRF/reference/mem_multithreshold.md),
[`moran()`](https://blasbenito.github.io/spatialRF/reference/moran.md),
[`moran_multithreshold()`](https://blasbenito.github.io/spatialRF/reference/moran_multithreshold.md),
[`pca()`](https://blasbenito.github.io/spatialRF/reference/pca.md),
[`pca_multithreshold()`](https://blasbenito.github.io/spatialRF/reference/pca_multithreshold.md),
[`rank_spatial_predictors()`](https://blasbenito.github.io/spatialRF/reference/rank_spatial_predictors.md),
[`residuals_diagnostics()`](https://blasbenito.github.io/spatialRF/reference/residuals_diagnostics.md),
[`residuals_test()`](https://blasbenito.github.io/spatialRF/reference/normality.md),
[`select_spatial_predictors_recursive()`](https://blasbenito.github.io/spatialRF/reference/select_spatial_predictors_recursive.md),
[`select_spatial_predictors_sequential()`](https://blasbenito.github.io/spatialRF/reference/select_spatial_predictors_sequential.md)

## Examples

``` r
data(plants_distance)

# Compute MEMs from distance matrix
mems <- mem(distance.matrix = plants_distance)

# View structure
head(mems)
#>          mem_1        mem_2       mem_3         mem_4        mem_5        mem_6
#> 1 -0.025921732  0.005220262 -0.04169686 -0.0363323858 -0.029427120 -0.004345679
#> 2 -0.099667898  0.053971309 -0.13244795  0.3826927819  0.205013057  0.006481398
#> 3 -0.001047654 -0.014304635  0.04436019 -0.0031386301 -0.026240412  0.036601404
#> 4 -0.016569475  0.004799116 -0.03074566  0.0005170325 -0.014444892 -0.003356330
#> 5 -0.022576055  0.001959546 -0.02303677 -0.0524239401  0.006729439 -0.006797669
#> 6 -0.015525218  0.002374152 -0.01979535 -0.0338955963 -0.007884866 -0.002744701
#>           mem_7        mem_8        mem_9       mem_10       mem_11
#> 1  0.0059417525 -0.003509625 -0.006296265  0.002487306  0.005395791
#> 2 -0.0338063656  0.174542054  0.024450222  0.005829746 -0.054961827
#> 3 -0.0616077280  0.019577234 -0.001448598 -0.018299683  0.039246420
#> 4  0.0009804302  0.010526820 -0.002889298  0.008730811  0.002146499
#> 5  0.0025571100 -0.011762268  0.004846323 -0.045296964 -0.039538738
#> 6  0.0029345255 -0.007054664 -0.002369704 -0.009715209 -0.004288805
#>         mem_12       mem_13        mem_14       mem_15       mem_16      mem_17
#> 1 -0.011379866  0.191604368 -0.0072850442 -0.026412798  0.109791768  0.12559194
#> 2  0.153839059  0.019835793 -0.0057251178 -0.062892949 -0.003832431  0.03668636
#> 3 -0.008007024 -0.014692044 -0.0201202412  0.008020865  0.048006557 -0.17744495
#> 4  0.007681169 -0.008201179  0.0001369046  0.006005988 -0.024858297 -0.01844209
#> 5 -0.011040322  0.164381811 -0.0088932051 -0.021214181  0.108145269  0.02998075
#> 6 -0.009536937  0.178202420 -0.0060291344 -0.016907370  0.083504053  0.06773982
#>         mem_18       mem_19       mem_20      mem_21      mem_22      mem_23
#> 1  0.107268296 -0.012273527  0.046229850 -0.04237342  0.03596611  0.01118606
#> 2  0.014722504  0.202464657  0.082229660  0.15771710  0.04038242 -0.11437678
#> 3  0.150866603 -0.003222866  0.030550230  0.08951193 -0.04504866  0.13461198
#> 4 -0.009470203 -0.011830035  0.040433340 -0.02567659 -0.06842079 -0.01692236
#> 5 -0.013711486  0.034505110 -0.178892320  0.04343963 -0.13917976 -0.04071620
#> 6  0.047243449 -0.009624029  0.008015184 -0.03834844 -0.05776603 -0.02652804
#>        mem_24        mem_25      mem_26        mem_27      mem_28       mem_29
#> 1  0.10224462 -0.0011848037 -0.02412275 -0.0007539716 -0.01644154 -0.052111626
#> 2 -0.01012395 -0.0013068838 -0.07838816  0.0235252584 -0.03939033  0.003796367
#> 3 -0.02047940 -0.0387798586 -0.07535325  0.1064394216  0.04395511 -0.014889858
#> 4  0.10377418  0.0049076695  0.03958842  0.0382303597 -0.01956660  0.097801181
#> 5  0.05601777 -0.0005085273 -0.04327854  0.0018750185 -0.02127332 -0.066337960
#> 6  0.09311909 -0.0021059632 -0.04019586  0.0116133257 -0.04411492 -0.104538187
#>         mem_30       mem_31       mem_32        mem_33        mem_34
#> 1  0.065321865 -0.009110300 -0.005527607  0.0024141508  0.0995535458
#> 2 -0.003727503  0.006294934 -0.009297452 -0.0011867862 -0.0019609284
#> 3 -0.004292680 -0.145501124  0.048790879  0.1471245843  0.0009205415
#> 4 -0.133857876  0.009342614 -0.004236143  0.0002171203 -0.0217659867
#> 5  0.089123320 -0.008281460  0.005782912 -0.0083726169 -0.0976154193
#> 6  0.131117350 -0.014943405 -0.004594169 -0.0026991507  0.0986535382
#>        mem_35       mem_36       mem_37       mem_38       mem_39       mem_40
#> 1 -0.02277398  0.032366522 -0.095207273  0.001470171 0.0220490866 -0.020744827
#> 2 -0.02249876  0.003705815 -0.009986122  0.001834603 0.0072022025  0.012513154
#> 3 -0.02456633 -0.047683754 -0.030017972  0.194260820 0.0002023167  0.117669489
#> 4 -0.01207111 -0.002365715  0.003665707 -0.002490411 0.0516209027 -0.004805407
#> 5  0.02262826 -0.068423826  0.207826521  0.003515905 0.0673115372  0.034927798
#> 6 -0.02665171  0.001086085 -0.021607620  0.005222174 0.0807527077 -0.013496439
#>         mem_41       mem_42        mem_43       mem_44       mem_45
#> 1  0.063465665 -0.001583198  0.0008763528 -0.004924952  0.004441966
#> 2  0.006773117  0.005153518 -0.0061488312  0.005445949  0.004192209
#> 3  0.028793256  0.002015785  0.0145429165  0.054685966 -0.031594959
#> 4 -0.052984447 -0.011659562  0.0007896651  0.012327561 -0.125926338
#> 5 -0.171082675  0.006518095  0.0029789094  0.004475698 -0.086190522
#> 6 -0.019748363  0.002035536  0.0001205283 -0.003080538 -0.102438242
#>         mem_46       mem_47        mem_48       mem_49       mem_50
#> 1 -0.010614314  0.109509260 -0.0291230983 -0.014141207  0.065556939
#> 2 -0.008707069  0.004100545 -0.0083583731  0.004919289  0.002300612
#> 3  0.154873292  0.012040765 -0.0001406001 -0.020129602 -0.006858614
#> 4 -0.008602096 -0.251552263  0.1535405874  0.034783817 -0.001360919
#> 5 -0.026927424  0.054298969  0.0705021721  0.001021822 -0.206500175
#> 6 -0.027814286  0.034531636 -0.0040461329 -0.008599773 -0.018817582
#>         mem_51       mem_52       mem_53        mem_54       mem_55
#> 1  0.008376850 -0.270094108  0.016737186 -0.0008228533  0.015664149
#> 2 -0.051908297 -0.017095345 -0.030601816 -0.0067216761 -0.031129552
#> 3  0.007107829 -0.004057733 -0.033720564  0.0245429177 -0.211044319
#> 4 -0.012019909 -0.203816105  0.006645515  0.0057952190  0.016373143
#> 5 -0.033419293 -0.008175125 -0.005998713  0.0025015180 -0.003434644
#> 6  0.010105730 -0.113070059  0.004311746  0.0054377734  0.010990643
#>         mem_56      mem_57       mem_58       mem_59       mem_60       mem_61
#> 1 -0.029544538 -0.08246695  0.037100611  0.096731288  0.005897200  0.008479632
#> 2  0.054424622  0.02365053  0.044103974 -0.007519343  0.030608519 -0.098699533
#> 3 -0.115905532  0.01459723 -0.016563181  0.010981383  0.050403472 -0.007539959
#> 4 -0.030249584  0.03505309  0.002291122  0.068845992 -0.004341585  0.024917090
#> 5 -0.014103637 -0.01536707 -0.035271720 -0.122375412  0.002701816 -0.010265142
#> 6 -0.004441768  0.11325329 -0.043342941 -0.007111342 -0.005799743  0.029787167
#>        mem_62       mem_63       mem_64       mem_65      mem_66       mem_67
#> 1 -0.09335058 -0.077496495 -0.011023456  0.013749019 -0.00670314 -0.092711212
#> 2 -0.07575240  0.105276944  0.004368849 -0.026456375 -0.06720143  0.026432216
#> 3 -0.01065693  0.008150333 -0.042359390  0.036145409 -0.02388848 -0.002307292
#> 4  0.03804528  0.019850232  0.009653611  0.003252935  0.03549700  0.047805935
#> 5  0.07225392  0.040042615 -0.020165538 -0.041935828 -0.01486902  0.086121023
#> 6  0.02760241 -0.138702962 -0.057357348 -0.169761130 -0.23725453  0.012066368
#>        mem_68      mem_69      mem_70      mem_71      mem_72       mem_73
#> 1  0.04605361  0.14007779 -0.03865851  0.01149307  0.06794522 -0.035343901
#> 2 -0.04457336  0.05193081  0.03527919  0.04203337  0.01282620 -0.009111780
#> 3  0.05047973 -0.02176933  0.05057488 -0.02103948 -0.01111083 -0.009049244
#> 4  0.01809031  0.01189454 -0.09374814 -0.19466068  0.02195679  0.063050823
#> 5 -0.07767593 -0.12162162  0.06707658  0.05352161 -0.03062204 -0.047062419
#> 6 -0.08022491 -0.20666531  0.08652559  0.01833518  0.15627827  0.016779720
#>         mem_74      mem_75      mem_76       mem_77       mem_78      mem_79
#> 1 -0.017915068 0.015542313  0.03292822  0.069727084 -0.017757708 0.013920614
#> 2  0.005375863 0.017540288 -0.03399917 -0.005818549 -0.009577624 0.034844339
#> 3  0.036453627 0.233927322  0.04447370 -0.032276461 -0.003526079 0.062467634
#> 4  0.063122301 0.030319256 -0.09559430  0.063758722  0.064366459 0.087933528
#> 5 -0.031912283 0.023691678  0.08129817  0.182586145  0.077074235 0.049456016
#> 6  0.013204202 0.005625772 -0.08945159 -0.169983941  0.035904400 0.005426446
#>        mem_80       mem_81      mem_82       mem_83       mem_84      mem_85
#> 1 -0.00821663 -0.015901522  0.02664505  0.051483998 -0.059455466  0.09722934
#> 2  0.01212693 -0.003600061  0.02960288  0.018758921  0.013363577  0.01470533
#> 3  0.03735965 -0.087326353 -0.03510723 -0.009796530 -0.061048344  0.04897573
#> 4 -0.04193539 -0.041426923  0.08514307  0.054298933  0.005360485 -0.01867608
#> 5  0.18633301  0.023410926  0.04332644 -0.044720417 -0.006508432 -0.01168749
#> 6  0.05857672 -0.020355821  0.03165506 -0.006830768 -0.034532156 -0.01424540
#>         mem_86       mem_87
#> 1 -0.097284331  0.086610223
#> 2  0.003332324 -0.020797134
#> 3  0.034998512 -0.040119569
#> 4 -0.022669681  0.007774045
#> 5 -0.030586171 -0.119438460
#> 6 -0.044059874 -0.104983179
dim(mems)
#> [1] 227  87

# Check column names
colnames(mems)[1:5]
#> [1] "mem_1" "mem_2" "mem_3" "mem_4" "mem_5"
```
