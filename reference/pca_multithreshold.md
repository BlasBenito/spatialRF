# Compute Principal Component Analysis at multiple distance thresholds

Computes principal components of a distance matrix at multiple distance
thresholds to generate multi-scale spatial predictors for
[`rf_spatial()`](https://blasbenito.github.io/spatialRF/reference/rf_spatial.md).
Each distance threshold defines a different neighborhood scale, and PCA
is applied to the weighted distance matrix at each scale.

## Usage

``` r
pca_multithreshold(
  distance.matrix = NULL,
  distance.thresholds = NULL,
  max.spatial.predictors = NULL
)
```

## Arguments

- distance.matrix:

  Numeric distance matrix between observations.

- distance.thresholds:

  Numeric vector of distance thresholds defining different neighborhood
  scales. Each threshold specifies the maximum distance for spatial
  neighbors at that scale. If `NULL`, automatically computed with
  [`default_distance_thresholds()`](https://blasbenito.github.io/spatialRF/reference/default_distance_thresholds.md).
  Default: `NULL`.

- max.spatial.predictors:

  Integer specifying the maximum number of spatial predictors to retain.
  If the total number of generated predictors exceeds this value, only
  the first `max.spatial.predictors` are kept (ordered by variance
  explained). Useful for managing memory when `distance.matrix` is very
  large. Default: `NULL` (keeps all predictors).

## Value

Data frame where each column is a spatial predictor derived from PCA at
a specific distance threshold. Columns are named with the pattern
`spatial_predictor_<distance>_<number>` (e.g.,
"spatial_predictor_1000_1", "spatial_predictor_5000_2"), where
`<distance>` is the distance threshold and `<number>` is the principal
component rank. The number of rows matches the number of observations in
`distance.matrix`.

## Details

This function generates multi-scale spatial predictors by applying PCA
to distance matrices at different neighborhood scales. The process for
each distance threshold:

1.  Converts the distance matrix to weights using
    [`weights_from_distance_matrix()`](https://blasbenito.github.io/spatialRF/reference/weights_from_distance_matrix.md),
    where distances above the threshold are set to zero

2.  Applies
    [`pca()`](https://blasbenito.github.io/spatialRF/reference/pca.md)
    to the weighted distance matrix to extract principal components

3.  Names the resulting predictors with the distance threshold for
    identification

4.  Filters out predictors with all near-zero values

**Multi-scale spatial modeling:**

Different distance thresholds capture spatial patterns at different
scales. Combining predictors from multiple thresholds allows
[`rf_spatial()`](https://blasbenito.github.io/spatialRF/reference/rf_spatial.md)
to account for spatial autocorrelation operating at multiple spatial
scales simultaneously. This is analogous to
[`mem_multithreshold()`](https://blasbenito.github.io/spatialRF/reference/mem_multithreshold.md)
but uses PCA instead of Moran's Eigenvector Maps.

**Comparison with MEMs:**

Both `pca_multithreshold()` and
[`mem_multithreshold()`](https://blasbenito.github.io/spatialRF/reference/mem_multithreshold.md)
generate spatial predictors from distance matrices, but differ in their
approach:

- **PCA**: Captures the main patterns of variation in the weighted
  distance matrix without considering spatial autocorrelation structure

- **MEMs**: Explicitly extracts spatial patterns with specific
  autocorrelation scales (positive and negative eigenvalues)

In practice, MEMs are generally preferred for spatial modeling because
they explicitly target spatial autocorrelation patterns, but PCA can
serve as a simpler alternative or for comparison.

## See also

[`pca()`](https://blasbenito.github.io/spatialRF/reference/pca.md),
[`mem_multithreshold()`](https://blasbenito.github.io/spatialRF/reference/mem_multithreshold.md),
[`weights_from_distance_matrix()`](https://blasbenito.github.io/spatialRF/reference/weights_from_distance_matrix.md),
[`default_distance_thresholds()`](https://blasbenito.github.io/spatialRF/reference/default_distance_thresholds.md)

Other spatial_analysis:
[`filter_spatial_predictors()`](https://blasbenito.github.io/spatialRF/reference/filter_spatial_predictors.md),
[`mem()`](https://blasbenito.github.io/spatialRF/reference/mem.md),
[`mem_multithreshold()`](https://blasbenito.github.io/spatialRF/reference/mem_multithreshold.md),
[`moran()`](https://blasbenito.github.io/spatialRF/reference/moran.md),
[`moran_multithreshold()`](https://blasbenito.github.io/spatialRF/reference/moran_multithreshold.md),
[`pca()`](https://blasbenito.github.io/spatialRF/reference/pca.md),
[`rank_spatial_predictors()`](https://blasbenito.github.io/spatialRF/reference/rank_spatial_predictors.md),
[`residuals_diagnostics()`](https://blasbenito.github.io/spatialRF/reference/residuals_diagnostics.md),
[`residuals_test()`](https://blasbenito.github.io/spatialRF/reference/normality.md),
[`select_spatial_predictors_recursive()`](https://blasbenito.github.io/spatialRF/reference/select_spatial_predictors_recursive.md),
[`select_spatial_predictors_sequential()`](https://blasbenito.github.io/spatialRF/reference/select_spatial_predictors_sequential.md)

## Examples

``` r
data(plants_distance)

# Compute PCA spatial predictors at multiple distance thresholds
pca_predictors <- pca_multithreshold(
  distance.matrix = plants_distance,
  distance.thresholds = c(0, 1000, 5000)
)

# View structure
head(pca_predictors)
#>   spatial_predictor_0_1 spatial_predictor_0_2 spatial_predictor_0_3
#> 1             6.5412490             -3.510548             3.2726476
#> 2            -0.6442379              3.680523             0.8800348
#> 3            -2.0963747             -2.058756            -2.8333905
#> 4             3.0600023              3.868316            -3.1601127
#> 5             3.5180107             -3.639294             3.6897010
#> 6             4.9349470             -4.187669             4.2546510
#>   spatial_predictor_0_4 spatial_predictor_0_5 spatial_predictor_0_6
#> 1           -4.51128070              1.068166           -0.88542301
#> 2            1.66282148              2.839332           -0.44030971
#> 3           -0.09204521              0.277262           -0.80395050
#> 4            2.51521471              1.556041           -2.35626506
#> 5           -0.89126897              1.046012            0.07682843
#> 6           -2.49573319              2.127176           -1.94376161
#>   spatial_predictor_0_7 spatial_predictor_0_8 spatial_predictor_0_9
#> 1            1.84561719            -1.4224262             2.1024954
#> 2            2.52230804            -4.2008090             0.9878908
#> 3            3.15135982             2.4708270             1.8075146
#> 4           -2.08528002            -0.5695373             3.0910832
#> 5            0.08022523            -0.9501949             1.6051918
#> 6            0.36810097            -1.1541598             3.0250504
#>   spatial_predictor_0_10 spatial_predictor_0_11 spatial_predictor_0_12
#> 1             -2.2048047             0.08489393             0.01975034
#> 2              1.9964648             0.41150047            -0.40326283
#> 3              0.5119326            -3.33611741             2.30848073
#> 4             -2.8332050             0.48601331            -0.48975524
#> 5             -1.3100400            -0.22172355             0.16822949
#> 6             -3.3774689            -0.12268166             0.26556050
#>   spatial_predictor_0_13 spatial_predictor_0_14 spatial_predictor_0_15
#> 1             -1.3533907              2.4560740              0.3389781
#> 2              0.3342004              0.1659733              0.5215901
#> 3             -0.5687886             -0.5762337              0.7367172
#> 4             -5.0345464             -3.5809132              2.4208356
#> 5              0.2674839             -3.3495379             -4.5949774
#> 6              0.4568751              0.6207357             -2.8590760
#>   spatial_predictor_0_16 spatial_predictor_0_17 spatial_predictor_0_18
#> 1            -0.06815495             -0.8997021            0.739872152
#> 2             0.52833245              1.8295350           -0.004335192
#> 3             0.51457597              0.8601242           -2.434276457
#> 4            -1.70613335             -1.2896838           -0.134133436
#> 5             1.84692051              0.1200565            0.229918911
#> 6             1.35611527             -0.2188054            0.682714497
#>   spatial_predictor_0_19 spatial_predictor_0_20 spatial_predictor_0_21
#> 1             -2.8523943              0.2828850             -0.4636340
#> 2             -0.9050865              0.2727140             -0.1720308
#> 3             -1.0881179              1.5281069              0.2690473
#> 4              2.4748912             -0.6358428              0.3518217
#> 5             -1.5589589              0.2137847             -0.5626545
#> 6             -3.3466658              0.6093051             -0.6588284
#>   spatial_predictor_0_22 spatial_predictor_0_23 spatial_predictor_0_24
#> 1              0.2222313             -0.2554465             0.09397339
#> 2             -1.8650306             -0.5917842            -2.72442504
#> 3             -3.3060892              0.5696291             2.62979068
#> 4             -0.2129972              0.8371578            -0.36491462
#> 5              0.2516483              0.8731701            -0.12162376
#> 6              0.4843863             -0.6219552             0.14526435
#>   spatial_predictor_0_25 spatial_predictor_0_26 spatial_predictor_0_27
#> 1           -0.130979234             0.05101266             0.00861395
#> 2            2.756727942            -0.38423493            -0.75012754
#> 3            0.003966942            -0.06104927             0.09843048
#> 4            0.576947856            -0.25629515            -0.53274419
#> 5            0.150386299            -0.04890020            -0.08303901
#> 6           -0.115486632             0.02443337            -0.09884074
#>   spatial_predictor_0_28 spatial_predictor_0_29 spatial_predictor_0_30
#> 1            0.005067488             0.05527370            -0.08582029
#> 2           -0.360492138             0.25675163            -0.43695097
#> 3           -1.129329085            -0.12983193            -0.31319207
#> 4           -0.762870842            -0.28366138             0.35260013
#> 5           -0.108454115            -0.03294856             0.04374144
#> 6           -0.139468658             0.04450753            -0.08024368
#>   spatial_predictor_0_31 spatial_predictor_0_32 spatial_predictor_0_33
#> 1            -0.03100019              1.1533673             -0.9762868
#> 2            -0.04899327             -0.2871824              0.7424244
#> 3            -0.69384374             -1.2490613             -1.6358793
#> 4             0.58481588             -3.2190687              1.0744383
#> 5             0.09556637             -1.8805816              1.5581620
#> 6             0.06408568              0.3544990             -0.4103129
#>   spatial_predictor_0_34 spatial_predictor_0_35 spatial_predictor_0_36
#> 1             -0.4299029             0.31674669             0.58756909
#> 2             -0.2442279             1.08729391             3.58681041
#> 3              0.5862158             0.05280711             0.07515343
#> 4             -3.1288663             0.89803260             6.28354714
#> 5              0.4678411            -0.87516094             0.16986673
#> 6             -0.3081117             0.36613756             0.54015575
#>   spatial_predictor_0_37 spatial_predictor_0_38 spatial_predictor_0_39
#> 1             0.21932420             0.40024571            -0.06629377
#> 2             0.43332403             1.76299634            -0.25727879
#> 3             0.01472187            -0.12180972            -1.10938916
#> 4             0.61015294             3.22776178            -0.11391381
#> 5            -0.63708861            -0.01831418            -0.01238312
#> 6            -1.28367277            -0.19242684            -0.04825407
#>   spatial_predictor_0_40 spatial_predictor_0_41 spatial_predictor_0_42
#> 1             0.21847684            -0.32155620            0.182314141
#> 2            -0.29504724             1.44968170           -1.868794870
#> 3             0.04489114             0.60804632            1.791523680
#> 4            -0.05573750            -2.43080177            2.629013747
#> 5             0.19895583            -0.28328616            0.201157398
#> 6            -0.86645433            -0.02863626            0.003624615
#>   spatial_predictor_0_43 spatial_predictor_0_44 spatial_predictor_0_45
#> 1             -2.5472946             -0.3361105            -0.03104557
#> 2              0.4144840             -2.6585964             0.87439977
#> 3              0.1733572             -0.7215686             0.94635530
#> 4             -0.7132357              3.7394744            -1.26541107
#> 5             -4.3991151             -0.6491992             0.05489488
#> 6              5.3379990              1.2923590            -0.23604482
#>   spatial_predictor_0_46 spatial_predictor_0_47 spatial_predictor_0_48
#> 1           -0.039016613             0.17917437            -1.21854341
#> 2           -0.009224324            -0.33861579             0.36412970
#> 3           -0.881322806             0.20749843             0.08140173
#> 4            0.008082394             0.26978584             0.59074079
#> 5            0.005403228            -0.07200772             0.75681712
#> 6            0.104027120            -0.26443752             2.88801112
#>   spatial_predictor_0_49 spatial_predictor_0_50 spatial_predictor_0_51
#> 1            -0.07638802             0.81223213           -0.008997438
#> 2            -0.14307832            -0.40930794            0.224756502
#> 3             0.00259021             0.01940428           -2.314985369
#> 4             0.68379774            -1.93356417           -0.057779419
#> 5            -0.03278712             0.10030191            0.048374556
#> 6             0.12938349            -1.46321008            0.166633683
#>   spatial_predictor_0_52 spatial_predictor_0_53 spatial_predictor_0_54
#> 1             -0.8317627              0.3286473            -0.12564652
#> 2              0.5030475             -0.0162404             0.05521883
#> 3             -0.1499584             -0.2011643            -2.02472508
#> 4              1.2776850             -0.2534865            -0.02972839
#> 5             -0.4912464             -0.6240036            -0.02001981
#> 6             -2.3576081              0.2216785            -0.23757404
#>   spatial_predictor_0_55 spatial_predictor_0_56 spatial_predictor_0_57
#> 1          -0.0135413824             0.00597336             0.04821755
#> 2           0.3634123118            -0.42482005             0.05582970
#> 3          -0.3620900188             0.88246032            -0.04572994
#> 4           0.2611973533             0.53901757             0.28161145
#> 5          -0.0423998851            -0.05416775             0.24550510
#> 6          -0.0007909515             0.19883471            -0.81484763
#>   spatial_predictor_0_58 spatial_predictor_0_59 spatial_predictor_0_60
#> 1            -0.02925179            -0.05255721             -0.2332668
#> 2             0.29627749            -0.27758511             -0.4225243
#> 3             1.05686813             0.94917734             -1.5961045
#> 4             0.03900912             0.24293074              0.2696000
#> 5            -0.03226713             0.10215243              0.4146850
#> 6            -0.16158009             0.01034322              0.1194169
#>   spatial_predictor_0_61 spatial_predictor_0_62 spatial_predictor_0_63
#> 1             0.34641572            -0.50196624             0.10384423
#> 2            -0.34720885            -0.07098915            -1.19446357
#> 3            -1.07007569            -0.03546902            -0.28254000
#> 4             0.07977352            -0.40764789             0.16941644
#> 5            -0.54720581             0.96739301            -0.12803092
#> 6             0.02990081            -1.09630559             0.07350173
#>   spatial_predictor_0_64 spatial_predictor_0_65 spatial_predictor_0_66
#> 1           -0.001317515             0.08209079             0.01404503
#> 2            1.797769157            -0.41667182             1.74704525
#> 3            0.300706615            -0.14461448             0.91846667
#> 4           -0.066982977             0.29806580            -1.20790421
#> 5            0.419981071            -0.38897602            -0.31372961
#> 6           -0.028504796            -0.36018982             0.03107108
#>   spatial_predictor_0_67 spatial_predictor_0_68 spatial_predictor_0_69
#> 1           -0.378264183             0.11325992             0.04633522
#> 2            0.004757671             0.28373029             0.63214058
#> 3            0.125190377            -0.51138973            -2.21808358
#> 4            0.146725004            -0.37036656            -0.64578574
#> 5           -0.175716329             0.01034927            -0.11278915
#> 6           -0.163715813             0.05699004             0.07077231
#>   spatial_predictor_0_70 spatial_predictor_0_71 spatial_predictor_0_72
#> 1           0.9695589860            -0.80953593             0.04282694
#> 2          -0.0530101359             0.21090134             0.28539550
#> 3          -0.0383423690            -0.01767762            -0.81216213
#> 4          -0.8039922887            -0.22458594            -0.07479636
#> 5           2.2521824682            -2.17333165             0.30577330
#> 6          -0.0006726529            -0.97093148             0.09942688
#>   spatial_predictor_0_73 spatial_predictor_0_74 spatial_predictor_0_75
#> 1           -0.001802074           -0.007552061             0.22447529
#> 2           -0.040268864           -0.093675683            -0.20873690
#> 3           -2.369906082            1.921701412             0.04915314
#> 4            0.047973233            0.045836363            -0.16511735
#> 5           -0.046289716           -0.044021328             0.12272552
#> 6           -0.048178438           -0.029834849            -0.02822535
#>   spatial_predictor_0_76 spatial_predictor_0_77 spatial_predictor_0_78
#> 1            -1.02077217              2.3208263              1.1242936
#> 2             0.65865216             -0.7709016              1.2841613
#> 3            -1.07608458             -0.5892094              0.2555620
#> 4             0.63008320             -1.5080148             -1.2652271
#> 5             0.02710256             -0.1855806             -0.4498778
#> 6             0.32383728             -0.8098673             -0.7390905
#>   spatial_predictor_0_79 spatial_predictor_0_80 spatial_predictor_0_81
#> 1             0.10737705             -0.9874505            -0.60791681
#> 2            -0.04045656             -0.8116747             0.08616597
#> 3            -0.06109865             -0.2044590            -0.10637972
#> 4             0.26042542              0.1759545             0.61147068
#> 5             0.22807866             -0.5280622            -0.09390985
#> 6             0.16603348             -0.1168842             0.14361123
#>   spatial_predictor_0_82 spatial_predictor_0_83 spatial_predictor_0_84
#> 1             0.17092789             0.13869006             -0.1063306
#> 2             1.62480224            -0.60160144              0.9706033
#> 3            -0.77009060            -0.22783184              0.4642759
#> 4             0.14710259            -0.93582624              0.1928803
#> 5            -0.03081479            -2.45549712             -1.2241813
#> 6            -0.02441120            -0.08506963              0.1665503
#>   spatial_predictor_0_85 spatial_predictor_0_86 spatial_predictor_0_87
#> 1            -0.09695653             -1.9713563            -0.69414161
#> 2             0.02135721             -0.1414950             0.16774244
#> 3            -0.30968350              0.0987767            -0.47984236
#> 4             0.54700927             -1.7199470            -0.10324755
#> 5            -0.13159355              0.5873739             0.01653804
#> 6             0.18818987             -2.4229322            -0.61548776
#>   spatial_predictor_0_88 spatial_predictor_0_89 spatial_predictor_0_90
#> 1           -0.009476627             0.07620585           -0.008702348
#> 2            0.176235306            -0.05100758           -0.181017936
#> 3            0.312437960             0.09636436            0.538452346
#> 4           -0.050103210            -0.47960828            1.361815048
#> 5            0.030543846            -0.95962175            0.222196337
#> 6           -0.043154016            -0.45470555            0.287871619
#>   spatial_predictor_0_91 spatial_predictor_0_92 spatial_predictor_0_93
#> 1             -1.1223476              0.4278930              0.2556971
#> 2             -0.5710842              0.4362525              0.9224351
#> 3              0.1452251              0.3884577              0.3367617
#> 4             -2.6113074             -1.7211080              1.9363814
#> 5             -0.9101882              0.6452189             -1.0244447
#> 6             -0.3770167             -0.8454587              1.0850418
#>   spatial_predictor_0_94 spatial_predictor_0_95 spatial_predictor_0_96
#> 1              0.1929911              1.8587205             0.31775625
#> 2              0.1204529             -0.6712237            -0.03600974
#> 3             -0.1213169             -0.1849252             0.88741677
#> 4              0.3644504             -1.9290492            -0.69756367
#> 5             -0.2285070             -0.8167660            -0.14556945
#> 6              0.2334973              0.8737636             0.09632637
#>   spatial_predictor_0_97 spatial_predictor_0_98 spatial_predictor_0_99
#> 1            -0.01221190           -0.015497713             0.26137494
#> 2            -0.01082980            0.007841496            -0.17561496
#> 3            -0.03467083           -0.117480717            -0.59934079
#> 4             0.27905287            0.105670608            -0.62710203
#> 5            -0.13553361           -0.027664746            -0.23746046
#> 6            -0.10889002           -0.030272625             0.02138123
#>   spatial_predictor_0_100 spatial_predictor_0_101 spatial_predictor_0_102
#> 1               0.2063193            0.9409807389               0.8886756
#> 2              -0.3413617            0.0008267297               0.1131548
#> 3               0.4448935            0.0948203420               1.7960577
#> 4              -1.1760863            0.3172973898              -0.2322664
#> 5               0.9669516           -3.5821046538              -0.6639071
#> 6               0.5896586           -0.8652203446               0.1256962
#>   spatial_predictor_0_103 spatial_predictor_0_104 spatial_predictor_0_105
#> 1              -1.8020070               1.6133014             -0.79673008
#> 2              -0.1649059              -0.2750918             -0.31444501
#> 3               1.2765341               0.4064930              0.04266547
#> 4               0.4542619               0.4776875             -1.24942974
#> 5               0.9650513              -0.6919296              1.60137589
#> 6              -0.2898752               0.0196108              0.24438644
#>   spatial_predictor_0_106 spatial_predictor_0_107 spatial_predictor_0_108
#> 1              0.14814271            -0.438993137             2.080079538
#> 2             -0.41082672             0.199060378            -0.003021403
#> 3              0.15816165             0.856899659            -0.036555161
#> 4             -0.04053481            -0.008992876             0.309668446
#> 5              0.21175636            -0.036154028            -0.181619600
#> 6             -0.02636411             0.203254273            -2.906557299
#>   spatial_predictor_0_109 spatial_predictor_0_110 spatial_predictor_0_111
#> 1           -0.1455889284             -0.32455179            -2.170972400
#> 2           -0.1895525040             -0.11356512            -0.002251603
#> 3           -0.9702567745             -1.88338112             0.370325272
#> 4            0.0357688861             -0.19496119            -0.714082736
#> 5           -0.0001107778              0.06189216            -0.158348256
#> 6            0.2505408727              0.09943266            -0.152074689
#>   spatial_predictor_0_112 spatial_predictor_0_113 spatial_predictor_0_114
#> 1             0.313419493               1.7571156              0.06751098
#> 2             0.002779533               0.4170583             -0.31897982
#> 3             0.041588472               0.4772743              1.42524585
#> 4            -0.127281026              -0.5422528              0.04477499
#> 5             0.079118868               0.1754404             -0.01666571
#> 6            -0.003019568               0.3474119              0.03830108
#>   spatial_predictor_0_115 spatial_predictor_0_116 spatial_predictor_0_117
#> 1             -0.25901492             -0.14854622             -0.80391253
#> 2             -0.04230312              0.28772131              0.11585272
#> 3              1.81900171             -1.48478350              0.82157141
#> 4              0.03399012             -0.06106836             -0.15297483
#> 5              0.01950440              0.02961632              0.13529278
#> 6             -0.01992509             -0.04978953             -0.07744586
#>   spatial_predictor_0_118 spatial_predictor_0_119 spatial_predictor_0_120
#> 1               0.6949590             3.795242027             -0.10942421
#> 2               0.1718249             0.001273049             -0.41432484
#> 3              -0.1279618             0.120059795             -0.40084307
#> 4               0.8837286             0.700150045             -0.16868967
#> 5              -0.6422129            -0.434978550              0.13789042
#> 6               0.4134145             0.063936578             -0.03761962
#>   spatial_predictor_0_121 spatial_predictor_0_122 spatial_predictor_0_123
#> 1            -0.015851855           -0.7318169580           -1.219145e-05
#> 2            -0.057694032           -0.1598128546            4.749882e-03
#> 3            -0.228922041            0.0008325318            1.950401e-02
#> 4            -0.003138167            0.0249328213            1.650181e-03
#> 5            -0.248086042           -1.5991171388            9.965476e-04
#> 6             0.076441685            0.0552619448           -9.081125e-04
#>   spatial_predictor_0_124 spatial_predictor_0_125 spatial_predictor_0_126
#> 1            9.392666e-05               0.1379210              0.08091534
#> 2            7.923005e-04              -0.3638162             -0.53156238
#> 3            4.104094e-03              -0.2182211             -0.29290519
#> 4            4.332144e-04              -0.6825361             -1.30471924
#> 5            4.729383e-04              -0.4111982             -0.82616192
#> 6           -6.891470e-05               0.5598889              0.93350460
#>   spatial_predictor_0_127 spatial_predictor_0_128 spatial_predictor_0_129
#> 1             -0.02679597              -0.6875499              -0.1286243
#> 2              0.03278254               0.3804117              -0.3114318
#> 3              2.20570183              -0.3853342               1.6013066
#> 4             -0.36698090               0.1966626               0.4318463
#> 5             -0.30337554              -1.9165297              -0.1679517
#> 6              0.29900046               0.6351557              -0.3222373
#>   spatial_predictor_0_130 spatial_predictor_0_131 spatial_predictor_0_132
#> 1               0.2122063             -0.02494497               1.2479316
#> 2              -0.2602449              0.02526810              -0.1773632
#> 3               0.2677385             -0.29305004              -0.0157087
#> 4              -1.2072085             -0.00770374               0.4391586
#> 5              -0.4902661              0.04242279               0.4432343
#> 6               1.5215418             -0.03802736              -1.9700640
#>   spatial_predictor_0_133 spatial_predictor_0_134 spatial_predictor_0_135
#> 1             -0.08835137              0.03369634              0.05751413
#> 2             -0.09811178              0.02608533              0.01173780
#> 3              0.19944878              0.63367485              0.44201353
#> 4             -0.09666627              0.08942716              0.04923991
#> 5             -0.05756931              0.02920401              0.03707083
#> 6              0.19309275             -0.00327431              0.07950854
#>   spatial_predictor_0_136 spatial_predictor_0_137 spatial_predictor_0_138
#> 1              1.31688831             -0.04570347             -0.01487333
#> 2             -0.33383255              0.05353476             -0.06788822
#> 3             -0.09189543             -0.46354224             -0.03462277
#> 4              0.51690373             -0.02870772              0.53993201
#> 5              1.41690894             -0.05688716             -0.34088361
#> 6             -0.59952809              0.06041714              0.77568907
#>   spatial_predictor_0_139 spatial_predictor_0_140 spatial_predictor_0_141
#> 1              0.77707822              0.26106898             -0.11949171
#> 2              0.36190892             -0.49712301              0.01775014
#> 3              0.07356210              0.07261467             -0.06567281
#> 4             -0.44403444             -0.10604717             -0.14646417
#> 5              0.41412708             -0.01138469             -1.34847179
#> 6              0.08972453              0.05262793              0.26256469
#>   spatial_predictor_0_142 spatial_predictor_0_143 spatial_predictor_0_144
#> 1             -0.01390711             -0.03935948              0.03267434
#> 2              0.33160248             -0.34039295             -1.26353610
#> 3              0.95943972             -1.19104380             -0.81311216
#> 4             -0.01066130              0.11214260              0.12850773
#> 5              0.02812667              0.02583393             -0.05278836
#> 6             -0.09666013             -0.09959828              0.17692994
#>   spatial_predictor_0_145 spatial_predictor_0_146 spatial_predictor_0_147
#> 1              0.39312402             1.056153790             0.521476619
#> 2              0.49365212             0.008607085             0.910896110
#> 3             -0.09709201             0.421028503            -0.974466411
#> 4             -1.06031060             0.578472092             0.059157846
#> 5              0.15513963            -0.460488710            -0.067150020
#> 6              1.71347738             0.742956381            -0.006642167
#>   spatial_predictor_0_148 spatial_predictor_0_149 spatial_predictor_0_150
#> 1            -1.139910825             -0.03734818             0.130080202
#> 2            -0.064984625             -0.29371245            -0.550204553
#> 3             0.048374795              0.04285457             0.798594570
#> 4            -0.007419167              0.22236026             0.079628182
#> 5            -0.077426797             -0.11633722            -0.007362213
#> 6             0.271442224              1.09290057            -0.082581422
#>   spatial_predictor_0_151 spatial_predictor_0_152 spatial_predictor_0_153
#> 1             0.004475877             0.017083941           -0.0176888283
#> 2            -0.034175346            -0.268358263           -0.0007587757
#> 3             0.036139637             0.552932859            1.5297276711
#> 4             0.012737367             0.107592760            0.0473336207
#> 5            -0.000486669            -0.008001979            0.0043054101
#> 6             0.008718341            -0.006859863            0.0016279524
#>   spatial_predictor_0_154 spatial_predictor_0_155 spatial_predictor_0_156
#> 1             -0.08457569              0.04376452               0.4774618
#> 2             -0.15456396              0.40447137               0.2605849
#> 3             -1.58097259              0.45888994              -0.3210489
#> 4              0.19015059             -0.23142549              -0.4318417
#> 5              0.01440067              0.01317765              -0.3789342
#> 6              0.05924714             -0.02508289               0.1680469
#>   spatial_predictor_0_157 spatial_predictor_0_158 spatial_predictor_0_159
#> 1              0.00497745             -0.15253378             -0.16847715
#> 2              0.30510432             -0.21351530             -0.37575449
#> 3             -0.40012610              0.14418659              0.44887111
#> 4             -0.10606825              0.34334561              0.50223395
#> 5              0.04128911             -0.02739118             -0.05142445
#> 6             -0.11244806              0.20494018              0.25994468
#>   spatial_predictor_0_160 spatial_predictor_0_161 spatial_predictor_0_162
#> 1             -0.44285594             0.017356853               0.1047585
#> 2             -0.10660133            -0.160739927              -0.6296241
#> 3             -0.24469764            -0.006514458               0.1092854
#> 4              0.84167516             0.196421214               0.2719738
#> 5              0.14965601             0.016073552               0.1392805
#> 6              0.08803845             0.194545679               0.6697601
#>   spatial_predictor_0_163 spatial_predictor_0_164 spatial_predictor_0_165
#> 1              0.09806388               0.1954017             0.014420732
#> 2             -0.33512035               0.8617900             0.346973203
#> 3             -0.15993912               0.2774425            -0.072634176
#> 4              0.11986935              -0.6216684            -0.345241633
#> 5              0.56919597               0.2101909             0.050217908
#> 6              0.47015445               0.1047632             0.009081827
#>   spatial_predictor_0_166 spatial_predictor_0_167 spatial_predictor_0_168
#> 1              -0.2354890             -0.15595746              0.33718614
#> 2               0.3004421              0.49546250             -0.05961929
#> 3               0.1613970             -0.07655047             -0.02901090
#> 4              -0.1273403             -0.80542822              0.02991288
#> 5               0.3703223              0.25021517             -0.76345852
#> 6              -0.4266210             -0.50177679              0.75537654
#>   spatial_predictor_0_169 spatial_predictor_0_170 spatial_predictor_0_171
#> 1             -0.01536964              0.36559648              0.03766481
#> 2              0.35694820             -0.33461727              1.02577632
#> 3             -0.03507249             -0.07779203              0.16399883
#> 4             -0.09442874              0.12079593             -0.48048865
#> 5              0.39987156             -1.09912030              0.63185719
#> 6             -0.07734427              0.46802951              0.13654523
#>   spatial_predictor_0_172 spatial_predictor_0_173 spatial_predictor_0_174
#> 1               0.1253257              -0.1610113             -0.06814937
#> 2              -0.1954604               0.3583458             -0.06444435
#> 3              -0.3563444              -0.5859440              0.02089588
#> 4               0.1584751              -0.2696555             -0.19126037
#> 5               0.1895644               0.3367973              0.23207085
#> 6               0.5799790              -0.2940290             -0.15605911
#>   spatial_predictor_0_175 spatial_predictor_0_176 spatial_predictor_0_177
#> 1               0.1027935              0.03116451             -0.05639859
#> 2               0.3718533              0.05299426              0.34017149
#> 3              -0.1716748              0.25926366             -0.02925540
#> 4              -0.2072883             -0.11128367             -0.07403952
#> 5              -0.1571659              0.00928933              0.10997754
#> 6              -0.4253851             -0.16148337              0.02846830
#>   spatial_predictor_0_178 spatial_predictor_0_179 spatial_predictor_0_180
#> 1              0.10683391              0.11072753             0.218196428
#> 2             -0.22672494             -0.54557556             0.282150057
#> 3             -0.32810255              0.20006420            -0.009171285
#> 4             -0.06689952             -0.23707127             0.224660686
#> 5             -0.11844507              0.04201469             0.542482359
#> 6             -0.24547269             -0.43533255             0.084640692
#>   spatial_predictor_0_181 spatial_predictor_0_182 spatial_predictor_0_183
#> 1              0.38683452             -0.02690507              0.18989270
#> 2              0.53480794             -0.46533412             -0.80312234
#> 3              0.01730616             -0.12627747             -0.06337055
#> 4              0.29966584             -0.10822004              0.09591721
#> 5             -0.86924106              0.11821603              0.03112643
#> 6             -0.59700989             -0.04642413             -0.37504780
#>   spatial_predictor_0_184 spatial_predictor_0_185 spatial_predictor_0_186
#> 1             -0.02894385             0.002034152             -0.11131446
#> 2              0.20350464            -0.050282536              0.06290212
#> 3              0.27302267            -0.112717379             -0.06317730
#> 4             -0.01313006             0.084435605             -0.07609181
#> 5             -0.04294119            -0.162278480              0.25105954
#> 6             -0.05039647            -0.581396329              0.59448364
#>   spatial_predictor_0_187 spatial_predictor_0_188 spatial_predictor_0_189
#> 1             0.720885733              -0.2842690             -0.16123765
#> 2            -0.382501014              -0.9521776             -0.13285551
#> 3             0.127598604               0.3391291             -0.05219062
#> 4             0.008563033               0.1031825              0.03766582
#> 5            -0.212142918               0.2726150             -0.05243145
#> 6            -0.620324347               0.1290930              0.07613207
#>   spatial_predictor_0_190 spatial_predictor_0_191 spatial_predictor_0_192
#> 1             0.353124505              0.17786926             -0.24666777
#> 2            -0.069383649             -0.38888284              0.40932004
#> 3             0.059676046             -0.19320854             -0.15049090
#> 4            -0.009832936              0.14104610             -0.10473591
#> 5             0.492808597              0.06710036              0.04012085
#> 6            -0.501761350             -0.15269921              0.02041464
#>   spatial_predictor_0_193 spatial_predictor_0_194 spatial_predictor_0_195
#> 1             -0.71823405             -0.01538441              0.03259211
#> 2             -0.18560700             -0.63476662              0.44417248
#> 3              0.04644974             -0.36518632              0.20265358
#> 4             -0.15550468              0.04449836             -0.09329388
#> 5              0.55857966              0.22516670             -0.18054588
#> 6             -0.14050336             -0.08748691              0.09994347
#>   spatial_predictor_0_196 spatial_predictor_0_197 spatial_predictor_0_198
#> 1              0.11308084            -0.340089500             -0.02003248
#> 2             -0.20543590            -0.194588865             -0.09051743
#> 3              0.05298784            -0.009727495              0.56555526
#> 4              0.03984539             0.032579353              0.04093300
#> 5              0.19625460            -0.208083468              0.04910342
#> 6             -0.15492481             0.280215466             -0.01245692
#>   spatial_predictor_0_199 spatial_predictor_0_200 spatial_predictor_0_201
#> 1            -0.111899010            -0.008220651              0.06864253
#> 2            -0.243088034             0.044523430              0.17265031
#> 3            -0.003210506             0.321777808              0.15045429
#> 4             0.335994361            -0.001657612              0.11882487
#> 5            -0.291749041            -0.049570488              0.16504150
#> 6             0.039776443             0.005082143              0.18774640
#>   spatial_predictor_0_202 spatial_predictor_0_203 spatial_predictor_0_204
#> 1              0.19762948              -0.2759177            -0.007008019
#> 2              0.09355998              -0.1483305             0.206183668
#> 3             -0.11299661              -0.0260271            -0.106827812
#> 4              0.01023782               0.5510294            -0.137734449
#> 5              0.03203948              -0.2242766             0.109869063
#> 6              0.37592718               0.1286010            -0.024031409
#>   spatial_predictor_0_205 spatial_predictor_0_206 spatial_predictor_0_207
#> 1              0.21323148            0.0703048491             -0.23796376
#> 2             -0.07786454            0.0209804716              0.22248559
#> 3             -0.01094558            0.0006048346             -0.01812314
#> 4             -0.05654874           -0.0438910645             -0.06199745
#> 5             -0.21641853            0.0265477498             -0.09424567
#> 6             -0.36423118            0.0857711974             -0.17767037
#>   spatial_predictor_0_208 spatial_predictor_0_209 spatial_predictor_0_210
#> 1             -0.03206617             0.008716490             -0.12655553
#> 2             -0.63594563            -0.077560921             -0.01870984
#> 3              0.03548244            -0.068084263             -0.01114850
#> 4             -0.13426802            -0.009008687              0.16961427
#> 5              0.15069726             0.002294747              0.42795326
#> 6              0.06118117             0.024565418             -0.01415947
#>   spatial_predictor_0_211 spatial_predictor_0_212 spatial_predictor_0_213
#> 1              0.02056728              0.04258712             -0.03570822
#> 2             -0.11017514             -0.15182483              0.04551669
#> 3              0.08551551             -0.02339698              0.12085762
#> 4              0.03703951              0.11785357              0.01750318
#> 5              0.05022557              0.12736044              0.02169161
#> 6              0.01420911             -0.15531528             -0.04014103
#>   spatial_predictor_0_214 spatial_predictor_0_215 spatial_predictor_0_216
#> 1            -0.051609578            -0.006568166             0.015870657
#> 2             0.044777028            -0.003493034            -0.035405878
#> 3            -0.098963854             0.298650794             0.013803668
#> 4            -0.028505597            -0.024012477             0.003276386
#> 5            -0.050056316            -0.025863815            -0.014798518
#> 6            -0.006764217            -0.002496677             0.037716598
#>   spatial_predictor_0_217 spatial_predictor_0_218 spatial_predictor_0_219
#> 1             0.093150343             0.009549805             0.048097265
#> 2             0.010594734             0.095426810             0.018012159
#> 3            -0.006237964            -0.021986421             0.066526736
#> 4             0.104389679             0.054447732             0.007562064
#> 5             0.057345346             0.074247146             0.025417330
#> 6             0.100711223            -0.019844393             0.025542096
#>   spatial_predictor_0_220 spatial_predictor_0_221 spatial_predictor_0_222
#> 1            -0.159757946            -0.001915497             0.047028880
#> 2            -0.041733230            -0.020259093             0.030009087
#> 3             0.006940596            -0.060738871             0.002720058
#> 4            -0.010903937            -0.009752419             0.029812464
#> 5            -0.079135965            -0.011880321            -0.010864066
#> 6            -0.089822277            -0.001655769            -0.079876709
#>   spatial_predictor_0_223 spatial_predictor_0_224 spatial_predictor_0_225
#> 1           -0.0156513394              0.05736403             0.011401407
#> 2            0.0213792395             -0.05082478            -0.024306350
#> 3           -0.0189027421             -0.01160332            -0.001262984
#> 4            0.0148438740             -0.04937007             0.006354207
#> 5            0.0095924835             -0.00882050            -0.030817884
#> 6           -0.0001438305              0.01418481            -0.017935342
#>   spatial_predictor_0_226 spatial_predictor_1000_1 spatial_predictor_1000_2
#> 1            0.0024923655                 9.077380               -0.9086304
#> 2           -0.0057327055                 2.542267                7.1430754
#> 3            0.0033561176                -6.074998               -6.0614633
#> 4           -0.0025942269                 9.449762                1.7916180
#> 5           -0.0031507527                 6.285115               -4.3297628
#> 6           -0.0001034069                 7.087413               -4.1308619
#>   spatial_predictor_1000_3 spatial_predictor_1000_4 spatial_predictor_1000_5
#> 1                 2.743508              -1.10277061                 3.203835
#> 2                 1.487433              -0.07194661                -1.645741
#> 3                 5.705296              -1.53773707                -1.992657
#> 4                 4.662155              -2.34817255                 2.303648
#> 5                -2.771713               3.40355567                 1.643642
#> 6                -1.958878               2.88868913                 2.321629
#>   spatial_predictor_1000_6 spatial_predictor_1000_7 spatial_predictor_1000_8
#> 1               -0.5613846                3.1262672               -0.4587673
#> 2                0.4264853               -0.6623700                1.3167399
#> 3                0.2832562               -2.0965849               -1.6281094
#> 4                1.5826000                0.7392115                0.2187453
#> 5                0.3733529                3.9401952               -3.2433727
#> 6                1.4442219                5.4314970               -4.3961920
#>   spatial_predictor_1000_9 spatial_predictor_1000_10 spatial_predictor_1000_11
#> 1               -0.2859252                -1.3165970                 0.5248074
#> 2                0.3042120                 0.8824864                 0.7916223
#> 3                3.5736466                -0.8511592                 1.9788836
#> 4               -0.3679947                 2.5292223                 1.1451495
#> 5                1.5595919                -3.1428199                -1.1338415
#> 6                1.9674551                -2.5239433                -1.2830117
#>   spatial_predictor_1000_12 spatial_predictor_1000_13 spatial_predictor_1000_14
#> 1               -0.57270252                 0.7021078                 0.9273710
#> 2               -0.01565485                 0.4258524                 1.1738109
#> 3               -0.68332656                 0.9890611                 1.1533828
#> 4               -1.78096096                 0.1810768                -0.9279167
#> 5                1.62646856                 0.1535510                -1.2545830
#> 6                1.36447892                -0.2997104                -1.2429061
#>   spatial_predictor_1000_15 spatial_predictor_1000_16 spatial_predictor_1000_17
#> 1                -1.0135562                 0.8297926                 0.1829125
#> 2                -0.3416350                 0.8934077                -0.4251121
#> 3                 0.6896154                 0.8550549                 0.5843015
#> 4                -0.6291021                -1.3197483                -0.5734209
#> 5                 0.2866956                 0.2701295                -0.4013594
#> 6                 0.7135930                -0.1027657                 0.1734998
#>   spatial_predictor_1000_18 spatial_predictor_1000_19 spatial_predictor_1000_20
#> 1                0.06183416                1.66294530                -1.0091865
#> 2               -0.67171502                0.03923729                -0.6065340
#> 3                0.27352922               -0.52058879                -0.1535620
#> 4               -1.48295594               -0.37967337                 0.9355907
#> 5                0.08799507               -0.32741456                -0.1683189
#> 6               -0.21368945               -0.59514890                -0.1269941
#>   spatial_predictor_1000_21 spatial_predictor_1000_22 spatial_predictor_1000_23
#> 1                 0.8996725              -0.910337760                0.35112984
#> 2                 1.1071164              -0.129782596               -0.39435312
#> 3                -1.6976610              -2.159169516               -0.59208712
#> 4                -3.0826904              -0.001171461               -0.03812193
#> 5                -0.9350109               1.039627719               -0.32724802
#> 6                -0.3152927               0.608356823               -0.27926834
#>   spatial_predictor_1000_24 spatial_predictor_1000_25 spatial_predictor_1000_26
#> 1                -0.1916026                 1.2759083               -0.23125788
#> 2                 0.3569665                 0.1729746                0.21486749
#> 3                 1.1235278                -1.5202554                1.52774677
#> 4                -0.3859549                 1.0762847               -0.98178414
#> 5                 0.1257106                -0.3198743                0.02231814
#> 6                 0.2697958                -1.1458635               -0.47522107
#>   spatial_predictor_1000_27 spatial_predictor_1000_28 spatial_predictor_1000_29
#> 1                 0.8510175               -0.84016926                 0.3685766
#> 2                -0.1942457                0.18750731                 0.5008872
#> 3                 0.6026364               -0.36676144                 0.2243654
#> 4                 1.1928947               -0.80355425                 0.2660253
#> 5                -1.2237320                0.85953136                 0.1423688
#> 6                 0.1867784               -0.04661881                -0.3659995
#>   spatial_predictor_1000_30 spatial_predictor_1000_31 spatial_predictor_1000_32
#> 1               -0.05184294                 0.7306511                 0.1084356
#> 2                0.18467546                 0.3949255                 0.5040535
#> 3                0.26872749                 0.8917106                 0.6116291
#> 4                0.22949424                -0.2656732                -0.2235287
#> 5                0.15028134                 0.6946815                -0.9676075
#> 6               -1.27731634                 1.1045308                -0.9145541
#>   spatial_predictor_1000_33 spatial_predictor_1000_34 spatial_predictor_1000_35
#> 1              0.0874635201                 1.1125507               -0.50954638
#> 2             -0.4541024304                 0.3585128                0.69320675
#> 3             -0.4217175019                 0.4073970                0.22747473
#> 4             -0.5969069595                -1.3377958                0.81252489
#> 5              0.0007443121                 0.5101423               -0.51091165
#> 6             -0.6346426603                -1.7254594               -0.02303392
#>   spatial_predictor_1000_36 spatial_predictor_1000_37 spatial_predictor_1000_38
#> 1                0.29322512               -0.48958628                -0.1194507
#> 2                0.57453948               -0.98887985                 0.1094166
#> 3               -0.05311258                0.16355027                 0.2482454
#> 4                0.04380853                0.01562752                -0.5024761
#> 5                1.58029381                1.47041420                -0.1374303
#> 6                1.77451447               -0.52206359                -0.7939217
#>   spatial_predictor_1000_39 spatial_predictor_1000_40 spatial_predictor_1000_41
#> 1                0.02465228                 0.1515012               -0.09398139
#> 2                0.20322124                 0.2285068                0.13735810
#> 3                0.65304582                -0.9116016                0.03670080
#> 4               -0.03739846                -0.1447509               -0.25194406
#> 5                0.03279993                 0.1660092               -0.05367003
#> 6               -0.11881942                 0.9773003                1.48611054
#>   spatial_predictor_1000_42 spatial_predictor_1000_43 spatial_predictor_1000_44
#> 1               -0.39397237                -0.6632581                -0.5606623
#> 2                0.66758538                -0.3883406                 0.4362763
#> 3                0.64410890                 0.2114401                -0.2011147
#> 4               -0.41452640                -0.5315298                 0.1487060
#> 5                0.02589065                -1.1165147                -1.2458665
#> 6                0.10705581                 0.2695373                -1.5707261
#>   spatial_predictor_1000_45 spatial_predictor_1000_46 spatial_predictor_1000_47
#> 1                 0.9861766               -0.61966588               -0.65999156
#> 2                -0.2878491               -0.61202012                0.52409596
#> 3                -0.3653370               -2.07895521                0.26389959
#> 4                 0.8319100               -0.30248754               -0.44520098
#> 5                 0.4706174               -0.07813427               -0.18653024
#> 6                 1.0861572                0.15666468               -0.05975561
#>   spatial_predictor_1000_48 spatial_predictor_1000_49 spatial_predictor_1000_50
#> 1                 0.6437432               -0.02612060               -0.04607011
#> 2                 0.1169782                1.01183794               -0.29641461
#> 3                -0.4224252                0.25741200                0.04927037
#> 4                 0.2112857                0.05037800               -0.61771131
#> 5                -0.7918804               -0.01097789               -0.72737754
#> 6                -1.5110080                0.30195196               -0.64192445
#>   spatial_predictor_1000_51 spatial_predictor_1000_52 spatial_predictor_1000_53
#> 1               -0.48265591                 0.1005205               -0.30565544
#> 2                0.19927025                -0.2238584               -0.62243410
#> 3               -0.39237563                 0.1211808                0.65599475
#> 4               -0.90354142                -0.5581592               -0.04051531
#> 5               -0.04677935                -0.2443140               -0.04126133
#> 6                0.38120860                -0.7871363                0.88155003
#>   spatial_predictor_1000_54 spatial_predictor_1000_55 spatial_predictor_1000_56
#> 1               0.006615201              -0.671530382                0.37128286
#> 2              -0.051774083              -0.130322700               -0.31386792
#> 3               0.230834366               0.009455472               -0.32442540
#> 4               0.322143294              -0.447871074               -0.02033659
#> 5               0.985519686              -0.528410497               -0.22957593
#> 6               0.058711306               0.053414873                0.63868496
#>   spatial_predictor_1000_57 spatial_predictor_1000_58 spatial_predictor_1000_59
#> 1               -0.17712363                0.56176643              -0.543796334
#> 2                0.06947338                0.21001746              -0.006818544
#> 3                0.05982259               -0.16815193               0.823174716
#> 4                0.09478758                0.02207308               0.359961227
#> 5                0.15623902               -1.20914364              -0.083232702
#> 6               -0.15103142               -0.26336281              -0.561780338
#>   spatial_predictor_1000_60 spatial_predictor_1000_61 spatial_predictor_1000_62
#> 1                 0.4200336                0.03315567               -0.66749046
#> 2                -0.2755754               -0.08895711               -0.02934840
#> 3                -0.2538394               -0.05881123               -0.65589109
#> 4                 0.1702653               -0.09837144               -0.29645621
#> 5                -0.9276037               -0.42155258                0.01320445
#> 6                 0.5284208               -0.58507446               -0.40519371
#>   spatial_predictor_1000_63 spatial_predictor_1000_64 spatial_predictor_1000_65
#> 1               0.387352611               -0.34188278                -0.8953244
#> 2              -0.001484311               -0.04054244                -0.5359462
#> 3               0.530809406                0.81460995                -0.4854731
#> 4              -0.064140051               -0.42738832                 0.1686890
#> 5               0.213242271               -0.55509119                 0.2981182
#> 6               1.059441067               -0.35635394                -0.3738498
#>   spatial_predictor_1000_66 spatial_predictor_1000_67 spatial_predictor_1000_68
#> 1                0.23352932               -0.27487511                0.61795615
#> 2                0.81525403               -0.05101507                0.34821761
#> 3                0.42491959               -0.94152703               -0.23657441
#> 4               -0.15708463               -0.32969190               -0.48535778
#> 5               -0.09972884                0.35433685                0.46496581
#> 6               -0.05349179                0.20935125                0.03903695
#>   spatial_predictor_1000_69 spatial_predictor_1000_70 spatial_predictor_1000_71
#> 1                 0.0780414                 0.1565868               -0.65214114
#> 2                -0.8856848                -0.3877796               -0.60408232
#> 3                 0.4843920                 0.8701678               -0.03313128
#> 4                 0.4102190                -0.2905093                0.45856107
#> 5                 0.2093664                 0.1817500               -0.65506179
#> 6                -0.3227077                -0.3371068                0.17869729
#>   spatial_predictor_1000_72 spatial_predictor_1000_73 spatial_predictor_1000_74
#> 1               -0.70598551                 0.5623477               -0.40280554
#> 2               -0.07970040                 0.7386908                0.75906081
#> 3               -0.44419323                -0.2988710               -0.32785463
#> 4               -0.02409202                -0.2548969               -0.33939930
#> 5               -0.51652380                 0.4340845               -0.02134449
#> 6               -0.95238775                 0.7235571                0.06218091
#>   spatial_predictor_1000_75 spatial_predictor_1000_76 spatial_predictor_1000_77
#> 1                0.48995894               -0.25958373               -0.46537813
#> 2                0.05245159                0.07963629                0.33601318
#> 3               -1.13070549                0.04422716                0.35001059
#> 4                0.10021688               -0.09278107               -0.44717549
#> 5               -0.08681766                0.16010510               -0.01992145
#> 6               -0.17046557                0.11405282                0.20987049
#>   spatial_predictor_1000_78 spatial_predictor_1000_79 spatial_predictor_1000_80
#> 1                0.27241884              0.3156705918                0.51674888
#> 2                0.13403376              0.7668389757               -0.07454272
#> 3               -0.26878292              0.1784822046               -0.18611807
#> 4               -0.03064687             -0.0333920981               -0.11277596
#> 5                0.37687165              0.1275326844               -0.07915582
#> 6                0.10282150             -0.0004774392               -0.50353972
#>   spatial_predictor_1000_81 spatial_predictor_1000_82 spatial_predictor_1000_83
#> 1                -0.5493968               -0.26153608                0.06915233
#> 2                -0.1122327                0.36612789               -0.60461384
#> 3                -0.5449908               -0.05853404                0.79638174
#> 4                -0.1552042                0.11052852                0.01690108
#> 5                 0.1585707               -0.03155931                0.21717685
#> 6                 0.2043007                0.21188188                0.23005137
#>   spatial_predictor_1000_84 spatial_predictor_1000_85 spatial_predictor_1000_86
#> 1                -0.3786726                 0.2317167                0.13228153
#> 2                -0.2135785                 0.2616585               -0.04086103
#> 3                 0.6124494                 0.4733756                0.30505196
#> 4                -0.2281051                -0.2459487               -0.20374544
#> 5                 0.2600211                -0.1605217               -0.26438064
#> 6                 0.5094054                -0.8226678               -0.45847853
#>   spatial_predictor_1000_87 spatial_predictor_1000_88 spatial_predictor_1000_89
#> 1                0.07425695                0.13939564               -0.09554662
#> 2                0.14678052               -0.19318932                0.32611976
#> 3                0.16120825               -0.74424759               -0.89245729
#> 4               -0.12900389               -0.18262828               -0.21453341
#> 5               -0.35194476                0.09937847                0.08819670
#> 6                0.33068914               -0.09671785                0.20534522
#>   spatial_predictor_1000_90 spatial_predictor_1000_91 spatial_predictor_1000_92
#> 1             -0.0008009093               -0.08513828                0.40150653
#> 2              0.0029935626                0.02473128               -0.03135561
#> 3             -0.0073305478                0.21882289               -0.37700521
#> 4             -0.0016966036               -0.14063421               -0.08158892
#> 5              0.0006352338               -0.13701676               -0.06910805
#> 6              0.0020399924                0.52353016               -0.14803147
#>   spatial_predictor_1000_93 spatial_predictor_1000_94 spatial_predictor_1000_95
#> 1               -0.42897436                 0.2489256               -0.19447406
#> 2               -0.38990233                -0.6766898                0.04541815
#> 3                0.44197270                -0.3981882               -0.37168202
#> 4               -0.52756002                -0.2692727               -0.34296200
#> 5               -0.02137147                -0.1479849                0.04936748
#> 6                0.07532458                -0.1356911                0.58905840
#>   spatial_predictor_1000_96 spatial_predictor_1000_97 spatial_predictor_1000_98
#> 1                 0.1556646                -0.1670358               -0.13208906
#> 2                 0.3935420                 0.4002215               -0.17955627
#> 3                 0.2143100                -0.2883027                0.68080370
#> 4                -0.5812603                 0.6262749                0.22732176
#> 5                 0.2006497                 0.1613373               -0.05872663
#> 6                -0.5759108                 0.3220476                0.77475114
#>   spatial_predictor_1000_99 spatial_predictor_1000_100
#> 1              -0.044041860                 0.06633904
#> 2              -0.005528929                 0.21347681
#> 3               0.101228308                 0.22893700
#> 4              -0.352370174                -0.09259244
#> 5               0.207376388                -0.07264746
#> 6              -0.262286961                -0.09633020
#>   spatial_predictor_1000_101 spatial_predictor_1000_102
#> 1                -0.45368932                 -0.2760360
#> 2                -0.02330021                  0.1477068
#> 3                -0.03790018                 -0.7101680
#> 4                -0.19680386                  0.2707485
#> 5                -0.15162566                  0.4053003
#> 6                -0.10297116                  0.2615967
#>   spatial_predictor_1000_103 spatial_predictor_1000_104
#> 1                -0.29923617                -0.03765327
#> 2                -0.17878932                 0.23855400
#> 3                -0.46967375                 0.54136506
#> 4                 0.05539984                 0.32979379
#> 5                -0.17442574                 0.40090282
#> 6                 0.48879101                 0.48208675
#>   spatial_predictor_1000_105 spatial_predictor_1000_106
#> 1                -0.38730058                0.235802729
#> 2                -0.74885898                0.182011200
#> 3                -0.09956545               -0.152069465
#> 4                 0.20119653                0.003290319
#> 5                -0.15611980                0.054587819
#> 6                -0.17351806                0.368774891
#>   spatial_predictor_1000_107 spatial_predictor_1000_108
#> 1                -0.01711965                -0.63360804
#> 2                -0.08441605                 0.27095205
#> 3                -0.21600666                -0.05514253
#> 4                 0.48783827                -0.05830070
#> 5                -0.02116593                 0.11978384
#> 6                 0.21193640                -0.46096917
#>   spatial_predictor_1000_109 spatial_predictor_1000_110
#> 1                -0.05593276              -0.0001220432
#> 2                 0.09730718               0.0002480737
#> 3                 0.42932273               0.0013553320
#> 4                 0.02460746               0.0001382205
#> 5                 0.21167070               0.0006776448
#> 6                -0.52209898              -0.0015613418
#>   spatial_predictor_1000_111 spatial_predictor_1000_112
#> 1                -0.01158962                -0.02821928
#> 2                 0.51319577                -0.20999123
#> 3                 0.35488436                 0.15780591
#> 4                 0.29210908                -0.71084135
#> 5                -0.12874545                 0.42035588
#> 6                 0.39472939                 0.06789094
#>   spatial_predictor_1000_113 spatial_predictor_1000_114
#> 1                -0.01899772                -0.41983245
#> 2                -0.05140725                 0.06755910
#> 3                -0.02538724                 0.07794361
#> 4                -0.01765101                -0.10679879
#> 5                -0.18007977                -0.14447927
#> 6                 0.18141688                 0.17449499
#>   spatial_predictor_1000_115 spatial_predictor_1000_116
#> 1                 0.04086579                 0.32799319
#> 2                -0.21124473                -0.02215237
#> 3                -0.12795602                -0.14319319
#> 4                 0.14957615                 0.12152644
#> 5                 0.05237376                 0.16831162
#> 6                -0.02106558                -0.57450841
#>   spatial_predictor_1000_117 spatial_predictor_1000_118
#> 1                -0.29055851                 0.59744027
#> 2                 0.03597917                -0.04503977
#> 3                 0.19887687                 0.09185366
#> 4                -0.11006035                -0.05418722
#> 5                -0.01024507                -0.12300611
#> 6                 0.27862740                 0.38399101
#>   spatial_predictor_1000_119 spatial_predictor_1000_120
#> 1                 0.04926071              -4.769155e-05
#> 2                -0.01369482              -5.577803e-05
#> 3                 0.08203879              -3.205527e-05
#> 4                 0.19093974              -1.157751e-04
#> 5                -0.14650711              -8.125763e-05
#> 6                -0.27778132              -5.462763e-06
#>   spatial_predictor_1000_121 spatial_predictor_1000_122
#> 1                  0.3873724                  0.0388225
#> 2                  0.4947354                 -0.3596190
#> 3                 -0.2868810                  0.0668885
#> 4                 -0.1016276                 -0.4371670
#> 5                 -0.2206454                 -0.4105715
#> 6                 -0.1357672                  0.3557067
#>   spatial_predictor_1000_123 spatial_predictor_1000_124
#> 1                -0.01687945                -0.29418652
#> 2                 0.30299469                 0.36770162
#> 3                 0.05894288                 0.03381909
#> 4                 0.02983715                 0.13095747
#> 5                 0.12117421                 0.34349064
#> 6                 0.01250515                -0.46348548
#>   spatial_predictor_1000_125 spatial_predictor_1000_126
#> 1                 0.63989633                -0.09945883
#> 2                 0.02863020                 0.33417404
#> 3                 0.23135716                 0.06841066
#> 4                 0.38674993                 0.49900765
#> 5                -0.08383792                 0.09483270
#> 6                 0.25903840                -0.01813702
#>   spatial_predictor_1000_127 spatial_predictor_1000_128
#> 1                 0.01525278                 0.08068148
#> 2                -0.03168691                -0.14406707
#> 3                -0.17621212                -0.06911660
#> 4                 0.48649732                -0.08780284
#> 5                 0.12473298                -0.18762897
#> 6                 0.23318187                 0.10363394
#>   spatial_predictor_1000_129 spatial_predictor_1000_130
#> 1               -0.009218467               9.205379e-05
#> 2                0.068960571               6.455978e-05
#> 3                0.096032205               4.125305e-05
#> 4               -0.133678147               5.574366e-05
#> 5                0.202462567               1.769061e-04
#> 6                0.087675753               5.736116e-05
#>   spatial_predictor_1000_131 spatial_predictor_1000_132
#> 1                 0.01526922                -0.03108702
#> 2                -0.18830668                 0.18717810
#> 3                -0.09350531                 0.09473859
#> 4                -0.32352980                 0.38970655
#> 5                -0.02441326                 0.67427320
#> 6                 0.04493872                -0.14355901
#>   spatial_predictor_1000_133 spatial_predictor_1000_134
#> 1               -0.108035585                 0.25618735
#> 2                0.201229563                -0.06039737
#> 3                0.001701353                -0.13615433
#> 4               -0.402028494                -0.06528587
#> 5               -0.186463256                -0.21558107
#> 6               -0.085901366                 0.32527359
#>   spatial_predictor_1000_135 spatial_predictor_1000_136
#> 1                 0.02719173                 0.07782244
#> 2                 0.21728578                -0.13863190
#> 3                 0.06449532                -0.08376184
#> 4                -0.03282081                 0.09232952
#> 5                 0.12893921                 0.10745242
#> 6                 0.17356754                -0.03100964
#>   spatial_predictor_1000_137 spatial_predictor_1000_138
#> 1                0.008844147                 -0.0165268
#> 2                0.144253093                 -0.1198514
#> 3               -0.187458584                  0.1115699
#> 4                0.118586759                 -0.3484621
#> 5               -0.301394719                  0.2147939
#> 6               -0.192354819                 -0.1251239
#>   spatial_predictor_1000_139 spatial_predictor_1000_140
#> 1                0.519660778                -0.16440638
#> 2                0.001917172                -0.10658212
#> 3                0.205748442                 0.06462444
#> 4                0.130851905                -0.33522911
#> 5               -0.541409623                 0.03187394
#> 6                0.132624081                 0.23228949
#>   spatial_predictor_1000_141 spatial_predictor_1000_142
#> 1                -0.29469105                -0.26749110
#> 2                -0.20642578                 0.12549727
#> 3                -0.07977042                 0.08096817
#> 4                -0.02558042                 0.21409745
#> 5                 0.33001476                 0.23412013
#> 6                 0.09045049                -0.17390379
#>   spatial_predictor_1000_143 spatial_predictor_1000_144
#> 1              -9.206344e-05              -0.0005592972
#> 2              -3.563774e-05               0.0002658979
#> 3              -3.848596e-05               0.0001186122
#> 4               8.275788e-05               0.0004696872
#> 5               1.344332e-04               0.0004637150
#> 6              -3.510016e-05              -0.0003796419
#>   spatial_predictor_1000_145 spatial_predictor_1000_146
#> 1                -0.05425864                0.095053701
#> 2                -0.24872931               -0.008810024
#> 3                -0.07453834                0.168655610
#> 4                 0.19934083               -0.057793915
#> 5                 0.26008154                0.004121833
#> 6                 0.08641070               -0.061938521
#>   spatial_predictor_1000_147 spatial_predictor_1000_148
#> 1               2.834211e-04                 0.18350624
#> 2              -7.970536e-05                 0.04275518
#> 3              -1.407978e-04                 0.09384395
#> 4               2.166871e-05                -0.21804064
#> 5              -1.818777e-04                 0.02210692
#> 6               1.468222e-05                -0.30265695
#>   spatial_predictor_1000_149 spatial_predictor_1000_150
#> 1                -0.12159275                 0.09022291
#> 2                -0.04563616                 0.05469155
#> 3                -0.03293483                -0.09430167
#> 4                 0.10919720                -0.44240661
#> 5                 0.01630492                -0.17213708
#> 6                -0.33013162                 0.10607782
#>   spatial_predictor_1000_151 spatial_predictor_1000_152
#> 1                -0.07044220              -8.801938e-05
#> 2                -0.10095742               9.650077e-05
#> 3                -0.31125871              -2.169334e-04
#> 4                -0.01946672              -1.038692e-05
#> 5                 0.10225323               8.838903e-05
#> 6                 0.06918140              -5.055664e-06
#>   spatial_predictor_1000_153 spatial_predictor_1000_154
#> 1                 0.17815589                -0.10354650
#> 2                 0.04496551                -0.49412247
#> 3                -0.02255119                -0.04404598
#> 4                 0.02078749                 0.17960310
#> 5                 0.06142089                 0.01401374
#> 6                -0.18950145                -0.04827998
#>   spatial_predictor_1000_155 spatial_predictor_1000_156
#> 1                0.099261998                 -0.1950990
#> 2                0.274421812                 -0.3507830
#> 3                0.050017563                 -0.0525186
#> 4               -0.001171834                  0.2479872
#> 5                0.165503634                  0.1165571
#> 6               -0.246983897                 -0.2087581
#>   spatial_predictor_1000_157 spatial_predictor_1000_158
#> 1                -0.10713110                -0.30102735
#> 2                 0.26351274                -0.21377240
#> 3                -0.02777393                 0.07521524
#> 4                 0.16189688                -0.13448902
#> 5                 0.49462594                 0.43128258
#> 6                -0.18450468                -0.12744868
#>   spatial_predictor_1000_159 spatial_predictor_1000_160
#> 1               -0.354617312               -0.010189831
#> 2               -0.081571617               -0.005886126
#> 3                0.014183450                0.097285925
#> 4                0.160568509               -0.141647311
#> 5                0.291796419               -0.077603718
#> 6                0.005219616                0.013746617
#>   spatial_predictor_1000_161 spatial_predictor_1000_162
#> 1                 0.06664901                 0.04322703
#> 2                 0.04995442                -0.13122305
#> 3                 0.09339931                -0.01497567
#> 4                -0.32705615                 0.05251200
#> 5                -0.09564079                 0.19040276
#> 6                 0.02563554                 0.13164388
#>   spatial_predictor_1000_163 spatial_predictor_1000_164
#> 1                 0.17848412                -0.10214687
#> 2                -0.10411648                -0.37000609
#> 3                -0.04582656                -0.13455419
#> 4                 0.23848115                 0.02550001
#> 5                -0.19658219                 0.18927122
#> 6                -0.03991719                -0.16538768
#>   spatial_predictor_1000_165 spatial_predictor_1000_166
#> 1                 0.21015712                -0.56335679
#> 2                -0.02822209                 0.17894816
#> 3                 0.07690353                 0.05686530
#> 4                 0.12651406                -0.01364143
#> 5                -0.03499097                -0.52642799
#> 6                -0.10865861                -0.04492617
#>   spatial_predictor_1000_167 spatial_predictor_1000_168
#> 1                -0.24385381                 0.13185590
#> 2                -0.21956908                -0.05279734
#> 3                 0.03223869                -0.21241011
#> 4                -0.49380248                 0.18079744
#> 5                 0.14490477                 0.07486651
#> 6                -0.07380817                 0.01647000
#>   spatial_predictor_1000_169 spatial_predictor_1000_170
#> 1                 0.03845440               -0.212954572
#> 2                 0.08217303               -0.029492020
#> 3                -0.28202065                0.028027184
#> 4                -0.11538412                0.286355785
#> 5                -0.07465713                0.002583255
#> 6                -0.05451610               -0.007976387
#>   spatial_predictor_1000_171 spatial_predictor_1000_172
#> 1                -0.04009098                0.034887148
#> 2                 0.10591745                0.003690949
#> 3                 0.08688216                0.079779359
#> 4                -0.09713410                0.457566184
#> 5                 0.09269985               -0.233819436
#> 6                 0.21182002                0.051400708
#>   spatial_predictor_1000_173 spatial_predictor_1000_174
#> 1                -0.04670346                 0.17887901
#> 2                 0.15528449                 0.17162563
#> 3                -0.31570546                 0.02075468
#> 4                 0.11429984                 0.28470229
#> 5                 0.09265256                 0.01492258
#> 6                -0.07600530                 0.15891999
#>   spatial_predictor_1000_175 spatial_predictor_1000_176
#> 1                 0.04229059                 0.13343464
#> 2                 0.18855511                 0.29513407
#> 3                 0.07289341                -0.07670053
#> 4                 0.19812513                -0.09584285
#> 5                 0.01307185                -0.25307693
#> 6                -0.04055946                 0.24508289
#>   spatial_predictor_1000_177 spatial_predictor_1000_178
#> 1                -0.17520563                 0.06347715
#> 2                 0.05269895                 0.13210017
#> 3                -0.16612732                -0.20515649
#> 4                -0.05841842                -0.05331353
#> 5                -0.21463020                -0.06838614
#> 6                 0.26217535                -0.12559696
#>   spatial_predictor_1000_179 spatial_predictor_1000_180
#> 1                -0.03194236                 0.13722181
#> 2                 0.03791062                 0.07987399
#> 3                -0.13801243                 0.27875851
#> 4                -0.32845563                -0.18948463
#> 5                -0.01899609                 0.01279153
#> 6                 0.24899721                 0.16339285
#>   spatial_predictor_1000_181 spatial_predictor_1000_182
#> 1                 0.04544184                -0.20861334
#> 2                -0.01133628                 0.05988454
#> 3                 0.08034252                 0.02920672
#> 4                -0.16371934                 0.13305155
#> 5                 0.27950224                 0.04742361
#> 6                -0.12606141                 0.09828413
#>   spatial_predictor_1000_183 spatial_predictor_1000_184
#> 1                 0.21283910                -0.32031592
#> 2                -0.39579572                 0.29855882
#> 3                -0.20357931                 0.07368100
#> 4                -0.14738299                 0.06260181
#> 5                 0.21435746                 0.34065720
#> 6                 0.06497147                -0.08345326
#>   spatial_predictor_1000_185 spatial_predictor_1000_186
#> 1                -0.14067315                 0.18188324
#> 2                 0.01526457                 0.28749570
#> 3                 0.01100462                -0.02844233
#> 4                 0.17210407                -0.29218754
#> 5                -0.01567314                -0.01215931
#> 6                 0.09300769                -0.31900580
#>   spatial_predictor_1000_187 spatial_predictor_1000_188
#> 1                0.198233682                -0.08633818
#> 2               -0.202243117                -0.07957483
#> 3               -0.098496134                -0.13356255
#> 4               -0.051351723                 0.02904595
#> 5                0.008509372                 0.08472043
#> 6               -0.088099944                -0.01836947
#>   spatial_predictor_1000_189 spatial_predictor_1000_190
#> 1                0.105909518                 0.03271559
#> 2               -0.009954965                -0.04941379
#> 3                0.076870719                -0.04124257
#> 4                0.051384017                 0.12033513
#> 5               -0.161484836                 0.16003697
#> 6               -0.001939472                -0.04467582
#>   spatial_predictor_1000_191 spatial_predictor_1000_192
#> 1                 0.11007316                -0.15048673
#> 2                 0.06524823                -0.02600331
#> 3                 0.05268069                -0.08252396
#> 4                 0.03813590                -0.07544144
#> 5                 0.03224471                -0.03497054
#> 6                -0.07401339                 0.12752773
#>   spatial_predictor_1000_193 spatial_predictor_1000_194
#> 1                 0.04415891              -0.0008019213
#> 2                 0.07149690              -0.0696793931
#> 3                 0.01938151               0.1199967056
#> 4                 0.19725002               0.1402371965
#> 5                 0.04608919               0.2014837336
#> 6                -0.06571467              -0.0719400130
#>   spatial_predictor_1000_195 spatial_predictor_1000_196
#> 1                 0.00981630                -0.01122345
#> 2                 0.04417014                 0.01846970
#> 3                -0.05613273                -0.06795751
#> 4                 0.12380374                 0.04109517
#> 5                 0.07467824                 0.10410837
#> 6                -0.02626196                 0.06641749
#>   spatial_predictor_1000_197 spatial_predictor_1000_198
#> 1                 0.07106718               -0.053902297
#> 2                -0.01123298                0.005815729
#> 3                -0.03202435                0.035645698
#> 4                 0.15686903               -0.115004252
#> 5                 0.11830548               -0.012965962
#> 6                -0.12652194               -0.019920603
#>   spatial_predictor_1000_199 spatial_predictor_1000_200
#> 1                -0.20623530               -0.023628149
#> 2                -0.02415067                0.030543279
#> 3                -0.01165361                0.165514099
#> 4                -0.07619757               -0.009055363
#> 5                 0.25024723               -0.108666802
#> 6                -0.06069160                0.017011133
#>   spatial_predictor_1000_201 spatial_predictor_1000_202
#> 1                0.002298788                -0.07259218
#> 2               -0.073131474                 0.31899637
#> 3               -0.111197457                -0.14547341
#> 4                0.078486442                 0.02427857
#> 5               -0.052776679                -0.01092183
#> 6                0.032488605                -0.03156595
#>   spatial_predictor_1000_203 spatial_predictor_1000_204
#> 1                0.043604775                 0.03961221
#> 2               -0.049491198                -0.09647992
#> 3               -0.018409201                -0.06049594
#> 4               -0.095391714                -0.03547790
#> 5                0.058986200                 0.05186302
#> 6               -0.009963391                 0.10151298
#>   spatial_predictor_1000_205 spatial_predictor_1000_206
#> 1                0.055339397               -0.001925527
#> 2                0.190099604               -0.002813292
#> 3               -0.002192481                0.160548730
#> 4                0.051969875               -0.005835412
#> 5                0.086413784                0.067380110
#> 6                0.019153111                0.101600044
#>   spatial_predictor_1000_207 spatial_predictor_1000_208
#> 1                -0.11332950                 0.08458987
#> 2                 0.02807374                -0.06482486
#> 3                 0.01253373                 0.13763975
#> 4                 0.11427849                 0.01501004
#> 5                -0.05636236                -0.03662402
#> 6                -0.05509374                -0.07817151
#>   spatial_predictor_1000_209 spatial_predictor_1000_210
#> 1                -0.01303693                 0.02171650
#> 2                 0.06539774                -0.07383373
#> 3                -0.03560779                -0.08436196
#> 4                 0.04120389                 0.03149245
#> 5                -0.03772202                -0.07329934
#> 6                -0.01210445                -0.06521393
#>   spatial_predictor_1000_211 spatial_predictor_1000_212
#> 1                -0.03768543                 0.10121648
#> 2                -0.01030415                -0.08007344
#> 3                -0.03358114                -0.04259485
#> 4                 0.02593932                 0.04834863
#> 5                -0.06795183                 0.02196886
#> 6                 0.06715684                -0.04693980
#>   spatial_predictor_1000_213 spatial_predictor_1000_214
#> 1               -0.076059766               -0.008073652
#> 2               -0.015890562                0.094634233
#> 3                0.014661895               -0.097279500
#> 4               -0.004992699               -0.025746433
#> 5               -0.016381206                0.012394422
#> 6                0.037207073                0.035573967
#>   spatial_predictor_1000_215 spatial_predictor_1000_216
#> 1               0.0102583954              -0.0008256536
#> 2              -0.0051012027               0.0094283948
#> 3               0.0717760449               0.0310794373
#> 4              -0.0013948909               0.0321282002
#> 5               0.0071850153              -0.0475434530
#> 6               0.0009507676               0.0506333693
#>   spatial_predictor_1000_217 spatial_predictor_1000_218
#> 1                0.043910002               0.0008037296
#> 2                0.029172498               0.0066914909
#> 3                0.018844708               0.0410163167
#> 4               -0.005036992              -0.0024965269
#> 5                0.019674666               0.0063390677
#> 6                0.009365937              -0.0150271488
#>   spatial_predictor_1000_219 spatial_predictor_1000_220
#> 1                0.016469482                0.004290533
#> 2                0.027267178                0.002228241
#> 3                0.002172922               -0.012184651
#> 4               -0.004160153               -0.040165613
#> 5                0.027002490               -0.008413590
#> 6                0.003038506               -0.011546975
#>   spatial_predictor_1000_221 spatial_predictor_1000_222
#> 1              -0.0202609754                0.034510611
#> 2               0.0070844180               -0.008768400
#> 3               0.0191296923               -0.010864004
#> 4              -0.0007575674                0.001374694
#> 5              -0.0075209326                0.021926368
#> 6              -0.0243947198               -0.010496391
#>   spatial_predictor_1000_223 spatial_predictor_1000_224
#> 1              -0.0162415807                0.014190063
#> 2              -0.0015334450                0.009492912
#> 3               0.0049023619                0.001689626
#> 4               0.0008329817               -0.015257705
#> 5              -0.0005376181                0.014034381
#> 6              -0.0134062331               -0.006183478
#>   spatial_predictor_1000_225 spatial_predictor_1000_226
#> 1               -0.004665539              -3.348231e-04
#> 2               -0.006730020               2.210457e-04
#> 3               -0.003366476               1.489924e-05
#> 4                0.004812213              -3.082347e-04
#> 5               -0.007005745              -1.075486e-04
#> 6               -0.010630333              -1.000373e-04
#>   spatial_predictor_5000_1 spatial_predictor_5000_2 spatial_predictor_5000_3
#> 1                7.4979321                 5.595937               -4.6140489
#> 2                0.7712476                -1.129660               -1.7997206
#> 3              -15.7241852                 1.794907               -1.5493741
#> 4                4.5564693                 3.387387               -4.9760367
#> 5               14.9079125                 5.256986                1.2863232
#> 6               13.1382641                 6.269924               -0.2384563
#>   spatial_predictor_5000_4 spatial_predictor_5000_5 spatial_predictor_5000_6
#> 1               -0.2906988               -2.1696067                1.0823500
#> 2               -0.6328311                1.9710152               -0.4123112
#> 3                4.1211026                0.6096068               -2.4274895
#> 4               -1.2882175                1.1412318                1.6785063
#> 5                3.2147224               -0.4604862               -3.0726206
#> 6                2.3164418               -2.0119794               -3.1643816
#>   spatial_predictor_5000_7 spatial_predictor_5000_8 spatial_predictor_5000_9
#> 1                0.1953238              -1.72267518               -1.5343426
#> 2                0.5297982              -0.08893646               -1.1137534
#> 3               -0.3622315               0.50189047                0.8282490
#> 4               -1.4574283               0.06430028                1.6372043
#> 5                0.7744369              -0.11007952                0.7233848
#> 6                1.3354207              -0.09249997               -0.3837810
#>   spatial_predictor_5000_10 spatial_predictor_5000_11 spatial_predictor_5000_12
#> 1                 0.2006849                 0.3210210              -0.442611654
#> 2                 0.9506206                -0.4222309               0.842227889
#> 3                 0.8148940                 0.7305228              -1.660624044
#> 4                -0.7582085                -0.1378830              -0.007454565
#> 5                 1.3423643                 0.4249159               1.765370899
#> 6                 0.5881006                 0.4370875               1.074596304
#>   spatial_predictor_5000_13 spatial_predictor_5000_14 spatial_predictor_5000_15
#> 1                -1.2613078                 0.5562651                -0.8481707
#> 2                 0.7300344                -0.3362488                -0.1809708
#> 3                -0.2532822                 2.1175828                 0.6618591
#> 4                 0.2138439                -0.3735321                 0.3142995
#> 5                 1.7138988                -2.1207283                -1.2723014
#> 6                 2.7535148                -1.6438034                -0.2226413
#>   spatial_predictor_5000_16 spatial_predictor_5000_17 spatial_predictor_5000_18
#> 1                 0.8633895               -0.57314924                -0.6914929
#> 2                 0.5350653               -0.29131158                 0.5348776
#> 3                 0.4114113               -0.12222711                 0.1350121
#> 4                -0.2313489                0.23715618                 0.1549923
#> 5                 2.8458315                0.53786790                 0.3553201
#> 6                 2.1134007                0.06667853                 1.7797691
#>   spatial_predictor_5000_19 spatial_predictor_5000_20 spatial_predictor_5000_21
#> 1              -0.270262830                 0.3197082                0.42319277
#> 2               0.492108220                -1.0776757               -0.04636031
#> 3               0.417469162                -0.2211733               -0.28176744
#> 4               0.079792953                -0.4885719                0.15375955
#> 5               1.168903350                -0.6395226               -0.64151873
#> 6               0.002727684                -0.4542828               -0.41030909
#>   spatial_predictor_5000_22 spatial_predictor_5000_23 spatial_predictor_5000_24
#> 1                0.71145536                 0.4482550               -0.76265526
#> 2               -0.91359014                 0.1428673               -0.70936142
#> 3               -0.20300149                -0.3169446                0.43281716
#> 4                0.05158425                 0.0549127                0.03495173
#> 5               -0.11672818                 0.2684882                1.74065350
#> 6               -1.51524311                -0.5929709                0.91434970
#>   spatial_predictor_5000_25 spatial_predictor_5000_26 spatial_predictor_5000_27
#> 1                0.96051562               -0.36947503                 0.3579769
#> 2               -0.42563435                0.08151393                 0.3952549
#> 3                0.04458953               -0.28247543                 0.9008243
#> 4               -0.14023263                0.02906035                 0.1360465
#> 5                1.03046011                2.25773489                 0.2385972
#> 6               -0.30084372               -0.44703352                -0.6287328
#>   spatial_predictor_5000_28 spatial_predictor_5000_29 spatial_predictor_5000_30
#> 1               -0.44102558               -0.18278714              -0.196409718
#> 2                0.03655041                0.37713917               0.266503437
#> 3                0.21644984               -0.17969901              -0.007827404
#> 4               -0.24325172                0.03448812              -0.157367606
#> 5               -0.20244564                0.32986829              -0.811992534
#> 6                0.40617433               -0.20423596              -1.415128785
#>   spatial_predictor_5000_31 spatial_predictor_5000_32 spatial_predictor_5000_33
#> 1                0.09420409                -0.3094583                0.09723619
#> 2                0.34485439                 0.1738221               -0.95954397
#> 3                0.26359530                -0.5075563                0.25361416
#> 4                0.02529313                 0.4195615                0.27185472
#> 5                0.99791697                -0.3581715                0.48737776
#> 6                0.63378599                 0.7152506               -0.24381513
#>   spatial_predictor_5000_34 spatial_predictor_5000_35 spatial_predictor_5000_36
#> 1               -0.55170139               -0.61545951                0.36556838
#> 2                0.56219493               -0.47144569                0.01606765
#> 3               -0.08998538                0.34380054                0.19664004
#> 4                0.14441646                0.23438333               -0.14960756
#> 5               -0.21819161                0.09931056                0.75484256
#> 6                0.21562044                0.18227621                0.80162721
#>   spatial_predictor_5000_37 spatial_predictor_5000_38 spatial_predictor_5000_39
#> 1                0.25703206              -0.525246617               -0.15998989
#> 2               -0.71062274              -0.003055872               -0.32899205
#> 3                0.06928158              -0.524837342                0.12427652
#> 4                0.49314782               0.086136782                0.03755471
#> 5                0.16679475               0.632274921               -0.60125824
#> 6                0.86481436               0.524513757               -0.56080834
#>   spatial_predictor_5000_40 spatial_predictor_5000_41 spatial_predictor_5000_42
#> 1                0.50187688                 0.3426003               -0.29938582
#> 2               -0.57866160                 0.8051492                0.16026383
#> 3               -0.74383680                 0.1497253                0.04332225
#> 4               -0.19872692                -0.5493496                0.29599197
#> 5               -0.09429786                -0.6664387                1.21141751
#> 6                0.16943011                 0.3467094               -1.27358337
#>   spatial_predictor_5000_43 spatial_predictor_5000_44 spatial_predictor_5000_45
#> 1                -0.5874966               -0.17224706               -0.36313269
#> 2                 0.1091538                0.33081097                0.41291020
#> 3                -0.1657209               -0.28772913                0.24509889
#> 4                 0.1731327                0.48977226                0.17838189
#> 5                -0.5266263               -0.09052172                0.06357287
#> 6                -0.1403866               -0.41375662                0.02171096
#>   spatial_predictor_5000_46 spatial_predictor_5000_47 spatial_predictor_5000_48
#> 1                0.20811220                0.12285804                -0.2362069
#> 2               -0.07305645                0.41417768                 0.1328381
#> 3                0.74713321               -0.15895816                 0.3056470
#> 4                0.32977991               -0.20699780                 0.4665904
#> 5               -0.80144759               -0.06381196                 0.2614053
#> 6               -0.41849279                1.11610021                 0.2978662
#>   spatial_predictor_5000_49 spatial_predictor_5000_50 spatial_predictor_5000_51
#> 1                0.25789616                -0.4788634                 0.4528828
#> 2               -0.21882567                -0.3636884                 0.6654397
#> 3                0.46053687                 0.2619842                -0.3361338
#> 4               -0.02054676                -1.7136093                -0.7619410
#> 5               -0.37886611                 0.5238826                -0.4104238
#> 6                0.42617449                -0.3461467                -0.1933895
#>   spatial_predictor_5000_52 spatial_predictor_5000_53 spatial_predictor_5000_54
#> 1               -0.37715322               -0.33533540                0.19933823
#> 2               -0.42474013               -0.54771567               -0.33018587
#> 3               -0.32485373                0.60782961               -0.30474434
#> 4               -0.27281229                0.01983853               -0.01214754
#> 5               -0.04985309                0.12288456               -0.35932739
#> 6                0.78744846                0.86805590               -0.07060811
#>   spatial_predictor_5000_55 spatial_predictor_5000_56 spatial_predictor_5000_57
#> 1               -0.54256219               -0.22586751               -0.25502529
#> 2               -0.29389418               -0.65109109                1.12370402
#> 3               -0.20106019               -0.10341341               -0.45872168
#> 4               -0.05148105               -0.03462309                0.31214388
#> 5                0.02295841                0.24292832                0.02958462
#> 6                0.63379739               -0.80191717               -0.85736869
#>   spatial_predictor_5000_58 spatial_predictor_5000_59 spatial_predictor_5000_60
#> 1               -0.07758414                -0.2530976               -0.04783070
#> 2                0.49011847                -0.3357097                0.31443243
#> 3                0.60177609                -0.1212974                0.33500265
#> 4               -0.17769991                 1.2476277                0.05800749
#> 5               -0.45918880                -0.9158359                0.17908340
#> 6                0.17471196                 0.5186434               -0.53304310
#>   spatial_predictor_5000_61 spatial_predictor_5000_62 spatial_predictor_5000_63
#> 1               -0.05008493              -0.009809992                0.02798676
#> 2                0.06467809              -0.050524769               -0.07902594
#> 3                0.23761462              -0.138697150                0.08882091
#> 4               -0.11890711              -0.634249936                0.22521078
#> 5               -0.95099972              -0.016246060                0.83307487
#> 6               -0.24590574               0.040777716                0.36220226
#>   spatial_predictor_5000_64 spatial_predictor_5000_65 spatial_predictor_5000_66
#> 1                 0.0519892                0.01748338                0.76134440
#> 2                 0.6491336               -0.39269294               -0.07591427
#> 3                 0.3602849               -0.17392083                0.01779709
#> 4                 0.6521110                0.01208535                0.31751506
#> 5                 0.6325593                0.19899809               -0.26671952
#> 6                -0.7259020               -0.37270941                0.87049695
#>   spatial_predictor_5000_67 spatial_predictor_5000_68 spatial_predictor_5000_69
#> 1               -0.47365169               0.244116027              -0.395432574
#> 2                0.13100337               0.001843066              -0.172573103
#> 3                0.00163444               0.150398914              -0.092788569
#> 4               -0.51481357              -0.048544118              -0.008988118
#> 5               -0.57654009               0.571781621               0.188326006
#> 6                0.10948841              -0.243064559               0.291946152
#>   spatial_predictor_5000_70 spatial_predictor_5000_71 spatial_predictor_5000_72
#> 1                0.12870826             -0.1696316663                0.02799090
#> 2                0.06706391              0.2039592511                0.05359738
#> 3               -0.04670764              0.2301505207               -0.41163584
#> 4                0.07383810              0.0006520584                0.17570213
#> 5                0.07143641              0.1475035572                0.16504222
#> 6                0.19839872             -0.1640378474               -0.21476398
#>   spatial_predictor_5000_73 spatial_predictor_5000_74 spatial_predictor_5000_75
#> 1               -0.33509243               -0.42016879                0.01912629
#> 2               -0.63053415               -0.44166759               -0.34448474
#> 3                0.05844466               -0.05310329               -0.10311279
#> 4                0.31551463               -0.03634220                0.02917954
#> 5                0.01991342                0.25174997                0.10976623
#> 6               -0.34834012               -0.29984229               -0.06658303
#>   spatial_predictor_5000_76 spatial_predictor_5000_77 spatial_predictor_5000_78
#> 1                0.04126469              -0.569376721              -0.024281748
#> 2               -0.12143825               0.462191021              -0.233378890
#> 3                0.07279637              -0.005086831               0.008821536
#> 4               -0.11941841               0.019670639               0.083130154
#> 5               -0.10825473              -0.096787964              -0.023040440
#> 6                0.16739874              -0.001853316              -0.037666142
#>   spatial_predictor_5000_79 spatial_predictor_5000_80 spatial_predictor_5000_81
#> 1                0.66487504              -0.113181937                0.41666988
#> 2                0.16692584              -0.221238709               -0.03221559
#> 3               -0.03434715               0.001249572               -0.01551276
#> 4               -0.00384974               0.025055563                0.05993084
#> 5               -0.11289553               0.133246246                0.03526176
#> 6                0.15920622              -0.151624845               -0.03014616
#>   spatial_predictor_5000_82 spatial_predictor_5000_83 spatial_predictor_5000_84
#> 1               -0.51336864               0.901450567               0.070508884
#> 2               -0.02373032              -0.008763726               0.046498246
#> 3                0.01238682               0.010915139               0.001555324
#> 4               -0.01488764              -0.114106065              -0.009269658
#> 5               -0.14908784              -0.226839394              -0.303282372
#> 6                0.36494100              -0.119884695               0.446472319
#>   spatial_predictor_5000_85 spatial_predictor_5000_86 spatial_predictor_5000_87
#> 1              -0.507168031               0.340221035                0.14268556
#> 2               0.053273354               0.004965765                0.06539732
#> 3               0.005989788               0.002682771                0.02050297
#> 4              -0.066137652               0.058247304               -0.06759365
#> 5              -0.244596490               0.004440773               -0.09645002
#> 6               0.002305655              -0.232783091                0.13595690
#>   spatial_predictor_5000_88 spatial_predictor_5000_89 spatial_predictor_5000_90
#> 1                0.35481119               0.296113738               0.008690046
#> 2                0.02334376              -0.106500741              -0.078312537
#> 3               -0.02448715              -0.013716238               0.019646973
#> 4               -0.05734959               0.073947106               0.244668450
#> 5               -0.14405823               0.002461805               0.021274943
#> 6                0.03924458               0.128551275               0.279380606
#>   spatial_predictor_5000_91 spatial_predictor_5000_92 spatial_predictor_5000_93
#> 1                0.35753898               0.100596004                0.14740248
#> 2                0.17211190               0.019309853               -0.02548785
#> 3               -0.08708766               0.009787409               -0.01376533
#> 4               -0.17398020              -0.100412263                0.03593894
#> 5                0.17529790               0.221671046               -0.07828306
#> 6               -0.17840433               0.120004931               -0.11403609
#>   spatial_predictor_5000_94 spatial_predictor_5000_95 spatial_predictor_5000_96
#> 1             -2.320086e-05              4.326749e-06             -5.476252e-06
#> 2             -1.046296e-07             -1.581923e-08              1.357082e-06
#> 3              3.519860e-06             -8.757295e-07             -3.537135e-07
#> 4             -8.597073e-07              1.909937e-06              3.472439e-06
#> 5             -1.586482e-06             -6.998965e-06             -7.976960e-06
#> 6              3.196971e-06             -1.848679e-07             -1.934321e-07
#>   spatial_predictor_5000_97 spatial_predictor_5000_98 spatial_predictor_5000_99
#> 1              1.165977e-06              -0.444101079              2.525777e-05
#> 2             -1.311240e-06               0.003519954              9.170230e-07
#> 3              4.441571e-07               0.031934392              4.176068e-06
#> 4             -7.165957e-07              -0.003676764             -4.924401e-06
#> 5             -1.904399e-06              -0.385408833              1.141964e-05
#> 6             -8.337438e-07               0.083141078             -2.944736e-06
#>   spatial_predictor_5000_100 spatial_predictor_5000_101
#> 1                 0.18020313               8.424397e-06
#> 2                -0.03872920              -3.188928e-06
#> 3                 0.10920120               3.335295e-07
#> 4                -0.15718241              -5.355552e-06
#> 5                 0.09600049               6.908224e-06
#> 6                -0.01236619              -3.075099e-06
#>   spatial_predictor_5000_102 spatial_predictor_5000_103
#> 1               1.674510e-06               3.793906e-06
#> 2              -3.278014e-06               3.279807e-06
#> 3               2.278548e-07               2.349076e-07
#> 4              -1.941629e-06              -3.252440e-06
#> 5               8.415033e-07               3.305046e-06
#> 6              -1.623662e-06               4.176191e-07
#>   spatial_predictor_5000_104 spatial_predictor_5000_105
#> 1               1.989033e-06               0.0042395366
#> 2               4.079831e-06              -0.1923160011
#> 3              -4.762781e-06              -0.0247450838
#> 4               8.125972e-06              -0.0738093537
#> 5              -9.518554e-07               0.0007159029
#> 6              -1.374238e-06              -0.0314885372
#>   spatial_predictor_5000_106 spatial_predictor_5000_107
#> 1                 0.01497052                 0.02870536
#> 2                 0.31929516                 0.02032415
#> 3                 0.01045909                 0.03938088
#> 4                 0.07457181                -0.17322072
#> 5                 0.01933428                 0.04712521
#> 6                 0.01492337                -0.02374653
#>   spatial_predictor_5000_108 spatial_predictor_5000_109
#> 1               -0.009355454                0.037406947
#> 2               -0.088349347               -0.108730190
#> 3                0.018247384                0.032030412
#> 4                0.499633363               -0.293777812
#> 5               -0.068489745               -0.008381988
#> 6               -0.012547073               -0.030425420
#>   spatial_predictor_5000_110 spatial_predictor_5000_111
#> 1                0.031927585              -1.968853e-06
#> 2               -0.133254434              -1.703649e-05
#> 3                0.120658266               1.706940e-05
#> 4               -0.006515499               3.007400e-05
#> 5                0.070832372               9.169846e-06
#> 6                0.045184041               5.028378e-06
#>   spatial_predictor_5000_112 spatial_predictor_5000_113
#> 1              -9.175133e-08              -4.206619e-06
#> 2              -1.659465e-05               8.314762e-06
#> 3               2.164742e-05              -1.382453e-05
#> 4               3.906296e-05              -1.920322e-05
#> 5               1.483281e-05               1.047723e-06
#> 6               3.736092e-06              -8.155946e-07
#>   spatial_predictor_5000_114 spatial_predictor_5000_115
#> 1               -0.038395803                -0.01523244
#> 2                0.050011019                 0.03259013
#> 3               -0.096712385                -0.03970536
#> 4               -0.143393905                -0.11269944
#> 5                0.034438303                -0.27773207
#> 6               -0.007145869                -0.01362833
#>   spatial_predictor_5000_116 spatial_predictor_5000_117
#> 1              -7.822445e-07                0.008179178
#> 2              -3.588491e-06               -0.065329576
#> 3              -2.077804e-06                0.011952425
#> 4              -4.672124e-06               -0.003098401
#> 5               3.955664e-06                0.034260541
#> 6              -5.519322e-07                0.069855289
#>   spatial_predictor_5000_118 spatial_predictor_5000_119
#> 1                0.010806712                 0.04854364
#> 2               -0.012605983                -0.01776150
#> 3               -0.004606653                -0.01428742
#> 4                0.002947441                 0.03709683
#> 5                0.045019687                 0.24198324
#> 6                0.022331950                 0.07838829
#>   spatial_predictor_5000_120 spatial_predictor_5000_121
#> 1              -5.105177e-07                0.047895481
#> 2              -1.273485e-06               -0.072042386
#> 3               1.068284e-06                0.004986572
#> 4              -5.966780e-07               -0.018653246
#> 5               2.982218e-06                0.057277615
#> 6               1.234462e-06                0.056815863
#>   spatial_predictor_5000_122 spatial_predictor_5000_123
#> 1               -0.148335064               2.505112e-05
#> 2               -0.058704625               2.289217e-06
#> 3                0.019944436              -5.092683e-05
#> 4               -0.159616291               1.532685e-05
#> 5                0.152229749              -1.714996e-05
#> 6                0.006630654               3.870150e-06
#>   spatial_predictor_5000_124 spatial_predictor_5000_125
#> 1              -1.826631e-05              -7.271981e-07
#> 2              -2.241790e-06               4.273531e-07
#> 3              -4.802262e-01              -1.739532e+00
#> 4              -5.393473e-06              -1.633627e-07
#> 5              -2.853686e-06              -2.455722e-07
#> 6              -2.671526e-07               4.140225e-07
#>   spatial_predictor_5000_126 spatial_predictor_5000_127
#> 1              -4.173751e-06              -5.592708e-06
#> 2              -6.779299e-07              -2.048445e-07
#> 3               4.469744e-07               9.443975e-07
#> 4              -1.176320e-06              -1.410786e-06
#> 5              -1.085580e-06              -1.664061e-06
#> 6              -2.922447e-07              -2.908489e-07
#>   spatial_predictor_5000_128 spatial_predictor_5000_129
#> 1              -3.032812e-06              -4.927817e-06
#> 2              -1.773210e-06              -1.877274e-06
#> 3              -3.398981e-07               8.525197e-08
#> 4              -1.050269e-06              -7.264175e-07
#> 5               2.906030e-08              -1.189820e-06
#> 6              -4.091938e-07              -6.716364e-07
#>   spatial_predictor_5000_130 spatial_predictor_5000_131
#> 1               -0.151329942                0.025700635
#> 2               -0.077974465                0.008209496
#> 3               -0.003884057                0.005776471
#> 4               -0.022705724                0.007292786
#> 5               -0.021398417               -0.026128559
#> 6               -0.035086341                0.037406473
#>   spatial_predictor_5000_132 spatial_predictor_5000_133
#> 1                0.086648096                0.035610723
#> 2                0.017318494                0.010609695
#> 3                0.014449892                0.002845015
#> 4               -0.003244472               -0.005860672
#> 5               -0.123303765               -0.024440175
#> 6                0.204287101                0.050708644
#>   spatial_predictor_5000_134 spatial_predictor_5000_135
#> 1               5.414258e-06               5.226581e-08
#> 2               4.761222e-06              -3.724000e-07
#> 3               3.837329e-07              -2.000071e-10
#> 4               5.925425e-07              -1.660098e-07
#> 5               3.419509e-06              -4.428840e-08
#> 6              -2.715417e-06              -4.307569e-08
#>   spatial_predictor_5000_136 spatial_predictor_5000_137
#> 1               3.882495e-06               5.208199e-07
#> 2               6.022336e-06              -7.244314e-08
#> 3               4.361224e-07               7.353512e-08
#> 4               1.281441e-06               1.807581e-07
#> 5               2.572735e-06               3.730601e-07
#> 6              -1.450940e-06              -5.372402e-07
#>   spatial_predictor_5000_138 spatial_predictor_5000_139
#> 1               1.795488e-07               1.081544e-07
#> 2               7.881502e-07               4.032325e-07
#> 3               1.605603e-07               5.386976e-08
#> 4               2.402562e-08               1.441754e-07
#> 5               3.842274e-07               1.770510e-07
#> 6              -7.000751e-07              -2.405244e-07
#>   spatial_predictor_5000_140 spatial_predictor_5000_141
#> 1              -2.561619e-05              -1.151843e-06
#> 2               1.701480e-02               4.199936e-07
#> 3               5.157950e-02               4.159402e-06
#> 4               2.646383e-01               2.378051e-05
#> 5              -1.654311e-02              -5.824787e-06
#> 6              -1.925363e-01              -2.073715e-05
#>   spatial_predictor_5000_142 spatial_predictor_5000_143
#> 1              -0.0250681623              -1.032570e-06
#> 2              -0.0239134298              -7.981849e-07
#> 3              -0.0005448688              -1.519002e-07
#> 4               0.0058228219               7.083941e-07
#> 5               0.0304475265              -2.138103e-06
#> 6              -0.0263625378              -4.134434e-06
#>   spatial_predictor_5000_144 spatial_predictor_5000_145
#> 1               -0.005186519                0.017987946
#> 2               -0.005441172                0.027627844
#> 3                0.006998568                0.012201824
#> 4                0.043108224                0.047862815
#> 5               -0.037339729                0.009068995
#> 6               -0.056500135                0.021532186
#>   spatial_predictor_5000_146 spatial_predictor_5000_147
#> 1                -0.04023358                 0.02908321
#> 2                -0.05478938                 0.04600401
#> 3                -0.03148226                 0.02335330
#> 4                -0.09737349                 0.07768028
#> 5                -0.05086418                 0.03056383
#> 6                -0.05184051                 0.03150556
#>   spatial_predictor_5000_148 spatial_predictor_5000_149
#> 1               2.247597e-06               2.978670e-07
#> 2              -5.905949e-06              -2.588625e-06
#> 3               2.006386e-06               5.089568e-07
#> 4               7.200028e-06               2.457451e-06
#> 5               2.899464e-06               7.882143e-07
#> 6               3.139145e-06               4.855015e-07
#>   spatial_predictor_5000_150 spatial_predictor_5000_151
#> 1               3.363379e-06                0.009457434
#> 2               1.121683e-06                0.003433385
#> 3               4.754405e-06                0.002529990
#> 4               2.215195e-05               -0.024389775
#> 5               3.731956e-06                0.033159041
#> 6               8.734180e-07                0.064963575
#>   spatial_predictor_5000_152 spatial_predictor_5000_153
#> 1               -0.006528381              -4.151262e-07
#> 2               -0.005982661              -1.315683e-06
#> 3               -0.002393207               9.189624e-08
#> 4               -0.001613411              -1.784545e-06
#> 5                0.013167085              -1.628301e-07
#> 6               -0.024117360               6.735365e-07
#>   spatial_predictor_5000_154 spatial_predictor_5000_155
#> 1              -1.767682e-06               -0.008372095
#> 2              -4.904244e-06               -0.008690985
#> 3              -5.577586e-07               -0.007500335
#> 4              -7.215429e-06               -0.044757256
#> 5               7.173701e-07               -0.006274424
#> 6               2.595622e-06               -0.008101605
#>   spatial_predictor_5000_156 spatial_predictor_5000_157
#> 1              -1.730990e-07               4.897052e-07
#> 2              -2.625473e-07              -2.416317e-07
#> 3              -5.049273e-07               5.240602e-07
#> 4              -2.321223e-06               1.608536e-06
#> 5              -4.089190e-07               5.544749e-07
#> 6              -6.705764e-07               7.733686e-07
#>   spatial_predictor_5000_158 spatial_predictor_5000_159
#> 1               -0.008381414               0.0003488504
#> 2               -0.005751376               0.0010326446
#> 3               -0.003866448               0.0004651584
#> 4               -0.016645028              -0.0090507930
#> 5               -0.014880659              -0.0040148937
#> 6                0.003203463               0.0008889841
#>   spatial_predictor_5000_160 spatial_predictor_5000_161
#> 1               4.205954e-05                0.010495049
#> 2               3.286039e-05                0.008433727
#> 3              -5.849845e-06               -0.001555914
#> 4              -1.324697e-04               -0.033223575
#> 5              -7.638297e-06               -0.002024381
#> 6               9.516000e-05                0.023295114
#>   spatial_predictor_5000_162 spatial_predictor_5000_163
#> 1               1.030651e-07                0.028331475
#> 2               1.944695e-06                0.418840279
#> 3               2.952161e-07                0.005023376
#> 4               9.970189e-07                0.022384190
#> 5              -3.135534e-08               -0.064425401
#> 6              -1.474469e-08               -0.014220179
#>   spatial_predictor_5000_164 spatial_predictor_5000_165
#> 1               -0.034343156               7.506771e-08
#> 2               -0.113828379               6.341120e-07
#> 3                0.032167622               4.486186e-08
#> 4                0.164511877              -8.052056e-08
#> 5                0.018797176              -1.225295e-07
#> 6               -0.006280768              -6.125511e-08
#>   spatial_predictor_5000_166 spatial_predictor_5000_167
#> 1               2.254128e-07               8.012492e-08
#> 2               1.345011e-06               3.237126e-07
#> 3               7.828253e-07              -2.058969e-08
#> 4              -1.696236e-06               1.582541e-07
#> 5               2.346066e-07              -2.440163e-08
#> 6              -2.796531e-07              -2.728751e-08
#>   spatial_predictor_5000_168 spatial_predictor_5000_169
#> 1              -2.294686e-07              -7.473783e-07
#> 2              -7.499518e-07              -2.573055e-06
#> 3               3.958992e-07              -6.093106e-07
#> 4              -1.096196e-06               3.237523e-06
#> 5               2.758318e-07              -5.437499e-08
#> 6              -8.782567e-08               4.481593e-07
#>   spatial_predictor_5000_170 spatial_predictor_5000_171
#> 1              -1.139482e-06              -1.754398e-07
#> 2              -1.572870e-06              -4.060717e-07
#> 3               8.843519e-07               3.400372e-08
#> 4               6.493165e-06               5.162414e-07
#> 5              -1.395927e-07               1.860553e-07
#> 6               2.915132e-07              -3.431686e-08
#>   spatial_predictor_5000_172 spatial_predictor_5000_173
#> 1              -0.0035083312              -4.994961e-07
#> 2              -0.0053965931              -2.699704e-06
#> 3               0.0014781631               3.890110e-07
#> 4               0.0162738395               1.232510e-06
#> 5               0.0004956037              -2.431631e-06
#> 6               0.0009098357              -4.038684e-08
#>   spatial_predictor_5000_174 spatial_predictor_5000_175
#> 1               -0.020847780               5.103058e-05
#> 2               -0.022530856               8.916436e-03
#> 3               -0.009627071               1.007134e-03
#> 4               -0.002411748               3.264790e-03
#> 5               -0.008954201               1.281553e-02
#> 6                0.006900835              -1.523966e-03
#>   spatial_predictor_5000_176 spatial_predictor_5000_177
#> 1                0.024103582                 0.04168831
#> 2                0.172783760                 0.16261054
#> 3               -0.002888844                 0.01802806
#> 4               -0.127449088                 0.38422830
#> 5                0.157726242                 0.14100843
#> 6               -0.018544849                -0.06974480
#>   spatial_predictor_5000_178 spatial_predictor_5000_179
#> 1                -0.16294900               -0.019557260
#> 2                 0.04681022                0.009430937
#> 3                -0.05913552               -0.002457816
#> 4                 0.47502813               -0.038615950
#> 5                 0.06959327                0.164252925
#> 6                 0.06211025                0.001495432
#>   spatial_predictor_5000_180 spatial_predictor_5000_181
#> 1                 0.14503141               -0.114021218
#> 2                -0.15630342               -0.133594058
#> 3                -0.02919124                0.009795877
#> 4                 0.23223721               -0.093197835
#> 5                -0.14128652                0.050607657
#> 6                 0.01437817                0.029723278
#>   spatial_predictor_5000_182 spatial_predictor_5000_183
#> 1                0.062419252                -0.07636088
#> 2               -0.058314411                 0.04535431
#> 3                0.007685995                -0.05324818
#> 4               -0.022027307                -0.11129619
#> 5                0.099585850                -0.02493129
#> 6               -0.015213041                -0.04214927
#>   spatial_predictor_5000_184 spatial_predictor_5000_185
#> 1                -0.19114449               -0.036575669
#> 2                -0.18038876               -0.054603254
#> 3                -0.01319985                0.005014893
#> 4                 0.15950252               -0.272786812
#> 5                -0.08242279               -0.052505937
#> 6                 0.04937164                0.019290998
#>   spatial_predictor_5000_186 spatial_predictor_5000_187
#> 1                 0.03187410                0.085087803
#> 2                 0.02252903               -0.044920223
#> 3                 0.01890684                0.008404729
#> 4                 0.03838228                0.211867203
#> 5                 0.06546382                0.022215221
#> 6                -0.05763620                0.010754958
#>   spatial_predictor_5000_188 spatial_predictor_5000_189
#> 1                -0.06573909               -0.054413731
#> 2                -0.02756360                0.146797570
#> 3                 0.03023579               -0.002453213
#> 4                 0.06897483                0.046107235
#> 5                -0.03830668                0.048742631
#> 6                -0.09098417               -0.030041887
#>   spatial_predictor_5000_190 spatial_predictor_5000_191
#> 1                 0.04129995               -0.051990766
#> 2                 0.02044926                0.136169001
#> 3                -0.04538298               -0.029793709
#> 4                 0.02636926                0.063211918
#> 5                -0.02470745               -0.090061844
#> 6                 0.10762107               -0.001930698
#>   spatial_predictor_5000_192 spatial_predictor_5000_193
#> 1                0.006128910                -0.02319155
#> 2                0.082836837                -0.09321701
#> 3               -0.004003243                 0.03246623
#> 4                0.008593111                 0.02896728
#> 5               -0.038967491                -0.02301189
#> 6                0.019494778                 0.02781235
#>   spatial_predictor_5000_194 spatial_predictor_5000_195
#> 1               -0.029126151               -0.049256747
#> 2                0.373777292               -0.055572452
#> 3               -0.000892558                0.008636422
#> 4               -0.003790401                0.006771184
#> 5               -0.027442550               -0.009960566
#> 6               -0.005105409               -0.084452969
#>   spatial_predictor_5000_196 spatial_predictor_5000_197
#> 1               -0.004988435               -0.021959151
#> 2                0.057771108               -0.079348868
#> 3               -0.007302956               -0.001516329
#> 4               -0.005621207               -0.146140201
#> 5                0.057116923               -0.046844718
#> 6               -0.009117143               -0.053811975
#>   spatial_predictor_5000_198 spatial_predictor_5000_199
#> 1               -0.009338907                -0.03353355
#> 2                0.155318949                 0.10193113
#> 3               -0.020013330                 0.08516081
#> 4                0.007485527                 0.12690446
#> 5                0.017266476                -0.05427252
#> 6                0.018986674                 0.01872612
#>   spatial_predictor_5000_200 spatial_predictor_5000_201
#> 1               -0.006278907                -0.02282181
#> 2                0.022570290                 0.08042194
#> 3               -0.022926830                 0.01394668
#> 4                0.142448768                -0.03670245
#> 5                0.033040630                 0.06351636
#> 6               -0.044005006                -0.01743215
#>   spatial_predictor_5000_202 spatial_predictor_5000_203
#> 1                0.081550427               -0.019753776
#> 2                0.076757478                0.011572329
#> 3               -0.011703378               -0.004836864
#> 4               -0.009702873                0.005365719
#> 5               -0.018129379               -0.011195417
#> 6                0.111844149                0.027378075
#>   spatial_predictor_5000_204 spatial_predictor_5000_205
#> 1               -0.088319811               0.0925269307
#> 2               -0.007216048               0.0060919935
#> 3                0.007183976              -0.0114514931
#> 4               -0.009852391              -0.0009452732
#> 5               -0.014345570               0.0027470358
#> 6                0.007689879              -0.0374848903
#>   spatial_predictor_5000_206 spatial_predictor_5000_207
#> 1                0.108242270               -0.005035036
#> 2                0.001839404                0.073996840
#> 3                0.011577212                0.023274497
#> 4               -0.014235516               -0.147920937
#> 5                0.019057038               -0.028503539
#> 6               -0.024331060               -0.014328685
#>   spatial_predictor_5000_208 spatial_predictor_5000_209
#> 1                 0.02138211               -0.054587046
#> 2                -0.12483404                0.035295414
#> 3                -0.05885742                0.004347831
#> 4                 0.04846692                0.096940497
#> 5                 0.02035388                0.015358342
#> 6                -0.06378213                0.023552132
#>   spatial_predictor_5000_210 spatial_predictor_5000_211
#> 1               -0.076808548                0.047108532
#> 2               -0.006671801               -0.009859583
#> 3               -0.031997744               -0.018897916
#> 4               -0.039115998                0.004758935
#> 5                0.021901406               -0.019979114
#> 6               -0.012323448                0.028321609
#>   spatial_predictor_5000_212 spatial_predictor_5000_213
#> 1               -0.041762304                0.025386723
#> 2                0.001184453               -0.041040469
#> 3               -0.003883662                0.014414037
#> 4               -0.051070923                0.003434082
#> 5                0.001814918               -0.003348873
#> 6                0.097616816                0.016515262
#>   spatial_predictor_5000_214 spatial_predictor_5000_215
#> 1               -0.035129096              -0.0589040455
#> 2                0.028177285               0.0091253357
#> 3                0.007964104              -0.0144448193
#> 4                0.024571549               0.0016409168
#> 5               -0.036377269              -0.0003738633
#> 6                0.003750415               0.0043103051
#>   spatial_predictor_5000_216 spatial_predictor_5000_217
#> 1                0.022234606               0.0239127260
#> 2               -0.026721605               0.0306571706
#> 3                0.005400601              -0.0144671221
#> 4                0.023411482              -0.0004589649
#> 5                0.005622125               0.0059677298
#> 6                0.031366385              -0.0259216419
#>   spatial_predictor_5000_218 spatial_predictor_5000_219
#> 1              -0.0327988292                0.030953290
#> 2              -0.0005431365               -0.020619518
#> 3               0.0115663171                0.007293985
#> 4              -0.0044959194               -0.003384949
#> 5               0.0181580875                0.018613763
#> 6              -0.0077981471               -0.012432384
#>   spatial_predictor_5000_220 spatial_predictor_5000_221
#> 1                0.010832936               -0.005718144
#> 2               -0.005417424               -0.015967523
#> 3                0.003271617                0.008493311
#> 4               -0.006052268                0.004601619
#> 5               -0.008593565               -0.003601343
#> 6                0.007153891               -0.008000411
#>   spatial_predictor_5000_222 spatial_predictor_5000_223
#> 1                0.005335864               -0.010323164
#> 2                0.003960862                0.015730650
#> 3                0.009761345               -0.005999356
#> 4                0.006782381               -0.002070305
#> 5                0.012549458                0.016887350
#> 6               -0.007366593               -0.006724990
#>   spatial_predictor_5000_224 spatial_predictor_5000_225
#> 1               0.0059831396               -0.012207163
#> 2              -0.0034493366               -0.003164900
#> 3               0.0009838373               -0.001478996
#> 4               0.0007328721                0.004616122
#> 5               0.0120863364               -0.004352694
#> 6              -0.0071464436               -0.005049954
#>   spatial_predictor_5000_226
#> 1               0.0024229895
#> 2               0.0009577276
#> 3              -0.0004845812
#> 4               0.0011629640
#> 5              -0.0012055527
#> 6               0.0012598813
dim(pca_predictors)
#> [1] 227 678

# Check predictor names (show scale information)
colnames(pca_predictors)[1:6]
#> [1] "spatial_predictor_0_1" "spatial_predictor_0_2" "spatial_predictor_0_3"
#> [4] "spatial_predictor_0_4" "spatial_predictor_0_5" "spatial_predictor_0_6"

# Limit number of predictors to save memory
pca_limited <- pca_multithreshold(
  distance.matrix = plants_distance,
  distance.thresholds = c(0, 1000, 5000),
  max.spatial.predictors = 20
)
ncol(pca_limited)  # At most 20 predictors
#> [1] 20
```
