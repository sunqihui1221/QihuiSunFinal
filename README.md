
<!-- README.md is generated from README.Rmd. Please edit that file -->

# QihuiSunFinal

<!-- badges: start -->

<!-- badges: end -->

The goal of QihuiSunFinal is to check statistics of pokemons, including
weigt, height and strength.

## Installation

``` r
devtools::install_github("sunqihui1221/MyPackage")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(QihuiSunFinal)
#> Loading required package: httr
#> Loading required package: dplyr
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
#> Loading required package: tidyverse
#> ── Attaching packages ───────────────────────────────────────────────────────────────────────────────────── tidyverse 1.3.0 ──
#> ✓ ggplot2 3.2.1     ✓ purrr   0.3.3
#> ✓ tibble  2.1.3     ✓ stringr 1.4.0
#> ✓ tidyr   1.0.0     ✓ forcats 0.4.0
#> ✓ readr   1.3.1
#> ── Conflicts ──────────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
#> x dplyr::filter() masks stats::filter()
#> x dplyr::lag()    masks stats::lag()
#> Loading required package: jsonlite
#> 
#> Attaching package: 'jsonlite'
#> The following object is masked from 'package:purrr':
#> 
#>     flatten
#> Loading required package: xml2
#> Loading required package: rvest
#> 
#> Attaching package: 'rvest'
#> The following object is masked from 'package:purrr':
#> 
#>     pluck
#> The following object is masked from 'package:readr':
#> 
#>     guess_encoding
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like
so:

``` r
# Function 1, Show statistics of pokemons, including weight, height and strength.
pokemon_data() 
#> # A tibble: 787 x 10
#>    Pokemon_Name Weight Height    HP Attack Defense `Sp. Atk` `Sp. Def`
#>    <chr>         <int>  <int> <int>  <int>   <int>     <int>     <int>
#>  1 arceus         3200     32   120    120     120       120       120
#>  2 mewtwo         1220     20   106    110      90       154        90
#>  3 lugia          2160     52   106     90     130        90       154
#>  4 ho-oh          1990     38   106    130      90       110       154
#>  5 rayquaza       2065     70   105    150      90       150        90
#>  6 dialga         6830     54   100    120     120       150       100
#>  7 palkia         3360     42    90    120     100       150       120
#>  8 reshiram       3300     32   100    120     100       150       120
#>  9 zekrom         3450     29   100    150     120       120       100
#> 10 xerneas        2150     30   126    131      95       131        98
#> # … with 777 more rows, and 2 more variables: Speed <int>,
#> #   Total_Ability <int>
```

``` r
# Function 2, Show chosen statistics of pokemons, "Weight" statistics for pokemons
pokemon_data_partial("Weight")
#>     df$Pokemon_Name Weight
#> 1            arceus   3200
#> 2            mewtwo   1220
#> 3             lugia   2160
#> 4             ho-oh   1990
#> 5          rayquaza   2065
#> 6            dialga   6830
#> 7            palkia   3360
#> 8          reshiram   3300
#> 9            zekrom   3450
#> 10          xerneas   2150
#> 11          yveltal   2030
#> 12         solgaleo   2300
#> 13           lunala   1200
#> 14          slaking   1305
#> 15           kyogre   3520
#> 16          groudon   9500
#> 17        regigigas   4200
#> 18           kyurem   3250
#> 19        dragonite   2100
#> 20              mew     40
#> 21        tyranitar   2020
#> 22           celebi     50
#> 23        salamence   1026
#> 24        metagross   5500
#> 25           latias    400
#> 26           latios    600
#> 27          jirachi     11
#> 28         garchomp    950
#> 29          heatran   4300
#> 30        cresselia    856
#> 31          manaphy     14
#> 32          darkrai    505
#> 33          victini     40
#> 34        hydreigon   1600
#> 35         genesect    825
#> 36           goodra   1505
#> 37          zygarde   3050
#> 38          diancie     88
#> 39            hoopa     90
#> 40        volcanion   1950
#> 41          kommo-o    782
#> 42         necrozma   2300
#> 43         magearna    805
#> 44        marshadow    222
#> 45          zeraora    445
#> 46         articuno    554
#> 47           zapdos    526
#> 48          moltres    600
#> 49           raikou   1780
#> 50            entei   1980
#> 51          suicune   1870
#> 52         regirock   2300
#> 53           regice   1750
#> 54        registeel   2050
#> 55             uxie      3
#> 56          mesprit      3
#> 57            azelf      3
#> 58         cobalion   2500
#> 59        terrakion   2600
#> 60         virizion   2000
#> 61         silvally   1005
#> 62        tapu-koko    205
#> 63        tapu-lele    186
#> 64        tapu-bulu    455
#> 65        tapu-fini    212
#> 66         nihilego    555
#> 67         buzzwole   3336
#> 68        pheromosa    250
#> 69        xurkitree   1000
#> 70       celesteela   9999
#> 71          kartana      1
#> 72         guzzlord   8880
#> 73        stakataka   8200
#> 74      blacephalon    130
#> 75         archeops    320
#> 76         arcanine   1550
#> 77          florges    100
#> 78        volcarona    460
#> 79         togekiss    380
#> 80         gyarados   2350
#> 81          snorlax   4600
#> 82          kingdra   1520
#> 83          blissey    468
#> 84          milotic   1620
#> 85       electivire   1386
#> 86        magmortar    680
#> 87          haxorus   1055
#> 88        naganadel   1500
#> 89           lapras   2200
#> 90           crobat    750
#> 91         swampert    819
#> 92        magnezone   1800
#> 93        rhyperior   2828
#> 94        tangrowth   1286
#> 95        porygon-z    340
#> 96        vanilluxe    575
#> 97          noivern    850
#> 98        charizard    905
#> 99       typhlosion    795
#> 100       infernape    550
#> 101         delphox    390
#> 102       type-null   1205
#> 103          gogoat    910
#> 104       blastoise    855
#> 105       exeggutor   1200
#> 106      feraligatr    888
#> 107        sceptile    522
#> 108        blaziken    520
#> 109          aggron   3600
#> 110         walrein   1506
#> 111        empoleon    845
#> 112       mamoswine   2910
#> 113      chesnaught    900
#> 114        greninja    400
#> 115       decidueye    366
#> 116      incineroar    830
#> 117       primarina    440
#> 118       golisopod   1080
#> 119       serperior    630
#> 120          emboar   1500
#> 121        samurott    946
#> 122        venusaur   1000
#> 123        cloyster   1325
#> 124        vaporeon    290
#> 125         jolteon    245
#> 126         flareon    250
#> 127        meganium   1005
#> 128          espeon    265
#> 129         umbreon    270
#> 130        torterra   3100
#> 131         lucario    540
#> 132       hippowdon   3000
#> 133         leafeon    255
#> 134         glaceon    259
#> 135       probopass   3400
#> 136        dusknoir   1066
#> 137         sylveon    235
#> 138          luxray    420
#> 139       tyrantrum   2700
#> 140         aurorus   2250
#> 141         starmie    800
#> 142          flygon    820
#> 143       klinklang    810
#> 144      chandelure    343
#> 145      krookodile    963
#> 146       gardevoir    484
#> 147         gallade    520
#> 148        dhelmise   2100
#> 149      tentacruel    550
#> 150      aerodactyl    590
#> 151        porygon2    325
#> 152        roserade    145
#> 153      lickilicky   1400
#> 154         yanmega    515
#> 155        gigalith   2600
#> 156      eelektross    805
#> 157       cryogonal   1480
#> 158         avalugg   5050
#> 159       poliwrath    540
#> 160        ampharos    615
#> 161         steelix   4000
#> 162         weavile    340
#> 163         gliscor    425
#> 164         zoroark    811
#> 165        mienshao    355
#> 166        braviary    410
#> 167       mandibuzz    395
#> 168        tsareena    214
#> 169      seismitoad    620
#> 170       excadrill    404
#> 171          pyroar    815
#> 172       nidoqueen    600
#> 173        nidoking    620
#> 174       ninetales    199
#> 175         machamp   1300
#> 176         shuckle    205
#> 177       honchkrow    273
#> 178      conkeldurr    870
#> 179         beartic   2600
#> 180         golduck    766
#> 181        alakazam    480
#> 182        rapidash    950
#> 183             muk    300
#> 184          gengar    405
#> 185         scyther    560
#> 186          pinsir    550
#> 187        politoed    339
#> 188          scizor   1180
#> 189       heracross    540
#> 190        ursaring   1258
#> 191        houndoom    350
#> 192         donphan   1200
#> 193         wailord   3980
#> 194         claydol   1080
#> 195        bronzong   1870
#> 196         drapion    615
#> 197       stoutland    610
#> 198        leavanny    205
#> 199      barbaracle    960
#> 200       clawitzer    353
#> 201        hawlucha    215
#> 202         carbink     57
#> 203        vikavolt    450
#> 204        mudsdale   9200
#> 205          bewear   1350
#> 206      talonflame    245
#> 207        drifblim    150
#> 208        simisage    305
#> 209        simisear    280
#> 210        simipour    290
#> 211       zebstrika    795
#> 212           golem   3000
#> 213          magmar    445
#> 214         omastar    350
#> 215        kabutops    405
#> 216         cradily    604
#> 217         armaldo    682
#> 218       rampardos   1025
#> 219       bastiodon   1495
#> 220        floatzel    335
#> 221       mismagius     44
#> 222      carracosta    810
#> 223      escavalier    330
#> 224        accelgor    253
#> 225         pangoro   1360
#> 226         toxapex    145
#> 227       abomasnow   1355
#> 228        dragalge    815
#> 229       vileplume    186
#> 230      victreebel    155
#> 231         slowbro    785
#> 232       electrode    666
#> 233         weezing     95
#> 234      kangaskhan    800
#> 235      electabuzz    300
#> 236          tauros    884
#> 237       bellossom     58
#> 238        slowking    795
#> 239         miltank    755
#> 240         exploud    840
#> 241         altaria    206
#> 242       toxicroak    444
#> 243        sigilyph    140
#> 244      gothitelle    440
#> 245       reuniclus    201
#> 246         bisharp    700
#> 247      bouffalant    946
#> 248        oranguru    760
#> 249       passimian    828
#> 250      ferrothorn   1100
#> 251        unfezant    290
#> 252         scrafty    300
#> 253        musharna    605
#> 254          raichu    300
#> 255          rhydon   1200
#> 256         mantine   2200
#> 257         huntail    270
#> 258        gorebyss    226
#> 259       relicanth    234
#> 260       staraptor    249
#> 261       spiritomb   1080
#> 262       scolipede   2005
#> 263         crustle   2000
#> 264        beheeyem    345
#> 265       druddigon   1390
#> 266       toucannon    260
#> 267          comfey      3
#> 268      turtonator   2120
#> 269          drampa   1850
#> 270         heatmor    580
#> 271          durant    330
#> 272        clefable    400
#> 273           hypno    756
#> 274      cofagrigus    765
#> 275          golurk   3300
#> 276         ambipom    203
#> 277         malamar    470
#> 278       heliolisk    210
#> 279       octillery    285
#> 280        ludicolo    550
#> 281         shiftry    596
#> 282          glalie   2565
#> 283         lopunny    333
#> 284        froslass    266
#> 285          phione     31
#> 286      whimsicott     66
#> 287       lilligant    163
#> 288       jellicent   1350
#> 289        slurpuff     50
#> 290        lurantis    185
#> 291        salazzle    222
#> 292       palossand   2500
#> 293          komala    199
#> 294         pidgeot    395
#> 295        skuntank    380
#> 296    crabominable   1800
#> 297         dewgong   1200
#> 298         kingler    600
#> 299       manectric    402
#> 300        cacturne    774
#> 301       gastrodon    299
#> 302        sawsbuck    925
#> 303         bruxish    190
#> 304        hariyama   2538
#> 305       vespiquen    385
#> 306        garbodor   1073
#> 307       trevenant    710
#> 308          swanna    242
#> 309      galvantula    143
#> 310         furfrou    280
#> 311        stunfisk    110
#> 312          dodrio    852
#> 313            xatu    150
#> 314         torkoal    804
#> 315         grumpig    715
#> 316        cinccino     75
#> 317       alomomola    316
#> 318          klefki     30
#> 319        whiscash    236
#> 320       crawdaunt    328
#> 321          swalot    800
#> 322        magneton    600
#> 323      forretress   1258
#> 324        skarmory    505
#> 325        stantler    712
#> 326           absol    470
#> 327           throh    555
#> 328            sawk    510
#> 329       amoonguss    105
#> 330        ribombee      5
#> 331      aromatisse    155
#> 332        maractus    280
#> 333         mr-mime    545
#> 334         lanturn    225
#> 335        jumpluff     30
#> 336         breloom    392
#> 337        sharpedo    888
#> 338        camerupt   2200
#> 339        lunatone   1680
#> 340         solrock   1540
#> 341         tropius   1000
#> 342        lumineon    240
#> 343        zangoose    403
#> 344         seviper    525
#> 345         ninjask    120
#> 346          golbat    550
#> 347        primeape    320
#> 348       hitmonlee    498
#> 349      hitmonchan    502
#> 350            jynx    406
#> 351       girafarig    415
#> 352       hitmontop    480
#> 353         swellow    198
#> 354         banette    125
#> 355        dusclops    306
#> 356        chimecho     10
#> 357      masquerain     36
#> 358       carnivine    270
#> 359       araquanid    820
#> 360         noctowl    408
#> 361         purugly    438
#> 362         sliggoo    175
#> 363       sandslash    295
#> 364        venomoth    125
#> 365         chansey    346
#> 366         seaking    390
#> 367        granbull    487
#> 368       piloswine    558
#> 369         cherrim     93
#> 370           arbok    650
#> 371        doublade     45
#> 372         liepard    375
#> 373          audino    310
#> 374          fearow    380
#> 375         persian    320
#> 376          seadra    250
#> 377        qwilfish     39
#> 378        pelipper    280
#> 379        vigoroth    465
#> 380         kecleon    220
#> 381           rotom      3
#> 382           klang    510
#> 383      wigglytuff    120
#> 384         tangela    350
#> 385      misdreavus     10
#> 386      togedemaru     33
#> 387         dedenne     22
#> 388        quagsire    750
#> 389          gligar    648
#> 390         sneasel    280
#> 391        magcargo    550
#> 392          lairon   1200
#> 393         volbeat    177
#> 394        illumise    177
#> 395          emolga     50
#> 396         dugtrio    333
#> 397         marowak    450
#> 398        sunflora     85
#> 399         swoobat    105
#> 400          mothim    233
#> 401       diggersby    424
#> 402       dragonair    165
#> 403       azumarill    285
#> 404       mightyena    370
#> 405         linoone    325
#> 406        castform      8
#> 407         shelgon   1105
#> 408          metang   2025
#> 409         watchog    270
#> 410        zweilous    500
#> 411         dartrix    160
#> 412        torracat    250
#> 413         brionne    175
#> 414        hakamo-o    470
#> 415         poipole     18
#> 416         pignite    555
#> 417        gumshoos    142
#> 418          furret    325
#> 419       dunsparce    140
#> 420        raticate    185
#> 421         servine    160
#> 422          dewott    245
#> 423          chatot     19
#> 424        vivillon    170
#> 425          ponyta    300
#> 426       sudowoodo    380
#> 427         corsola     50
#> 428         pupitar   1520
#> 429        medicham    315
#> 430          sealeo    876
#> 431         bibarel    315
#> 432          gabite    560
#> 433         fraxure    360
#> 434       pyukumuku     12
#> 435         braixen    145
#> 436         ivysaur    130
#> 437      charmeleon    190
#> 438       wartortle    225
#> 439        parasect    295
#> 440         machoke    705
#> 441         haunter      1
#> 442         bayleef    158
#> 443         quilava    190
#> 444        croconaw    250
#> 445         togetic     32
#> 446         murkrow     21
#> 447       wobbuffet    285
#> 448         grovyle    216
#> 449       combusken    195
#> 450       marshtomp    280
#> 451          plusle     42
#> 452           minun     42
#> 453          grotle    970
#> 454        monferno    220
#> 455        prinplup    230
#> 456       pachirisu     39
#> 457         gurdurr    400
#> 458       eelektrik    220
#> 459       quilladin    290
#> 460       frogadier    109
#> 461       shiinotic    115
#> 462          archen     95
#> 463         kadabra    565
#> 464         ariados    335
#> 465        delcatty    326
#> 466         roselia     20
#> 467         wailmer   1300
#> 468       charjabug    105
#> 469         cosmoem   9999
#> 470      butterfree    320
#> 471        beedrill    295
#> 472           gloom     86
#> 473         porygon    365
#> 474       beautifly    284
#> 475       vanillish    410
#> 476      weepinbell     64
#> 477        graveler   1050
#> 478          ledian    356
#> 479           yanma    380
#> 480        munchlax   1050
#> 481         boldore   1020
#> 482       gothorita    180
#> 483       poliwhirl    200
#> 484            onix   2100
#> 485       lickitung    655
#> 486          dustox    316
#> 487         mudbray   1100
#> 488      kricketune    255
#> 489       palpitoad    170
#> 490     fletchinder    160
#> 491         sableye    110
#> 492          mawile    115
#> 493        swadloon     73
#> 494       farfetchd    150
#> 495        nosepass    970
#> 496         floette      9
#> 497         herdier    147
#> 498         duosion     80
#> 499         lampent    130
#> 500         vullaby     90
#> 501          litleo    135
#> 502        nidorina    200
#> 503        nidorino    195
#> 504         flaaffy    133
#> 505           magby    214
#> 506           luxio    305
#> 507          tyrunt    260
#> 508          amaura    252
#> 509           aipom    115
#> 510          elekid    235
#> 511         loudred    405
#> 512          spinda     50
#> 513      whirlipede    585
#> 514        larvesta    288
#> 515       tranquill    150
#> 516         omanyte     75
#> 517          kabuto    115
#> 518          lileep    238
#> 519         anorith    125
#> 520        tirtouga    165
#> 521          espurr     35
#> 522        trumbeak    148
#> 523        krokorok    334
#> 524       growlithe    190
#> 525        cranidos    315
#> 526        shieldon    570
#> 527         buneary     55
#> 528         mienfoo    200
#> 529         rufflet    105
#> 530          skiddo    310
#> 531       pidgeotto    300
#> 532        drifloon     12
#> 533         scraggy    118
#> 534         pancham     80
#> 535         rhyhorn   1150
#> 536        clamperl    525
#> 537         mantyke    650
#> 538        spritzee      5
#> 539         swirlix     35
#> 540         koffing     10
#> 541          staryu    345
#> 542        skiploom     10
#> 543          lombre    325
#> 544         nuzleaf    280
#> 545         vibrava    153
#> 546        staravia    155
#> 547        pawniard    102
#> 548         stufful     68
#> 549      crabrawler     70
#> 550           unown     50
#> 551       tentacool    455
#> 552          cacnea    513
#> 553        deerling    195
#> 554        frillish    330
#> 555          elgyem     90
#> 556          snover    505
#> 557         voltorb    104
#> 558        chinchou    120
#> 559       teddiursa     88
#> 560        delibird    160
#> 561        houndour    108
#> 562          phanpy    335
#> 563            aron    600
#> 564          spoink    306
#> 565         luvdisc     87
#> 566          buizel    295
#> 567      hippopotas    495
#> 568         skorupi    120
#> 569         finneon     70
#> 570           zorua    125
#> 571       clauncher     83
#> 572          stunky    192
#> 573        trubbish    310
#> 574         drowzee    324
#> 575         drilbur     85
#> 576       magnemite     60
#> 577            seel    900
#> 578          grimer    300
#> 579          krabby     65
#> 580       exeggcute     25
#> 581           eevee     65
#> 582         shellos     63
#> 583         dwebble    145
#> 584         honedge     20
#> 585        clefairy     75
#> 586          woobat     21
#> 587         pikachu     60
#> 588          oddish     54
#> 589         psyduck    196
#> 590          cubone     65
#> 591         goldeen    150
#> 592            natu     20
#> 593            axew    180
#> 594          skrelp     73
#> 595          rowlet     15
#> 596          litten     43
#> 597         popplio     75
#> 598        salandit     48
#> 599       sandygast    700
#> 600          joltik      6
#> 601       bulbasaur     69
#> 602       chikorita     64
#> 603         turtwig    102
#> 604         pansage    105
#> 605         pansear    110
#> 606         panpour    135
#> 607        slowpoke    360
#> 608        darumaka    375
#> 609      karrablast     59
#> 610        squirtle     90
#> 611        totodile     95
#> 612          piplup     52
#> 613         froakie     70
#> 614         chespin     90
#> 615            abra    195
#> 616           doduo    392
#> 617          gastly      1
#> 618         treecko     50
#> 619         torchic     25
#> 620          mudkip     76
#> 621          swablu     12
#> 622         glameow     39
#> 623         mime-jr    130
#> 624        sewaddle     25
#> 625      charmander     85
#> 626       cyndaquil     79
#> 627        chimchar     62
#> 628        phantump     70
#> 629        corphish    115
#> 630           snivy     81
#> 631           tepig     99
#> 632        oshawott     59
#> 633        fennekin     94
#> 634         binacle    310
#> 635         venonat    300
#> 636          mankey    280
#> 637          machop    195
#> 638        shellder     40
#> 639        smoochum     60
#> 640        carvanha    208
#> 641           numel    240
#> 642         timburr    125
#> 643        ducklett     55
#> 644       vanillite     57
#> 645       ferroseed    188
#> 646         cubchoo     85
#> 647         shelmet     77
#> 648        mareanie     80
#> 649        bergmite    995
#> 650        cutiefly      2
#> 651          yamask     15
#> 652          golett    920
#> 653         flabebe      1
#> 654          gulpin    103
#> 655       sandshrew    120
#> 656         poliwag    124
#> 657      bellsprout     40
#> 658         geodude    200
#> 659         dratini     33
#> 660        snubbull     78
#> 661        remoraid    120
#> 662        larvitar    720
#> 663          baltoy    215
#> 664         snorunt    168
#> 665           bagon    421
#> 666          beldum    952
#> 667         bronzor    605
#> 668           gible    205
#> 669        croagunk    230
#> 670        minccino     58
#> 671           klink    210
#> 672           deino    173
#> 673           goomy     28
#> 674         grubbin     44
#> 675        jangmo-o    297
#> 676          vulpix     99
#> 677          horsea     80
#> 678       shroomish     45
#> 679       electrike    152
#> 680         shuppet     23
#> 681         duskull    150
#> 682         blitzle    298
#> 683         tympole     45
#> 684         foongus     10
#> 685           munna    233
#> 686         sandile    152
#> 687          meowth     42
#> 688          pineco     72
#> 689        trapinch    150
#> 690          spheal    395
#> 691          bonsly    150
#> 692         gothita     58
#> 693         solosis     10
#> 694         steenee     82
#> 695      helioptile     60
#> 696           ekans     69
#> 697           ditto     40
#> 698        barboach     19
#> 699           inkay     35
#> 700           paras     54
#> 701       chingling      6
#> 702           riolu    202
#> 703        morelull     15
#> 704        purrloin    101
#> 705          mareep     78
#> 706         slakoth    240
#> 707        meditite    112
#> 708           budew     12
#> 709      roggenrola    180
#> 710        cottonee      6
#> 711         petilil     66
#> 712        rockruff     92
#> 713          kirlia    202
#> 714      fletchling     17
#> 715       nidoran-f     70
#> 716         cherubi     33
#> 717        lillipup     41
#> 718          tynamo      3
#> 719         litwick     31
#> 720       nidoran-m     90
#> 721      jigglypuff     55
#> 722         taillow     23
#> 723         wingull     95
#> 724         surskit     17
#> 725        dewpider     40
#> 726         nincada     55
#> 727         diglett      8
#> 728          ledyba    108
#> 729         pikipek     12
#> 730          pidove     21
#> 731           shinx     95
#> 732         spearow     20
#> 733        hoothoot    212
#> 734          skitty    110
#> 735          wynaut    140
#> 736        venipede     53
#> 737          patrat    116
#> 738         rattata     35
#> 739         yungoos     60
#> 740          pidgey     18
#> 741        spinarak     85
#> 742          marill     85
#> 743          hoppip      5
#> 744          slugma    350
#> 745          swinub     65
#> 746        smeargle    580
#> 747          bidoof    200
#> 748        fomantis     15
#> 749           zubat     75
#> 750          togepi     15
#> 751          starly     20
#> 752          noibat     80
#> 753          combee     55
#> 754       zigzagoon    175
#> 755         whismur    163
#> 756        makuhita    864
#> 757        bunnelby     50
#> 758        shedinja     12
#> 759          wimpod    120
#> 760           burmy     34
#> 761       poochyena    136
#> 762           lotad     26
#> 763          seedot     40
#> 764         happiny    244
#> 765          cleffa     30
#> 766         sentret     60
#> 767          spewpa     84
#> 768       igglybuff     10
#> 769          wooper     85
#> 770         tyrogue    210
#> 771       bounsweet     32
#> 772         metapod     99
#> 773          kakuna    100
#> 774           pichu     20
#> 775         silcoon    100
#> 776         cascoon    115
#> 777        magikarp    100
#> 778          feebas     74
#> 779      scatterbug     25
#> 780          cosmog      1
#> 781           ralts     66
#> 782        caterpie     29
#> 783          weedle     32
#> 784         wurmple     36
#> 785       kricketot     22
#> 786         azurill     20
#> 787         sunkern     18
```

``` r
# Function3, Show summary statistics of pokemons, for example, "Weight"
summary_pokemon_data_partial("Weight", TRUE)
#>      Weight      
#>  Min.   :   1.0  
#>  1st Qu.:  91.0  
#>  Median : 273.0  
#>  Mean   : 616.6  
#>  3rd Qu.: 649.0  
#>  Max.   :9999.0
```
