module Csound.Catalog.Wave.Woodwind
    ( flute
    , bassClarinet
    , frenchHorn
    , sheng
    , hulusi
    , dizi
    ) where


import Csound.Base hiding (fromSpec)

import Csound.Catalog.Wave.WoodwindAlg

totalDur :: (D, D, D) -> D
totalDur (att, sus, dec) = att + sus + dec

-------------------------------------------------------------------------
-- Flute

-- | An emulation of the flute. Parameters
--
-- > flute seed vibDepth attack sustain decay brightnessLevel cps = 
--
-- * seed - a seed for the random signals/numbers. It's in (0, 1)
--
-- * vibDepth -  Amount of the vibrato. It's in [-1, 1]
--
-- * attack - duration of the attack. 
--      Recommended value: 0.12 for slurred notes,
--                         0.06 for tongued notes, 
--                         0.03 for short notes. 
--
-- * sustain - duration of the sustain
--
-- * decay - duration of the decay.
--      Recommended value: 0.1 (0.05 for short notes).
--
-- * brightnessLevel - filter cutoff factor. It's in (0, 1). The 0 is 40 Hz, the 1 s 10240 Hz
--
-- * cps - frequency of the note
flute :: D -> D -> D -> D -> D -> D -> D -> Sig
flute = woodwind $ WoodwindSpec 
    { woodwindRange         = fromSpec fluteRangeSpec
    , woodwindVibratoDur    = totalDur
    , woodwindFreqDeviation = ((-0.03, 0), (0, 0.003), (-0.0015, 0), (0, 0.012)) 
    }

fluteRangeSpec :: [RangeSpec]
fluteRangeSpec = 
    [ RangeSpec 
        { rangeFreq = 427.28
        , rangeNorm = 3949
        , rangeHarms =  
            [ HarmSpec 
                { harmAmp   = AmpSpec 
                    { ampAttack     = [0,     0.002, 0.045, 0.146, 0.272, 0.072, 0.043]
                    , ampSustain    = [0.043, 0.230, 0.000, 0.118, 0.923]
                    , ampDecay      = [0.923, 1.191, 0.794, 0.418, 0.172, 0.053, 0]
                    }
                , harmWave  = [2000, 489, 74, 219, 125, 9, 33, 5, 5]
                }
            , HarmSpec 
                { harmAmp   = AmpSpec
                    { ampAttack     = [0,     0.009, 0.022, -0.049, -0.120, 0.297, 1.890]
                    , ampSustain    = [1.890, 1.543, 0.000, 0.546, 0.690]
                    , ampDecay      = [0.690, -0.318, -0.326, -0.116, -0.035, -0.020, 0]
                    }
                , harmWave  = [2729, 1926, 346, 662, 537, 110, 61, 29, 7]
                }
            , HarmSpec
                { harmAmp   = AmpSpec
                    { ampAttack     = [0,     0.005, -0.026, 0.023, 0.133, 0.060, -1.245]
                    , ampSustain    = [-1.245, -0.760, 1.000, 0.360, -0.526]
                    , ampDecay      = [-0.526, 0.165, 0.184, 0.060, 0.010, 0.013, 0]
                    }
                , harmWave  = [2558, 2012, 390, 361, 534, 139, 53, 22, 10, 13, 10]
                }
            ]
        }
    , RangeSpec
        { rangeFreq = 608.22
        , rangeNorm = 27668.2
        , rangeHarms = 
            [ HarmSpec 
                { harmAmp = AmpSpec
                    { ampAttack     = [0.000, 0.000, -0.005, 0.000, 0.030, 0.198, 0.664]
                    , ampSustain    = [0.664, 1.451, 1.782, 1.316, 0.817]
                    , ampDecay      = [0.817, 0.284, 0.171, 0.082, 0.037, 0.012, 0]
                    }
                , harmWave = [12318, 8844, 1841, 1636, 256, 150, 60, 46, 11]
                }
            , HarmSpec 
                { harmAmp   = AmpSpec
                    { ampAttack     = [0,0.000,0.320,0.882,1.863,4.175,4.355]
                    , ampSustain    = [4.355,-5.329,-8.303,-1.480,-0.472]
                    , ampDecay      = [-0.472,1.819,-0.135,-0.082,-0.170,-0.065,0]
                    }
                , harmWave = [1229, 16, 34, 57, 32]
            }
            , HarmSpec 
                { harmAmp = AmpSpec
                    { ampAttack     = [0,1.000,0.520,-0.303,0.059,-4.103,-6.784]
                    , ampSustain    = [-6.784,7.006,11,12.495,-0.562]
                    , ampDecay      = [-0.562,-4.946,-0.587,0.440,0.174,-0.027,0]
                    }
                , harmWave = [163, 31, 1, 50, 31]
                }
            ]
        }
    , RangeSpec 
        { rangeFreq = 1013.7
        , rangeNorm = 3775
        , rangeHarms = 
            [ HarmSpec 
                { harmAmp = AmpSpec
                    { ampAttack     = [0,0.005,0.000,-0.082,0.36,0.581,0.416]
                    , ampSustain    = [0.416,1.073,0.000,0.356,0.86]
                    , ampDecay      = [0.86,0.532,0.162,0.076,0.064,0.031,0]
                    }
                , harmWave = [4128, 883, 354, 79, 59, 23]
                }
            , HarmSpec 
                { harmAmp = AmpSpec
                    { ampAttack     = [0,-0.005,0.000,0.205,-0.284,-0.208,0.326]
                    , ampSustain    = [0.326,-0.401,1.540,0.589,-0.486]
                    , ampDecay      = [-0.486,-0.016,0.141,0.105,-0.003,-0.023,0]
                    }                
                , harmWave = [1924, 930, 251, 50, 25, 14]
                }
            , HarmSpec
                { harmAmp = AmpSpec
                    { ampAttack = [0,0.722,1.500,3.697,0.080,-2.327,-0.684]
                    , ampSustain = [-0.684,-2.638,0.000,1.347,0.485]
                    , ampDecay = [0.485,-0.419,-0.700,-0.278,0.167,-0.059,0]
                    }
                , harmWave = [94, 6, 22, 8]
                }
            ]
        }
    , RangeSpec 
        { rangeFreq = 22000
        , rangeNorm = 4909.05
        , rangeHarms = 
            [ HarmSpec
                { harmAmp = AmpSpec
                    { ampAttack     = [0,0.000,0.000,0.211,0.526,0.989,1.216]
                    , ampSustain    = [1.216,1.727,1.881,1.462,1.28]
                    , ampDecay      = [1.28,0.75,0.34,0.154,0.122,0.028,0]
                    }
                , harmWave = [2661, 87, 33, 18]
                }
            , HarmSpec
                { harmAmp = AmpSpec
                    { ampAttack     = [0,0.500,0.000,0.181,0.859,-0.205,-0.430]
                    , ampSustain    = [-0.430,-0.725,-0.544,-0.436,-0.109]
                    , ampDecay      = [-0.109,-0.03,-0.022,-0.046,-0.071,-0.019,0]
                    }                
                , harmWave = [174, 12]
                }
            , HarmSpec
                { harmAmp = AmpSpec
                    { ampAttack     = [0,0.000,1.000,0.426,0.222,0.175,-0.153]
                    , ampSustain    = [-0.153,0.355,0.175,0.16,-0.246]
                    , ampDecay      = [-0.246,-0.045,-0.072,0.057,-0.024,0.002,0]
                    }                
                , harmWave = [314, 13]
                }
            ]
        }
    ]

-----------------------------------------------------------------
-- Bass clarinet

-- | An emulation of the bass clarinet. Parameters
--
-- > bassClarinet seed vibDepth attack sustain decay brightnessLevel cps = 
--
-- * seed - a seed for the random signals/numbers. It's in (0, 1)
--
-- * vibDepth -  Amount of the vibrato. It's in [-1, 1]
--
-- * attack - duration of the attack. 
--      Recommended value: 0.06 for tongued notes, 
--                         0.03 for short notes. 
--
-- * sustain - duration of the sustain
--
-- * decay - duration of the decay.
--      Recommended value: 0.15 (0.04 for short notes).
--
-- * brightnessLevel - filter cutoff factor. It's in (0, 1). The 0 is 40 Hz, the 1 s 10240 Hz
--
-- * cps - frequency of the note
bassClarinet :: D -> D -> D -> D -> D -> D -> D -> Sig
bassClarinet = woodwind $ WoodwindSpec 
    { woodwindRange         = fromSpec bassClarinetRangeSpec
    , woodwindVibratoDur    = totalDur
    , woodwindFreqDeviation = ((0, 0.015), (-0.005, 0), (0, 0.003), (0, 0.017)) 
    }


bassClarinetRangeSpec :: [RangeSpec]
bassClarinetRangeSpec = 
    [RangeSpec
        { rangeFreq = 67.13
        , rangeNorm = 29786.7
        , rangeHarms = [HarmSpec
                           { harmAmp = AmpSpec
                               { ampAttack     = [0,-0.012,0.000,0.494,0.717,1.005,1.396]
                               , ampSustain    = [1.396,1.838,1.616,1.579,1.343]
                               , ampDecay      = [1.343,1.231,0.958,0.581,0.000,-0.089,0]
                               }
                           , harmWave = [5321,43,5458,112,7869,166,2338,211,5575,104,3280,127,1184,296,1290,742,419,238,248,481,605,585,619,691,93,242,762,826,831,830,440,62,286,310,409,200,422,101,170,51,177,44,114,93,27,26,48,80,98,61,69,45,31,13,7,11,33,0,38,21,11]
                           }
                       ,HarmSpec
                           { harmAmp = AmpSpec
                               { ampAttack     = [0,0.301,1.000,0.891,0.753,0.499,0.333]
                               , ampSustain    = [0.333,0.065,0.125,0.166,-0.002]
                               , ampDecay      = [-0.002,0.023,-0.018,0.029,0.000,0.389,0]
                               }
                           , harmWave = [2390,251,2895,266,1251,148,358,146,300,93,81,65,50,20,28,3,19,2,8,12,60,38,39,61,9,34,25,42,37,6]
                           }
                       ,HarmSpec
                           { harmAmp = AmpSpec
                               { ampAttack     = [0,-0.009,0.000,-0.166,-0.298,-0.446,-0.837]
                               , ampSustain    = [-0.837,-0.945,-0.644,-0.573,-0.341]
                               , ampDecay      = [-0.341,-0.254,0.047,0.405,1.000,0.486,0]
                               }
                           , harmWave = [4231,54,3544,101,4989,146,1459,122,3251,157,1310,147,779,6,1326,396,720,235,238,260,516,123,519,45,36,112,320,112,253,83,216,62,107,52,94,126,32,49,33,35,32,32,15,14]
                           }]
        }
    ,RangeSpec
        { rangeFreq = 95.56
        , rangeNorm = 23524.2
        , rangeHarms = [HarmSpec
                           { harmAmp = AmpSpec
                               { ampAttack     = [0,0.023,0.062,0.140,0.605,0.902,1.302]
                               , ampSustain    = [1.302,1.355,1.355,1.202,0.934]
                               , ampDecay      = [0.934,0.891,0.654,0.225,0.000,-0.012,0]
                               }
                           , harmWave = [6317,51,6402,115,4155,81,1130,67,609,372,1333,427,1708,490,1817,798,2287,553,1410,385,421,208,448,70,347,146,753,482,572,119,222,101,158,216,75,155,67,14,27,38,74,3,61,105,109,71,34,12,14,27,15,23,28,46,24,35,13,10,27,7,42,33,44,24,56,50,53,13]
                           }
                       ,HarmSpec
                           { harmAmp = AmpSpec
                               { ampAttack     = [0,0.035,0.230,0.637,0.410,0.279,-0.240]
                               , ampSustain    = [-0.240,-0.259,-0.248,-0.086,-0.090]
                               , ampDecay      = [-0.090,-0.109,-0.074,-0.037,0.000,0.016,0]
                               }
                           , harmWave = [3464,54,4267,457,84,217,49,29,21,48,38,27,16,21,65,34,224,461,312,102]
                           }
                       ,HarmSpec
                           { harmAmp = AmpSpec
                               { ampAttack     = [0,-0.070,-0.144,-0.167,-0.230,-0.350,-0.500]
                               , ampSustain    = [-0.500,-0.650,-0.435,-0.083,0.009]
                               , ampDecay      = [0.009,0.030,0.485,1.477,1.000,0.278,0]
                               }
                           , harmWave = [1516,179,890,59,566,60,257,17,94,39,172,44,40,41,77,10]
                           }]
        }
    ,RangeSpec
        { rangeFreq = 135.16
        , rangeNorm = 19174.7
        , rangeHarms = [HarmSpec
                           { harmAmp = AmpSpec
                               { ampAttack     = [0,0.023,0.062,0.140,0.605,0.902,1.302]
                               , ampSustain    = [1.302,1.355,1.355,1.202,0.934]
                               , ampDecay      = [0.934,0.891,0.654,0.225,0.000,-0.012,0]
                               }
                           , harmWave = [10317,51,3402,115,3855,81,2130,67,1009,872,1233,427,1208,490,1217,398,1187,153,710,285,261,28,278,10,277,26,153,92,82,9,22,11,28,26,15,25,7]
                           }
                       ,HarmSpec
                           { harmAmp = AmpSpec
                               { ampAttack     = [0,0.035,0.230,0.637,0.410,0.279,-0.240]
                               , ampSustain    = [-0.240,-0.259,-0.248,-0.086,-0.090]
                               , ampDecay      = [-0.090,-0.109,-0.074,-0.037,0.000,0.016,0]
                               }
                           , harmWave = [3464,54,2267,257,44,117,29,29,21]
                           }
                       ,HarmSpec
                           { harmAmp = AmpSpec
                               { ampAttack     = [0,-0.070,-0.144,-0.167,-0.230,-0.350,-0.500]
                               , ampSustain    = [-0.500,-0.650,-0.435,-0.083,0.009]
                               , ampDecay      = [0.009,0.030,0.485,1.477,1.000,0.278,0]
                               }
                           , harmWave = [1516,179,990,59,266,60,157,17,94,39]
                           }]
        }
    ,RangeSpec
        { rangeFreq = 191.11
        , rangeNorm = 27370.2
        , rangeHarms = [HarmSpec
                           { harmAmp = AmpSpec
                               { ampAttack     = [0,0.015,0.000,0.504,0.294,0.334,0.000]
                               , ampSustain    = [0.000,0.580,1.166,1.017,0.996]
                               , ampDecay      = [0.996,1.015,0.308,0.002,0.010,-0.005,0]
                               }
                           , harmWave = [14794,31,882,197,3524,218,3723,61,2540,1574,1262,333,689,395,602,165,85,265,147,22,119,149,28,61,17,69,100,26,143,46,118,65,12]
                           }
                       ,HarmSpec
                           { harmAmp = AmpSpec
                               { ampAttack     = [0,0.115,1.000,0.567,0.275,-0.427,0.000]
                               , ampSustain    = [0.000,-1.425,-1.222,-0.760,-0.270]
                               , ampDecay      = [-0.270,-0.026,1.483,1.819,0.549,0.188,0]
                               }
                           , harmWave = [3278,18,70,4,39,10,16,5,12,57,7,13,11,12,9]
                           }
                       ,HarmSpec
                           { harmAmp = AmpSpec
                               { ampAttack     = [0,-0.002,0.000,-0.020,0.519,0.743,1.000]
                               , ampSustain    = [1.000,0.853,0.365,0.371,0.110]
                               , ampDecay      = [0.110,-0.004,0.185,0.011,0.001,0.003,0]
                               }
                           , harmWave = [16146,44,1093,292,4965,1047,6341,28,4423,760,1292,1316,659,1498,421,535,116,482,103,38,134,274,265,47,65,59,142,15,75,84,70,78,12]
                           }]
        }
    ,RangeSpec
        { rangeFreq = 22000
        , rangeNorm = 22329.4
        , rangeHarms = [HarmSpec
                           { harmAmp = AmpSpec
                               { ampAttack     = [0,0.015,0.000,0.504,0.294,0.334,0.000]
                               , ampSustain    = [0.000,0.580,1.166,1.017,0.996]
                               , ampDecay      = [0.996,1.015,0.308,0.002,0.010,-0.005,0]
                               }
                           , harmWave = [14794,31,882,197,3524,218,1823,61,1540,774,662,233,289,195,152,65,45,12]
                           }
                       ,HarmSpec
                           { harmAmp = AmpSpec
                               { ampAttack     = [0,0.115,1.000,0.567,0.275,-0.427,0.000]
                               , ampSustain    = [0.000,-1.425,-1.222,-0.760,-0.270]
                               , ampDecay      = [-0.270,-0.026,1.483,1.819,0.549,0.188,0]
                               }
                           , harmWave = [3278,18,70,4,39,10]
                           }
                       ,HarmSpec
                           { harmAmp = AmpSpec
                               { ampAttack     = [0,-0.002,0.000,-0.020,0.519,0.743,1.000]
                               , ampSustain    = [1.000,0.853,0.365,0.371,0.110]
                               , ampDecay      = [0.110,-0.004,0.185,0.011,0.001,0.003,0]
                               }
                           , harmWave = [16146,44,1093,292,2965,1047,3341,128,2423,760,392,316,159,148,61,45,26]
                           }]
        }]


------------------------------------------------------------------
-- French horn

-- | An emulation of the french horn. Parameters
--
-- > frenchHorn seed vibDepth attack sustain decay brightnessLevel cps = 
--
-- * seed - a seed for the random signals/numbers. It's in (0, 1)
--
-- * vibDepth -  Amount of the vibrato. It's in [-1, 1]
--
-- * attack - duration of the attack. 
--      Recommended value: 0.06 for tongued notes (up to 0.12 for lower notes, up to G2), 
--                         0.03 for short notes. 
--
-- * sustain - duration of the sustain
--
-- * decay - duration of the decay.
--      Recommended value: 0.25 (0.04 for short notes).
--
-- * brightnessLevel - filter cutoff factor. It's in (0, 1). The 0 is 40 Hz, the 1 s 10240 Hz
--
-- * cps - frequency of the note
frenchHorn :: D -> D -> D -> D -> D -> D -> D -> Sig
frenchHorn = woodwind $ WoodwindSpec 
    { woodwindRange         = fromSpec frenchHornRangeSpec
    , woodwindVibratoDur    = totalDur
    , woodwindFreqDeviation = ((-0.012, 0), (0, 0.005), (-0.005, 0), (0, 0.009)) 
    }

frenchHornRangeSpec :: [RangeSpec]
frenchHornRangeSpec = 
    [RangeSpec
        { rangeFreq = 113.26
        , rangeNorm = 5137
        , rangeHarms = [HarmSpec
                           { harmAmp = AmpSpec
                               { ampAttack     = [0,0.000,0.000,0.298,1.478,1.901,2.154]
                               , ampSustain    = [2.154,2.477,2.495,2.489,1.980]
                               , ampDecay      = [1.980,1.759,1.506,1.000,0.465,0.006,0]
                               }
                           , harmWave = [478,1277,2340,4533,2413,873,682,532,332,364,188,258,256,114,80,68,36]
                           }
                       ,HarmSpec
                           { harmAmp = AmpSpec
                               { ampAttack     = [0,0.000,1.000,2.127,0.694,-0.599,-1.807]
                               , ampSustain    = [-1.807,-2.485,-2.125,-2.670,-0.798]
                               , ampDecay      = [-0.798,-0.056,-0.038,0.000,0.781,0.133,0]
                               }
                           , harmWave = [414,906,831,507,268,36]
                           }
                       ,HarmSpec
                           { harmAmp = AmpSpec
                               { ampAttack     = [0,1.000,0.000,-4.131,-6.188,-1.422,1.704]
                               , ampSustain    = [1.704,6.362,3.042,5.736,-0.188]
                               , ampDecay      = [-0.188,-2.558,-2.409,0.000,-1.736,0.167,0]
                               }
                           , harmWave = [74,50,68,156,50,48,52,66]
                           }]
        }
    ,RangeSpec
        { rangeFreq = 152.055
        , rangeNorm = 35685
        , rangeHarms = [HarmSpec
                           { harmAmp = AmpSpec
                               { ampAttack     = [0,0.000,0.000,0.000,0.308,0.926,1.370]
                               , ampSustain    = [1.370,3.400,3.205,3.083,2.722]
                               , ampDecay      = [2.722,2.239,2.174,1.767,1.098,0.252,0]
                               }
                           , harmWave = [677,2663,4420,1597,1236,780,581,325,415,201,212,202,156,26]
                           }
                       ,HarmSpec
                           { harmAmp = AmpSpec
                               { ampAttack     = [0,0.478,1.000,0.000,4.648,1.843,5.242]
                               , ampSustain    = [5.242,-0.853,-0.722,-0.860,-0.547]
                               , ampDecay      = [-0.547,-0.462,-0.380,-0.387,-0.355,-0.250,0]
                               }
                           , harmWave = [648,1635,828,149,89,41]
                           }
                       ,HarmSpec
                           { harmAmp = AmpSpec
                               { ampAttack     = [0,-0.107,0.000,1.000,-0.570,0.681,-1.097]
                               , ampSustain    = [-1.097,1.495,0.152,0.461,0.231]
                               , ampDecay      = [0.231,0.228,0.256,0.152,0.087,0.042,0]
                               }
                           , harmWave = [1419,3414,901,503,204,146]
                           }]
        }
    ,RangeSpec
        { rangeFreq = 202.74
        , rangeNorm = 39632
        , rangeHarms = [HarmSpec
                           { harmAmp = AmpSpec
                               { ampAttack     = [0,0.039,0.000,0.000,0.230,0.216,0.647]
                               , ampSustain    = [0.647,1.764,1.961,1.573,1.408]
                               , ampDecay      = [1.408,1.312,1.125,0.802,0.328,0.061,0]
                               }
                           , harmWave = [1722,14359,5103,1398,2062,696,652,266,264,176,164,75]
                           }
                       ,HarmSpec
                           { harmAmp = AmpSpec
                               { ampAttack     = [0,1.142,1.000,0.000,-1.181,-3.005,-1.916]
                               , ampSustain    = [-1.916,2.325,3.249,2.154,1.766]
                               , ampDecay      = [1.766,2.147,1.305,0.115,0.374,0.162,0]
                               }
                           , harmWave = [1237,2287,237,72]
                           }
                       ,HarmSpec
                           { harmAmp = AmpSpec
                               { ampAttack     = [0,-0.361,0.000,1.000,1.369,1.865,1.101]
                               , ampSustain    = [1.101,-0.677,-0.833,-0.437,-0.456]
                               , ampDecay      = [-0.456,-0.465,-0.395,-0.144,-0.061,-0.012,0]
                               }
                           , harmWave = [2345,7796,1182,266,255,193,85]
                           }]
        }
    ,RangeSpec
        { rangeFreq = 270.32
        , rangeNorm = 26576.1
        , rangeHarms = [HarmSpec
                           { harmAmp = AmpSpec
                               { ampAttack     = [0,0.000,-0.147,-0.200,-0.453,-0.522,0.000]
                               , ampSustain    = [0.000,2.164,1.594,2.463,1.506]
                               , ampDecay      = [1.506,1.283,0.618,0.222,0.047,0.006,0]
                               }
                           , harmWave = [9834,16064,2259,1625,1353,344,356,621,195,155,77,98]
                           }
                       ,HarmSpec
                           { harmAmp = AmpSpec
                               { ampAttack     = [0,1.000,16.034,24.359,12.399,3.148,0.000]
                               , ampSustain    = [0.000,8.986,-2.516,13.268,0.541]
                               , ampDecay      = [0.541,-2.107,-11.221,-14.179,-7.152,5.327,0]
                               }
                           , harmWave = [377,193,41]
                           }
                       ,HarmSpec
                           { harmAmp = AmpSpec
                               { ampAttack     = [0,0.000,-0.318,-0.181,0.861,1.340,1.000]
                               , ampSustain    = [1.000,-1.669,-0.669,-2.208,-0.709]
                               , ampDecay      = [-0.709,-0.388,0.641,1.101,0.817,0.018,0]
                               }
                           , harmWave = [8905,10946,1180,1013,506,125,48]
                           }]
        }
    ,RangeSpec
        { rangeFreq = 360.43
        , rangeNorm = 26866.7
        , rangeHarms = [HarmSpec
                           { harmAmp = AmpSpec
                               { ampAttack     = [0,2.298,2.017,2.099,1.624,0.536,1.979]
                               , ampSustain    = [1.979,-2.465,-4.449,-4.176,-1.518]
                               , ampDecay      = [-1.518,-0.593,0.000,0.384,0.386,0.256,0]
                               }
                           , harmWave = [16460,4337,1419,1255,43,205,81,73,60,38]
                           }
                       ,HarmSpec
                           { harmAmp = AmpSpec
                               { ampAttack     = [0,-1.498,-1.342,-0.983,-0.402,0.572,-0.948]
                               , ampSustain    = [-0.948,4.490,6.433,5.822,1.845]
                               , ampDecay      = [1.845,0.618,0.000,-0.345,-0.295,-0.164,0]
                               }
                           , harmWave = [16569,5563,1838,1852,134,340,129,159,162,99]
                           }
                       ,HarmSpec
                           { harmAmp = AmpSpec
                               { ampAttack     = [0,-0.320,0.179,-0.551,-0.410,-0.417,-0.028]
                               , ampSustain    = [-0.028,-1.517,-1.523,-1.057,0.883]
                               , ampDecay      = [0.883,1.273,1.000,0.660,0.271,0.026,0]
                               }
                           , harmWave = [10383,4175,858,502,241,165]
                           }]
        }
    ,RangeSpec
        { rangeFreq = 480.29
        , rangeNorm = 31013.2
        , rangeHarms = [HarmSpec
                           { harmAmp = AmpSpec
                               { ampAttack     = [0,6.711,4.998,3.792,-0.554,-1.261,-5.584]
                               , ampSustain    = [-5.584,-4.633,-0.384,-0.555,-0.810]
                               , ampDecay      = [-0.810,0.112,0.962,1.567,0.881,0.347,0]
                               }
                           , harmWave = [15341,5092,1554,640,101]
                           }
                       ,HarmSpec
                           { harmAmp = AmpSpec
                               { ampAttack     = [0,-5.829,-4.106,-3.135,1.868,1.957,6.851]
                               , ampSustain    = [6.851,5.135,0.097,0.718,1.679]
                               , ampDecay      = [1.679,0.881,-0.009,-0.927,-0.544,-0.225,0]
                               }
                           , harmWave = [16995,6133,1950,788,136]
                           }
                       ,HarmSpec
                           { harmAmp = AmpSpec
                               { ampAttack     = [0,0.220,0.177,0.333,-0.302,0.071,-0.563]
                               , ampSustain    = [-0.563,0.338,1.214,0.840,0.103]
                               , ampDecay      = [0.103,0.003,-0.114,-0.049,-0.031,-0.017,0]
                               }
                           , harmWave = [22560,9285,4691,1837,342,294,307,222,288,103]
                           }]
        }
    ,RangeSpec
        { rangeFreq = 22000
        , rangeNorm = 26633.5
        , rangeHarms = [HarmSpec
                           { harmAmp = AmpSpec
                               { ampAttack     = [0,0.046,0.000,0.127,0.686,1.000,1.171]
                               , ampSustain    = [1.171,0.000,0.667,0.969,1.077]
                               , ampDecay      = [1.077,1.267,1.111,0.964,0.330,0.047,0]
                               }
                           , harmWave = [19417,5904,1666,913,266,55,81,46]
                           }
                       ,HarmSpec
                           { harmAmp = AmpSpec
                               { ampAttack     = [0,0.262,1.000,1.026,0.419,0.000,-0.172]
                               , ampSustain    = [-0.172,0.000,-0.764,-0.547,-0.448]
                               , ampDecay      = [-0.448,-0.461,-0.199,-0.015,0.432,0.120,0]
                               }
                           , harmWave = [11940,1211,111,38]
                           }
                       ,HarmSpec
                           { harmAmp = AmpSpec
                               { ampAttack     = [0,-0.014,0.000,0.102,0.006,0.000,-0.016]
                               , ampSustain    = [-0.016,1.000,0.753,0.367,0.163]
                               , ampDecay      = [0.163,-0.030,-0.118,-0.207,-0.103,-0.007,0]
                               }
                           , harmWave = [25132,6780,2886,1949,507,505,466,488,336,121]
                           }]
        }]


------------------------------------------------------------------
-- Sheng

-- | An emulation of the sheng. Parameters
--
-- > sheng seed vibDepth attack sustain decay brightnessLevel cps = 
--
-- * seed - a seed for the random signals/numbers. It's in (0, 1)
--
-- * vibDepth -  Amount of the vibrato. It's in [-1, 1]
--
-- * attack - duration of the attack. 
--      Recommended value: 0.1, 
--                         0.03 for short notes. 
--
-- * sustain - duration of the sustain
--
-- * decay - duration of the decay.
--      Recommended value: 0.2 (0.04 for short notes).
--
-- * brightnessLevel - filter cutoff factor. It's in (0, 1). The 0 is 40 Hz, the 1 s 10240 Hz
--
-- * cps - frequency of the note
sheng :: D -> D -> D -> D -> D -> D -> D -> Sig
sheng = woodwind $ WoodwindSpec 
    { woodwindRange         = shengRange
    , woodwindVibratoDur    = const 0.625
    , woodwindFreqDeviation = ((-0.03, 0), (0, 0.003), (-0.0015, 0), (0, 0.012))
    }

shengRange :: (D, D, D) -> D -> ([(Sig, Tab)], D)
shengRange (iattack, isustain, idecay) ifreq = ([(amp1, iwt1), (amp2, iwt2), (amp3, iwt3)], inorm)
    where
        amp1 = ar $ linseg 
            [ 0, 0.20*iattack, 2000, 0.40*iattack, 2050,  0.4*iattack, 2250, 0.18*isustain, 2500
            , 0.78*isustain, 2300, 0.04*isustain, 2000, 1.0*idecay, 0, 1, 0]
        amp2 = ar $	linseg 
            [ 0, 0.11*iattack, 100, 0.12*iattack, 5000,  0.12*iattack, 7500, 0.3*iattack, 9500
            , 0.35*iattack, 10500, 0.18*isustain, 12000, 0.3*isustain, 11000, 0.48*isustain, 10000
            , 0.04*isustain, 9000, 0.23*idecay, 7000, 0.67*idecay, 0, 1, 0]
        amp3 = ar $ linseg 
            [ 0, 0.18*iattack, 10, 0.15*iattack, 1250,  0.2*iattack, 1800,  0.24*iattack, 1900 
            , 0.23*iattack, 2200,  0.03*isustain, 2600, 0.2*isustain, 2900, 0.35*isustain, 2700 
            , 0.23*isustain, 2400, 0.15*isustain, 2000, 0.04*isustain, 1800, 0.23*idecay, 1200
            , 0.42*idecay, 20, 0.15*idecay, 0, 1, 0]

        inorm = byRange [ 34991, 32586, 35331, 37480 ]

        iwt1 = f11
        iwt2 = ifB (ifreq `lessThan` 1025) f31 f35
        iwt3 = byRange [f32, f33, f34, f36]
           
        f11 = sine
        f31 = skipNorm $ sines2 [(2, 1), (3, 0.16), (4, 1.16), (5, 0.45), (6, 0.33)]
        f32 = skipNorm $ sines2 
                [ (7, 1), (8, 0.83), (9, 0.85), (10, 0.16), (11, 0.5), (12, 0.38)
                , (13, 0.05), (14, 0.26), (15, 0.16), (16, 0.13), (17, 0.12), (18, 0.05), (19, 0.11)]
        f33 = skipNorm $ sines2 [ (7, 0.21), (8, 0.33), (9, 0.36), (10, 0.3), (11, 0.76), (12, 0.38), (13, 0.5), (14, 0.07)]
        f34 = skipNorm $ sines2 [ (7, 0.43), (8, 1.2), (9, 0.4), (10, 0.3), (11, 0.1) ]
        f35 = skipNorm $ sines2 [ (2, 0.58), (3, 0.83), (4, 0.83) ]
        f36 = skipNorm $ sines2 [ (5, 2.1), (6, 1.2), (7, 0.4) ]   
       
        byRange :: Tuple a => [a] -> a
        byRange = byFreq ifreq . zip freqs
        freqs = [538, 760, 1025, 22000]

----------------------------------------------------------------
-- Hulusi 

-- | An emulation of the hulusi. Parameters
--
-- > hulusi seed vibDepth attack sustain decay brightnessLevel cps = 
--
-- * seed - a seed for the random signals/numbers. It's in (0, 1)
--
-- * vibDepth -  Amount of the vibrato. It's in [-1, 1]
--
-- * attack - duration of the attack. 
--      Recommended value: 0.03
--
-- * sustain - duration of the sustain
--
-- * decay - duration of the decay.
--      Recommended value: 0.1 (0.04 for short notes).
--
-- * brightnessLevel - filter cutoff factor. It's in (0, 1). The 0 is 40 Hz, the 1 s 10240 Hz
--
-- * cps - frequency of the note
hulusi :: D -> D -> D -> D -> D -> D -> D -> Sig
hulusi = woodwind $ WoodwindSpec 
    { woodwindRange         = hulusiRange
    , woodwindVibratoDur    = const 20
    , woodwindFreqDeviation = ((-0.03, 0), (0, 0.003), (-0.0015, 0), (0, 0.012))
    }


hulusiRange :: (D, D, D) -> D -> ([(Sig, Tab)], D)
hulusiRange (iattack, isustain, idecay) ifreq = 
        ( [ (amp1, iwt1), (amp2, iwt2), (amp3, iwt3) ]       
        , inorm 
        )
    where 
        byRange :: Tuple a => [a] -> a
        byRange = byFreq ifreq . zip [320, 427, 680, 22000]
       
        amp1 = byRange [amp1_0, amp1_1, amp1_2, amp1_3]
        amp2 = byRange [amp2_0, amp2_1, amp2_2, amp2_3]
        amp3 = byRange [amp3_0, amp3_1, amp3_2, amp3_3]
       
        iwt1 = byRange [iwt1_0, iwt1_1, iwt1_2, iwt1_3]
        iwt2 = byRange [iwt2_0, iwt2_1, iwt2_2, iwt2_3]
        iwt3 = byRange [iwt3_0, iwt3_1, iwt3_2, iwt3_3]

        inorm = byRange [ inorm_0, inorm_1, inorm_2, inorm_3 ]

        -- range 0
        amp1_0 = ar $ linseg 
            [ 0, 0.33*iattack, 750, 0.17*iattack, 2000,  0.17*iattack
            , 5000, 0.16*iattack, 13000, 0.17*iattack, 15000, 0.03*isustain
            , 16000, 0.47*isustain, 15000, 0.5*isustain, 13500, 0.2*idecay
            , 13000, 0.2*idecay, 11000, 0.2*idecay, 6000, 0.2*idecay, 150, 0.2*idecay, 0
            ]
        amp2_0 = ar $ linseg 
            [ 0, 0.67*iattack, 30, 0.33*iattack, 9000,  0.05*isustain
            , 11000, 0.05*isustain, 12000, 0.4*isustain, 9500, 0.5*isustain
            , 7200, 0.2*idecay, 5600, 0.2*idecay, 3200, 0.2*idecay
            , 1000, 0.2*idecay, 50, 0.1*idecay, 0, 0.1*idecay, 0 
            ]
        amp3_0 = ar $ linseg 
            [ 0, 0.33*iattack, 0, 0.4*iattack, 30,  0.27*iattack
            , 3600, 0.07*isustain, 2000, 0.43*isustain, 2800, 0.5*isustain
            , 3000, 0.2*idecay, 2700, 0.2*idecay, 1500, 0.2*idecay, 150
            , 0.2*idecay, 50, 0.08*idecay, 0, 0.12*idecay, 0]

        iwt1_0 = f11
        iwt2_0 = f31
        iwt3_0 = f32

        inorm_0 = 26985

        -- range 1
        amp1_1 = ar $ linseg 
            [ 0, 0.43*iattack, 1600, 0.27*iattack, 9000, 0.3*iattack
            , 5400, 0.5*isustain, 5500, 0.5*isustain, 5300, 0.2*idecay
            , 4200, 0.2*idecay, 3000, 0.2*idecay, 1000, 0.2*idecay
            , 100, 0.2*idecay, 0 
            ]
        amp2_1 = ar $ linseg 
            [ 0, 0.43*iattack, 20, 0.13*iattack, 700,  0.30*iattack
            , 6300,  0.14*iattack, 7500, 0.03*isustain, 9000, 0.14*isustain
            , 9200, 0.65*isustain, 7000, 0.14*isustain, 6000, 0.04*isustain
            , 5000, 0.2*idecay, 4600, 0.2*idecay, 3600, 0.2*idecay, 2400, 0.2*idecay
            , 600, 0.15*idecay, 0, 0.05*idecay, 0
            ]
        amp3_1 = ar $ linseg 
            [ 0, 0.52*iattack, 10, 0.26*iattack, 1500,  0.22*iattack
            , 7000, 0.03*isustain, 9000, 0.02*isustain, 10500, 0.15*isustain
            , 9700, 0.65*isustain, 8000, 0.15*isustain, 6400, 0.2*idecay
            , 4600, 0.2*idecay, 2600, 0.2*idecay, 800, 0.2*idecay
            , 10, 0.1*idecay, 0, 0.1*idecay, 0 ]
        iwt1_1 = f11
        iwt2_1 = f33
        iwt3_1 = f34
        inorm_1 = 36133
        
        -- range2: 				; for high middle range tones
        amp1_2 = ar $ linseg 
            [ 0, 0.27*iattack, 1500, 0.22*iattack, 8000, 0.51*iattack
            , 9000, 0.02*isustain, 11000, 0.04*isustain, 10600, 0.81*isustain
            , 10000, 0.09*isustain, 9000, 0.04*isustain, 7000, 0.2*idecay
            , 6000, 0.2*idecay, 4000, 0.2*idecay, 2000, 0.2*idecay, 600, 0.2*idecay, 0 
            ]
        amp2_2 = ar $ linseg 
            [ 0, 0.38*iattack, 20, 0.17*iattack, 3800, 0.45*iattack, 5500, 0.02*isustain
            , 6000, 0.5*isustain, 3800, 0.35*isustain, 3300, 0.09*isustain
            , 1000, 0.04*isustain, 750, 0.2*idecay, 600, 0.2*idecay, 350, 0.2*idecay
            , 150, 0.2*idecay, 40, 0.2*idecay, 0 
            ]
        amp3_2 = ar $ linseg 
            [ 0, 0.44*iattack, 20, 0.1*iattack, 1300, 0.08*iattack, 750, 0.38*iattack
            , 600, 0.5*isustain, 800, 0.35*isustain, 750, 0.1*isustain, 550,  0.05*isustain
            , 50, 0.2*idecay, 30, 0.2*idecay, 15, 0.2*idecay, 7, 0.2*idecay, 0, 0.2*idecay, 0
            ]
        iwt1_2 = f35
        iwt2_2 = f36
        iwt3_2 = f37
        inorm_2 = 27905

        -- range3: 				; for high range tones
        amp1_3 = ar $ linseg 
            [ 0, 0.15*iattack, 300, 0.15*iattack, 1100, 0.15*iattack
            , 4000, 0.15*iattack, 9000, 0.15*iattack, 20000, 0.15*iattack
            , 27000, 0.10*iattack, 29000, 0.12*isustain, 26000, 0.56*isustain
            , 27000, 0.32*isustain, 24000, 0.33*idecay, 23000, 0.33*idecay
            , 6000, 0.17*idecay, 1000, 0.16*idecay, 0 ]
        amp2_3 = ar $ linseg 
            [ 0, 0.45*iattack, 15, 0.15*iattack, 250, 0.15*iattack, 850, 0.15*iattack
            , 1800, 0.1*iattack, 2100, 0.03*isustain, 2250, 0.07*isustain, 2000
            , 0.25*isustain, 2100, 0.4*isustain, 2000, 0.15*isustain, 1400, 0.1*isustain
            , 800, 0.45*idecay, 170, 0.22*idecay, 120, 0.11*idecay, 40, 0.11*idecay
            , 15, 0.11*idecay, 0
            ]
        amp3_3 = ar $ linseg 
            [ 0, 0.52*iattack, 15, 0.15*iattack, 400, 0.22*iattack
            , 2050, 0.11*iattack, 2200, 0.06*isustain, 1000, 0.15*isustain
            , 1500, 0.13*isustain, 1250, 0.5*isustain, 2500, 0.04*isustain
            , 2300, 0.12*isustain, 2000, 0.2*idecay, 1600, 0.2*idecay
            , 900, 0.2*idecay, 150, 0.2*idecay, 20, 0.1*idecay, 0, 0.1*idecay, 0
            ]
        iwt1_3 = 	f11
        iwt2_3 = 	f12
        iwt3_3 = 	f13
        inorm_3 = 	27507 

        f11 = sine
        f12 = skipNorm $ sines2 [(2, 1)]
        f13 = skipNorm $ sines2 [(3, 1)]
        f31 = skipNorm $ sines2 [(2, 0.46), (3, 1), (5, 0.31), (6, 0.17), (9, 0.12)]
        f32 = skipNorm $ sines2 [(4, 1), (7, 0.34), (8, 0.25), (10, 0.19), (11, 0.25)]
        f33 = skipNorm $ sines2 [(2, 1), (7, 0.33), (10, 0.23), (11, 0.07)]
        f34 = skipNorm $ sines2 [(3, 0.77), (4, 0.29), (5, 1), (6, 0.5), (8, 0.2), (9, 0.07)]
        f35 = skipNorm $ sines2 [(1, 1), (2, 0.36), (3, 1.1), (4, 0.3)]
        f36 = skipNorm $ sines2 [(5, 1), (6, 0.22)]
        f37 = skipNorm $ sines2 [(7, 1), (8, 0.42), (9, 0.17), (10, 0.35), (11, 0.2)]

------------------------------------------------------------------------
-- dizi

-- | An emulation of the dizi. Parameters
--
-- > dizi seed vibDepth attack sustain decay brightnessLevel cps = 
--
-- * seed - a seed for the random signals/numbers. It's in (0, 1)
--
-- * vibDepth -  Amount of the vibrato. It's in [-1, 1]
--
-- * attack - duration of the attack. 
--      Recommended value: 0.12 for slurred notes, 0.07 for tongued notes, 0.03 for short notes.
--
-- * sustain - duration of the sustain
--
-- * decay - duration of the decay.
--      Recommended value: 0.14 (0.04 for short notes).
--
-- * brightnessLevel - filter cutoff factor. It's in (0, 1). The 0 is 40 Hz, the 1 s 10240 Hz
--
-- * cps - frequency of the note
dizi :: D -> D -> D -> D -> D -> D -> D -> Sig
dizi = woodwind $ WoodwindSpec 
    { woodwindRange         = diziRange
    , woodwindVibratoDur    = const 0.625
    , woodwindFreqDeviation = ((-0.03, 0), (0, 0.003), (-0.0015, 0), (0, 0.012))
    }


diziRange :: (D, D, D) -> D -> ([(Sig, Tab)], D)
diziRange (iattack, isustain, idecay) ifreq = 
        ( [ (amp1, iwt1), (amp2, iwt2), (amp3, iwt3) ]       
        , inorm 
        )
    where 
        byRange :: Tuple a => [a] -> a
        byRange = byFreq ifreq . zip [320, 480, 680, 905, 1280, 1710, 22000]
       
        amp1 = byRange [amp1_1, amp1_2, amp1_3, amp1_4, amp1_5, amp1_6, amp1_7]
        amp2 = byRange [amp2_1, amp2_2, amp2_3, amp2_4, amp2_5, amp2_6, amp2_7]
        amp3 = byRange [amp3_1, amp3_2, amp3_3, amp3_4, amp3_5, amp3_6, amp3_7]
       
        iwt1 = byRange [iwt1_1, iwt1_2, iwt1_3, iwt1_4, iwt1_5, iwt1_6, iwt1_7]
        iwt2 = byRange [iwt2_1, iwt2_2, iwt2_3, iwt2_4, iwt2_5, iwt2_6, iwt2_7]
        iwt3 = byRange [iwt3_1, iwt3_2, iwt3_3, iwt3_4, iwt3_5, iwt3_6, iwt3_7]

        inorm = byRange [inorm_1, inorm_2, inorm_3, inorm_4, inorm_5, inorm_6, inorm_7 ]

        iatt = iattack / 6
        isus = isustain / 4
        idec = iattack / 6

 
        -- range1:							; for very low range tones
        amp1_1 = ar $	linseg 
            [ 0, 0.4*iattack, 1500,  0.6*iattack, 10000,  0.5*isustain, 11000
            ,  0.5*isustain, 9000, 0.4*idecay, 8000, 0.3*idecay, 1500, 0.3*idecay, 0
            ]
        amp2_1 = ar $	linseg 
            [ 0, 0.4*iattack, 200,  0.1*iattack, 1000,   0.5*iattack, 6000
            ,  0.1*isustain, 11000,  0.3*isustain, 13000,  0.6*isustain, 12000
            ,  0.6*idecay, 1500, 0.4*idecay, 0 ]
        amp3_1 = ar $ linseg 
            [ 0, 0.5*iattack, 30,  0.5*iattack, 500,  0.1*isustain, 1200
            ,  0.7*isustain, 2200,  0.2*isustain, 1750, 0.5*idecay, 250
            , 0.2*idecay, 0, 1, 0 ]
        iwt1_1 = 	f11
        iwt2_1 = 	f20
        iwt3_1 = 	f21
        inorm_1 = 	32875

        -- range2:							; for very low range tones
        amp1_2 = ar $	linseg 
            [ 0, 0.4*iattack, 2000,  0.3*iattack, 6000,  0.3*iattack, 25000
            ,  0.5*isustain, 24000,  0.5*isustain, 20000, 0.4*idecay, 5000
            , 0.3*idecay, 1500, 0.3*idecay, 0 ]
        amp2_2 = ar $ linseg
            [ 0, 0.5*iattack, 100,  0.5*iattack, 3000,  0.1*isustain, 4500
            ,  0.4*isustain, 2000,  0.4*isustain, 2200,  0.1*isustain, 500
            , 0.5*idecay, 150, 0.5*idecay, 0 ]
        amp3_2 = ar $ linseg
            [ 0, 0.5*iattack, 30,  0.5*iattack, 500,  0.1*isustain, 1200
            ,  0.7*isustain, 2200,  0.2*isustain, 1750, 0.5*idecay, 250, 0.2*idecay, 0, 1, 0 ]
        iwt1_2 = 	f11
        iwt2_2 = 	f22
        iwt3_2 = 	f23
        inorm_2 = 	26080

        -- range3:							; for low range tones
        amp1_3 = ar $ linseg 
            [ 0, iatt, 0.000, iatt, 0.219, iatt, 0.500, iatt, 0.889, iatt, 1.035
            , iatt, 0.963, isus, 0.424, isus, 0.135, isus, 0.108, isus, 0.204
            , idec, 0.445, idec, 0.531, idec, 0.513, idec, 0.365, idec, 0.053, idec, 0 
            ]
        amp2_3 = ar $ linseg
            [ 0, iatt, 0.000, iatt, -0.106, iatt, -0.112, iatt, -0.187, iatt
            , -0.091, iatt, 0.056, isus, 0.558, isus, 0.901, isus, 0.904, isus
            , 0.729, idec, 0.303, idec, 0.057, idec, 0.016, idec, -0.076, idec, -0.016, idec, 0 
            ]
        amp3_3 = ar $ linseg 
            [ 0, iatt, 1.000, iatt, 0.607, iatt, -0.116, iatt, -0.205, iatt, -0.530, iatt
            , -0.195, isus, 0.601, isus, 0.478, isus, -0.371, isus, -0.916, idec
            , -0.782, idec, -0.107, idec, -0.811, idec, -0.189, idec, -0.036, idec, 0
            ]
        iwt1_3 = 	f24
        iwt2_3 = 	f25
        iwt3_3 = 	f26
        inorm_3 = 	24364
                
        -- range4:							; for low mid-range tones 
        amp1_4 = ar $	linseg 
            [ 0, iatt, 0.000, iatt, 0.049, iatt, 0.027, iatt, 0.005, iatt
            , -0.020, iatt, 0.378, isus, 0.925, isus, 1.032, isus, 1.106, isus
            , 0.915, idec, 0.858, idec, 0.722, idec, 0.250, idec, -0.002, idec
            , 0.004, idec, 0
            ]
        amp2_4 = ar $	linseg 
            [ 0, iatt, 0.000, iatt, -0.182, iatt, -0.029, iatt, 0.397, iatt
            , 2.065, iatt, 3.136, isus, 0.250, isus, -0.685, isus, -1.369, isus
            , -1.176, idec, -1.023, idec, -0.212, idec, 0.810, idec, 0.469, idec
            , 0.018, idec, 0
            ]
        amp3_4 = ar $	linseg
            [ 0, iatt, 1.000, iatt, 0.007, iatt, 1.039, iatt, 0.466, iatt
            , 0.627, iatt, 4.181, isus, -2.481, isus, -2.529, isus
            , -4.838, isus, 0.137, idec, -2.823, idec, -1.899, idec
            , 4.910, idec, 0.319, idec, 0.039, idec, 0
            ]
        iwt1_4 = 	f27
        iwt2_4 = 	f28
        iwt3_4 = 	f29
        inorm_4 = 27832
                                
        -- range5:							; for high mid-range tones 
        amp1_5 = ar $	linseg 
            [ 0, iatt, 0.000, iatt, 0.000, iatt, 0.018, iatt, 0.000, iatt
            , 0.450, iatt, 1.130, isus, 1.475, isus, 1.682, isus, 1.533, isus
            , 1.243, idec, 0.945, idec, 0.681, idec, 0.210, idec, 0.046, idec
            , 0.004, idec, 0
            ]
        amp2_5 = ar $	linseg 
            [ 0, iatt, 0.000, iatt, 0.102, iatt, 0.196, iatt, 1.000, iatt
            , 1.108, iatt, -0.024, isus, -1.557, isus, -2.443, isus
            , -1.553, isus, -0.979, idec, -0.268, idec, -0.271, idec
            , -0.015, idec, 0.017, idec, 0.108, idec, 0 
            ]
        amp3_5 = ar $	linseg 
            [ 0, iatt, 1.000, iatt, 0.423, iatt, 0.287, iatt, 0.000, iatt
            , -0.987, iatt, -0.621, isus, 3.030, isus, 2.349, isus, 3.075, isus
            , 0.331, idec, 0.994, idec, -1.319, idec, -0.378, idec, 0.000, idec
            , -0.023, idec, 0
            ]
        iwt1_5 = 	f30
        iwt2_5 = 	f31
        iwt3_5 = 	f32
        inorm_5 = 	27918

        -- range6:							; for high range tones
        amp1_6 = ar $	linseg 
            [ 0, iatt, 0.000, iatt, 0.322, iatt, 0.115, iatt, 0.090, iatt
            , -0.148, iatt, 1.743, isus, 2.079, isus, 0.844, isus
            , 0.889, isus, 1.914, idec, 0.718, idec, 0.206, idec
            , 0.361, idec, -0.278, idec, -0.272, idec, 0
            ]
        amp2_6 = ar $	linseg 
            [ 0, iatt, 1.000, iatt, 2.675, iatt, 1.579, iatt, -0.879, iatt
            , -4.025, iatt, -9.342, isus, 4.570, isus, -3.372, isus
            , -2.904, isus, 0.755, idec, 5.796, idec, -3.764, idec
            , 2.193, idec, 0.718, idec, 1.029, idec, 0 ]
        amp3_6 = ar $ linseg 
            [ 0, iatt, 0.000, iatt, -0.334, iatt, -0.108, iatt, 0.028, iatt
            , 0.765, iatt, -0.874, isus, -1.222, isus, 0.236, isus
            , 0.187, isus, -1.036, idec, 0.276, idec, 0.532, idec
            , -0.204, idec, 0.311, idec, 0.283, idec, 0 ]
        iwt1_6 = 	f33
        iwt2_6 = 	f34
        iwt3_6 = 	f35
        inorm_6 = 	23538

        -- range7:							; for very high range tones 
        amp1_7 = ar $ linseg 
            [ 0, iatt, 0.000, iatt, -0.071, iatt, 0.017, iatt, 0.134, iatt
            , -0.068, iatt, 0.192, isus, 1.375, isus, 1.875, isus, 1.463, isus
            , 1.446, idec, 0.932, idec, 0.561, idec, 0.100, idec, 0.036, idec
            , 0.000, idec, 0 
            ]
        amp2_7 = ar $ linseg 
            [ 0, iatt, 1.000, iatt, 3.541, iatt, 3.665, iatt, -0.651, iatt
            , 1.017, iatt, 1.331, isus, -4.611, isus, -2.534, isus, -4.241, isus
            , -2.738, idec, -0.609, idec, -1.065, idec, 1.122, idec, 0.605, idec
            , 0.093, idec, 0 ]
        amp3_7 = ar $ linseg 
            [ 0, iatt, 0.000, iatt, 0.061, iatt, 0.093, iatt, 0.323, iatt
            , 1.011, iatt, 0.819, isus, 0.162, isus, -0.152, isus, 0.080, isus
            , -0.139, idec, 0.051, idec, -0.054, idec, -0.019, idec, -0.013, idec
            , 0.003, idec, 0 ]
        iwt1_7 = 	f36
        iwt2_7 = 	f37
        iwt3_7 = 	f38
        inorm_7 = 	30675

        f11 = sine

        f20 = skipNorm $ sines [ 0, 1, 0.21 ]
        f21 = skipNorm $ sines [ 0, 0, 0, 1, 1.3, 0.66, 0.9, 0.5, 0.8, 0.6, 1.2, 0.5, 0.8, 0.6, 0.75, 0.6, 0.9, 1.2, 0.9, 1.1, 0.85, 0.9, 0.4, 0.3, 0.45, 0.3, 0.25, 0.15 ]
        f22 = skipNorm $ sines [ 0, 1, 0, 0.33 ]
        f23 = skipNorm $ sines [ 0, 0, 0.75, 0, 0.28, 0.45, 0.36, 1, 0.54, 0.5, 0.81, 1, 0.95, 0.9, 0.08, 0.24, 0.45, 0.41, 0.25, 0.07 ]
        f24 = skipNorm $ sines [ 20742, 2870, 929, 899, 1567, 958, 318, 1168, 838, 781, 192 ] 
        f25 = skipNorm $ sines [ 18419, 4615, 1255, 689, 3851, 1889, 498, 3127, 3041, 2262, 422, 136 ]
        f26 = skipNorm $ sines [ 1700, 331, 615, 259, 188, 164, 79, 393, 191, 108 ] 
        f27 = skipNorm $ sines [ 17040, 1836, 3609, 4228, 3600, 1910, 9599, 3722, 925, 862, 1292, 227 ]
        f28 = skipNorm $ sines [ 4206, 283, 125, 465, 341, 168, 201, 196, 199, 140 ] 
        f29 = skipNorm $ sines [ 24, 22, 129, 209, 54, 127 ]
        f30 = skipNorm $ sines [ 13283, 1588, 2948, 337, 9009, 1040, 2175, 222 ]
        f31 = skipNorm $ sines [ 3831, 572, 332, 252, 209, 243, 91 ]
        f32 = skipNorm $ sines [ 61, 59, 120, 196, 26 ]
        f33 = skipNorm $ sines [ 22605, 2267, 3470, 1604, 2849, 86 ]
        f34 = skipNorm $ sines [ 97, 92, 113, 75, 63 ]
        f35 = skipNorm $ sines [ 21615, 1982, 3912, 1422, 2987 ]
        f36 = skipNorm $ sines [ 17863, 3388, 257 ]
        f37 = skipNorm $ sines [ 97, 136, 50 ]
        f38 = skipNorm $ sines [ 28207, 1745, 499 ]

