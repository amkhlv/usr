# Getting dpi

    xdpyinfo

# monitor table

## Obtaining info from xrandr

    xrandr --verbose

\-- this has screen dimensions in mm. To calculate the diagonal in inches:

    ( x=410; y=257; echo 'e( 1/2 * l( '$x'^2 + '$y'^2 ))/25.4' | bc -l )

To calculate the screen dpi:

    ( x=410; r=1440 ; echo 25.4*$r/$x | bc -l )
    

## Widescreen Format (16:10) Display DPI Resolution   Diagonal

    (inches) 
            2560x1600(WQXGA)    
                       1920x1200(WUXGA)    
                                  1680x1050(WSXGA+)    
                                             1440x900(WXGA+)    
                                                        1280x800(WXGA)
    12.1    250x249    187x187    164x164    140x140    125x125
    13.3    227x227    170x170    149x149    128x128    114x113
    14.1    214x214    161x160    141x140    120x120    107x107
    15      201x201    151x151    132x132    113x113    101x101
    15.4    196x196    147x147    129x128    110x110    98x98
    17      178x177    133x133    117x116    100x100    89x89
    19      159x159    119x119    104x104    89x89      79x79
    20      151x151    113x113    99x99      85x85      76x75
    22      137x137    103x103    90x90      77x77      69x69
    24      126x126    94x94      83x82      71x71      63x63
    27      112x112    84x84      73x73      63x63      56x56
    30      101x101    76x75      66x66      57x57      50x50
    
    

## Standard Format (4:3) Display DPI Resolution Diagonal

    (inches)    
            1600x1200(UXGA)        
                       1400x1050(SXGA+)    
                                  1280x960   1024x768
    12.1    165x165    145x145    132x132    106x106
    14.1    142x142    124x124    113x114    91x91
    15      133x133    117x117    107x107    85x85
    17      118x118    103x103    94x94      75x75
    19      105x105    92x92      84x84      67x67
    20      100x100    87x88      80x80      64x64
    
    

## SXGA (5:4) Display DPI Resolution Diagonal

    (inches)1280x1024(SXGA)
    12.1    135x135
    14.1    116x116
    15      109x109
    17      96x96
    19      86x86
    20      82x82
