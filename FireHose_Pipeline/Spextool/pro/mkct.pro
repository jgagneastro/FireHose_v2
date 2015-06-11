;+
; NAME:
;     mkct (make color table)
;
; PURPOSE:
;     Loads a color table and reserves the lower indices for plotting
;
; CATEGORY:
;     Plotting and Image Display
;
; CALLING SEQUENCE:
;     mkct,[cm],BOT=bot,RED=red,GREEN=green,BLUE=blue,CANCEL=cancel
;
; INPUTS:
;     None
;
; OPTIONAL INPUTS:
;     cm - The index of the requested color map 
;
;          0-        B-W LINEAR   14-             STEPS   28-         Hardcandy
;          1-        BLUE/WHITE   15-     STERN SPECIAL   29-            Nature
;          2-   GRN-RED-BLU-WHT   16-              Haze   30-             Ocean
;          3-   RED TEMPERATURE   17- Blue - Pastel - R   31-        Peppermint
;          4- BLUE/GREEN/RED/YE   18-           Pastels   32-            Plasma
;          5-      STD GAMMA-II   19- Hue Sat Lightness   33-          Blue-Red
;          6-             PRISM   20- Hue Sat Lightness   34-           Rainbow
;          7-        RED-PURPLE   21-   Hue Sat Value 1   35-        Blue Waves
;          8- GREEN/WHITE LINEA   22-   Hue Sat Value 2   36-           Volcano
;          9- GRN/WHT EXPONENTI   23- Purple-Red + Stri   37-             Waves
;         10-        GREEN-PINK   24-             Beach   38-         Rainbow18
;         11-          BLUE-RED   25-         Mac Style   39-   Rainbow + white
;         12-          16 LEVEL   26-             Eos A   40-   Rainbow + black
;         13-           RAINBOW   27-             Eos B
;
; KEYWORD PARAMETERS:
;     BOT    - The starting index of the color map.
;     RED    - The array of red indices
;     GREEN  - The array of green indices
;     BLUE   - The array of blue indices
;     NAMES  - An array of the color names
;     INDEX  - An array of the color indices
;     CANCEL - Set on return if there is a problem
;
; OUTPUTS:
;     None
;
; OPTIONAL OUTPUTS:
;     None
;
; COMMON BLOCKS:
;     None
;
; SIDE EFFECTS:
;     Overwrites the existing color table
;
; RESTRICTIONS:
;     None
;
; PROCEDURE:
;     Loads a color table and a personal table for plotting colors.
;     The person colors are given below.
;
;     0 : Black    10 : Lightblue
;     1 : White    11 : Grey      
;     2 : Red      12 : Violet
;     3 : Green    13 : Purple       
;     4 : Blue     14 : Deep Pink       
;     5 : Yellow   15 : Chartreuse      
;     6 : Magenta  16 :        
;     7 : Cyan
;     8 : Orange
;     9 : Navy
;
; EXAMPLE:
;     
; MODIFICATION HISTORY:
;     2000-05-05 - Written by M. Cushing, Institute for Astronomy, UH
;                  (I believe I got it from someone else but cannot
;                  remember from whom.)
;     2003-06-25 - Add more colors with better ones taken from Fanning's
;                  pickcolor routine.
;     2004-02-17 - Added more colors and added the NAMES and INDICES
;                  keywords
;     2006-10-05 - Added Windows support, D. Clemens, Boston University.
;     
;-
pro mkct,cm,BOT=bot,RED=red,GREEN=green,BLUE=blue,NAMES=names,INDICES=indices,$
         CANCEL=cancel

cancel = 0

if n_params() eq 1 then begin

    cancel = cpar('mkct',cm,1,'Cm',[2,3,4,5],0)
    if cancel then return

endif
if !d.name eq 'X' then device, DECOMPOSED=0,TRUE_COLOR=24
if !d.name eq 'WIN' then device, DECOMPOSED=0

names   = ['Black','White','Red','Green','Blue','Yellow','Magenta']
indices = [0      ,1      ,2    ,3      ,4     ,5       ,6        ]
red     = [0      ,255    ,255  ,0      ,0     ,255     ,255      ]
green   = [0      ,255    ,0    ,255    ,0     ,255     ,0        ]
blue    = [0      ,255    ,0    ,0      ,255   ,0       ,255      ]

names   = [names  ,'Cyan','Orange','Navy','Lightblue','Grey','Violet']
indices = [indices,7     ,8       ,9     ,10         ,11    ,12      ]
red     = [red    ,0     ,255     ,0     ,127        ,50    ,238     ]
green   = [green  ,255   ,165     ,0     ,127        ,50    ,130     ]
blue    = [blue   ,255   ,0       ,128   ,255        ,50    ,238     ]

names   = [names  ,'Purple','Deep Pink','Chartreuse','Dark Green']
indices = [indices,13      ,14         ,15          ,16          ]
red     = [red    ,160     ,255        ,127         ,0           ]
green   = [green  ,32      ,20         ,255         ,100         ]
blue    = [blue   ,240     ,147        ,0           ,0           ] 


tvlct, red, green, blue

if n_params() eq 0 then cm = 0
loadct, cm, BOTTOM=n_elements(red), /SILENT

bot = n_elements(red)

tvlct,red,green,blue,bot,/GET

end




