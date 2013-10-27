#picaxe 18m2
#no_data

clear0:   let pinsB=%00000000
etape0:   low B.0
          pause 100
          if pinC.6=1 and pinC.7=0 then goto clear1
          if pinC.7=1 and pinC.6=0 then goto clear2
          goto etape0
clear1:   let pinsB=%00000000
etape1:   high B.0
          pause 100
          if pinC.6=0 or pinC.7=1 then goto clear0
          goto etape1
clear2:   let pinsB=%00000000
etape2:   high B.0
          pause 100
          if pinC.7=0 or pinC.6=1 then goto clear0
          goto etape2
