#picaxe 18m2
#no_data

#no_data
main:
    ;high B.0
    disconnect
      
    
    ;sertxd ("En attente de vos ordres",13,10)
    ;on lit deux chiffres entre 0 et 8 pour piloter 4 sorties
    ;il faut le faire deux fois de suite
    
    ;on lit les ordres
    ;envoiyer un chiffre netre 0 et 255
    serrxd #b0
    let b1 = b0 and $01
    let b2 = b0 and $02
    let b3 = b0 and $04
    let b4 = b0 and $08
    let b5 = b0 and $10
    let b6 = b0 and $20
    let b7 = b0 and $40
    let b8 = b0 and $80
    
    if b1 = $01 then high B.0 else low B.0 endif 
    if b2 = $02 then high B.1 else low B.1 endif
    if b3 = $04 then high B.2 else low B.2 endif
    if b4 = $08 then high B.3 else low B.3 endif
    if b5 = $10 then high B.4 else low B.4 endif
    if b6 = $20 then high B.5 else low B.5 endif
    if b7 = $40 then high B.6 else low B.6 endif
    if b8 = $80 then high B.7 else low B.7 endif
    
    
           
    ;sertxd ("Output:",#b0," recue",13,10)
    
    let b9 = pinsC
    sertxd (#b9,13,10)
    
    reconnect
    pause 200
    goto main 
    
