;-----------------------------------------------------------------------------------------------------------------
;Ce programme est l'oeuvre exclusive de TACK S�bastien, professeur de Technologie aux coll�ge de Verson et Brunet 
;Merci de maintenir la paternit� de cette oeuvre
;R�alisation Aout 2013
;
;-----------------------------------------------------------------------------------------------------------------
;L'id�e est celle-ci l'ordinateur communique avec la carte PICAXE au travers du cable USB de programmation
;Lorsqu'un ordre est envoy�, le PICAXE execute cet ordre recu en,tre 0 et 255 puis retourne l'�tat des entr�es 
;par d�faut. 
;
#no_data
main:
    ;high B.0
    disconnect
      
    
    'sertxd ("En attente de vos ordres",13,10)
    ;on lit deux chiffres entre 0 et 8 pour piloter 4 sorties
    ;il faut le faire deux fois de suite
    
    ;on lit les ordres
    ;envoiyer un chiffre netre 0 et 255
    ;a faire si 2 carac particulier alors envoyer etat des ADC sinon executer ordre
    
    ;recoit un ordre entre 0 et 255 puis actionne les sorties
    serrxd #b0
    'let b1 = b0 and $01
    let b2 = b0 and $02
    let b3 = b0 and $04
    'let b4 = b0 and $08
    'let b5 = b0 and $10
    'let b6 = b0 and $20
    'let b7 = b0 and $40
    'let b8 = b0 and $80
    
    'if b1 = $01 then high 0 else low 0 endif 
    if b2 = $02 then high 1 else low 1 endif
    if b3 = $04 then high 2 else low 2 endif
    'if b4 = $08 then high B.3 else low B.3 endif
    'if b5 = $10 then high B.4 else low B.4 endif
    'if b6 = $20 then high B.5 else low B.5 endif
    'if b7 = $40 then high B.6 else low B.6 endif
    'if b8 = $80 then high B.7 else low B.7 endif
    
    ;lit l'etat des entrees    
    let b9 = pins and %00011000
    
    sertxd (#b9,"#",13,10)
    
    reconnect
    
    goto main 
    