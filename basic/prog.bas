#picaxe 18m2
#no_data

main:
    disconnect
    serrxd b0
    let pins= b0
    ;if #b0=1 then high B.0 else low B.0 endif
    sertxd ("Commande ",b0," recue")
    pause 100
    reconnect
    goto main 
