;; Tip: Zet DrRacket in op de programmeertaal R5RS (linksonderaan).
;; Je mag/kan gebruiken maken van #lang r5rs, maar het inladen van
;; bestanden werkt dan anders dan in standaard R5RS en raden we, als je net
;; begint met het programmeerproject, dus af.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                  ;;;;*----------------------------------*;;;;                  
                  ;;;;*         >>> test.rkt  <<<        *;;;;                  
                  ;;;;* > Programmeerproject 2022-2023 < *;;;;                  
                  ;;;;*                                  *;;;;                  
                  ;;;;*         >>  Versie 1  <<         *;;;;                  
                  ;;;;*                                  *;;;;                  
                  ;;;;*            Written by:           *;;;;                  
                  ;;;;*           Bjarno Oeyen           *;;;;                  
                  ;;;;*                                  *;;;;                  
                  ;;;;*      Software Languages Lab      *;;;;                  
                  ;;;;*----------------------------------*;;;;                  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Met dit bestand kan je snel functionaliteit van de grafische bibliotheek
;; uittesten. Het is dus niet de bedoeling dat je jouw hele spel implementeert
;; door alles globaal te implementeren zonder gebruik te maken van ADTs! Verder
;; hebben we in dit bestand ook veel "magische" constanten staan. De hoogtes en
;; breedtes die in dit bestand gebruikt worden zijn hardgecodeerd. Doe dit niet
;; bij jouw project! Gebruik dit bestand alleen voor te functionaliteit van de
;; grafische bibliotheek uit te testen, niet voor je programmeerproject mee te
;; maken.

;; Hint: voer dit bestand lijn-per-lijn uit. Maak een nieuw bestand aan in
;; dezelfde map waarin je de grafische bibliotheek (Graphics.rkt) hebt staan, en
;; voer elke lijn hieronder in de Read-Eval-Print Loop uit. Of, plaats dit
;; bestand in dezelfde map als de grafische bibliotheek, en maak gebruik van
;; commentaar (; of #| ... |#) rond de stukken code die je niet wil uitvoeren.

;; We hebben bij elk stukje code commentaar geplaatst die uitlegt wat een
;; oproep van een procedure uit de grafische bibliotheek doet. Het is niet de
;; bedoeling dat je jouw project ook van zoveel commentaar voorziet! Maar maak
;; wel gebruik van commentaar om de algemene structuur van je code duidelijk
;; te maken en/of om toelichting te geven bij de implementatie.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; De grafische bibliotheek is geschreven in Racket (een andere variant van
;; Scheme) maar je kan deze perfect vanuit R5RS gebruiken door ze te importeren
;; met een #%require form, deze maakt een Racket-bibliotheek beschikbaar in
;; R5RS-code (als je gebruik maakt van DrRacket).
(#%require "Graphics.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Sidenote ---------------------------------------------------------------;;
;; Je kan ook andere procedures uit Racket importeren met #%require.          ;;
;; Onderstaande lijn code importeert de volgende procedures uit Racket.       ;;
;; - random: genereert random getallen                                        ;;
;;     Bijvoorbeeld:                                                          ;;
;;     (random) ;; -> 0.8110203432599622: een random, inexact, getal tussen 0 ;;
;;                                        (inclusief) en 1 (exclusief).       ;;
;;     (random 100) ;; -> 42: een random, natuurlijk (exact), getal tussen 0  ;;
;;                            (inclusief) en het gegeven getal (exclusief).   ;;
;;     (random 20 30) ;; -> 28: een random, natuurlijk (exact) getal tussen   ;;
;;                              de twee meegegeven getallen (exclusief het    ;;
;;                              hoogste getal). Deze aanroep kan dus enkel    ;;
;;                              getallen tussen 20 en 29 (inclusief)          ;;
;;                              teruggeven.                                   ;;
;;                                                                            ;;
;; - error: stopt de executie van het programma met een foutmelding in de     ;;
;;   REPL.                                                                    ;;
;;     Bijvoorbeeld:                                                          ;;
;;     (error "bericht werd niet begrepen in positie ADT")                    ;;
(#%require (only racket random error))
;; Je mag voor je eigen project ook andere procedures uit Racket importeren   ;;
;; indien je de functionaliteit niet zelf in de specificatie van R5RS kan     ;;
;; terugvinden. Maar hou er wel rekening mee dat procedures van Racket niet   ;;
;; altijd compatibel zijn met values van R5RS. Bijvoorbeeld: Racket heeft de  ;;
;; procedures `(first)` tot en met `(tenth)` om respectievelijk het eerste    ;;
;; tot en met het tiende element van een lijst terug te geven. Deze           ;;
;; procedures werken echter niet met de lijsten (met andere woorden: cons-    ;;
;; cellen) die R5RS gebruikt. In sommige gevallen kan je values van R5RS      ;;
;; omzetten naar values van Racket en omgekeerd, maar dat is niet altijd even ;;
;; eenvoudig.                                                                 ;;
;; Bovenstaande imports worden door `test.rkt` niet gebruikt en zijn          ;;
;; eigenlijk overbodig. Je kan, wanneer je dit programma uitvoert `random` en ;;
;; `error` testen in de REPL. Merk op dat `error` in de REPL niet de REPL     ;;
;; stopt, maar dat deze opnieuw start (in dezelfde Scheme-omgeving).          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; We starten ons programma met een lijn code die een nieuw venster aanmaakt.
;; Onderstaande lijn code maakt een nieuw venster (window) aan van 800 pixels
;; breed en 600 pixels hoog. De titel van de window is "Hallo, wereld!".
(define venster (make-window 800 600 "Hallo, wereld!"))

;; Je kan de achtergrondkleur aanpassen door de `set-background!` message te
;; sturen naar het `venster` object. Deze geeft een procedure terug die, wanneer
;; deze opgeroepen wordt, de kleur van het scherm wijzigt.
((venster 'set-background!) "blue")
;; Bekijk op https://docs.racket-lang.org/draw/color-database___.html welke
;; kleuren je bij naam kan gebruiken (laat wel spaties in de namen weg).

;; Om iets op het scherm te tekenen moet je eerst een laag toevoegen aan
;; een window. Vervolgens kan je aan een laag tiles toevoegen om iets
;; effectief op het scherm te kunnen tekenen. De volgorde waarop je de
;; lagen aanmaakt bepaald de volgorde waarmee ze getekend worden. De eerste
;; laag die je aanmaakt wordt steeds onderaan getekend. De tweede laag wordt
;; bovenop de eerste laag getekend. De derde laag wordt bovenop de tweede laag
;; getekend. Enzoverder.
;; Elke laag heeft een verzameling van tiles die getekend moeten worden. De
;; volgorde waarop tiles op dezelfde laag getekend worden is onbepaald.
;; Als je dus explosies steeds bovenop een bepaald spelelement wil tekenen ga
;; je een explosie-laag moeten voorzien die gemaakt is na de laag waarop deze
;; spelelementen getekend worden.
(define mijn-eerste-laag ((venster 'new-layer!)))
;; Je kan zoveel mogelijk lagen maken als je wil, maar des te meer lagen je
;; hebt des te meer werk de grafische bibliotheek heeft. Maar, teken ook je
;; tiles niet allemaal op dezelfde laag: want elke keer als er 1 tile aangepast
;; wordt (bijvoorbeeld: zijn positie wijzigt) dan moet de gehele laag opnieuw
;; getekend worden door de grafische bibliotheek: als je dus al je tiles op 1
;; laag zet zal dit een negatief effect hebben op de performantie van je spel!

;; Onderstaande lijn maakt een tile aan van 200 pixels breed en 100 pixels
;; hoog. Je kan een tile beschouwen als een "canvas" waarop je kan tekenen,
;; en dat je vervolgens op het scherm kan plaatsen zodanig deze op het scherm
;; getekend wordt (zie verderop).
(define mijn-tile (make-tile 200 100))

;; Een tile is standaard leeg. Je kan op een tile iets tekenen. Dit ga je
;; meestal slechts 1 keer moeten doen (je moet bijvoorbeeld niet voor elke frame
;; of update opnieuw de gehele tile aanmaken en hertekenen!), tenzij je
;; natuurlijk informatie op een tile (bijvoorbeeld de huidige score) wil
;; aanpassen. Onderstaande lijnen code tekenen op de tile een rechthoek (in het
;; rood) en een tekst (in het wit). De volgorde van de parameters (en hun
;; betekenis kan je terugvinden in de documentatie).
((mijn-tile 'draw-rectangle!) 10 10 180 80 "red")
((mijn-tile 'draw-text!) "Hallo!" 12 100 30 "white")
;; In plaats van met `draw-rectangle` en `draw-text` iets op een tile te
;; tekenen kan je ook `(make-bitmap-tile "pad-naar-bitmap.png")` gebruiken om
;; rechtstreeks een afbeelding (bitmap) te laden naar een tile. Dit is doorgaans
;; de makkelijkste manier om iets complex op het scherm te tekenen. Als je
;; gebruik wil maken van transparantie in je afbeelding moet je wel eerst een
;; mask aanmaken, en moet je deze mask ook meegeven aan de aanroep van
;; `make-tile`. Voor meer informatie: lees de documentatie over de grafische
;; bibliotheek. In dit testbestand gebruiken we geen bitmap tiles. Bij wijze van
;; oefening kan je de definitie van `mijn-tile` aanpassen dat die een bitmap
;; tile bevat. Daarvoor moet je een afbeelding plaatsen in dezelfde map als
;; de testcode.

;; Een tile wordt dus pas zichtbaar op een scherm als je hem toevoegt aan een
;; laag. Je wijst een tile best toe aan maximaal 1 laag! Je zal geen foutmelding
;; krijgen wanneer je probeert 1 tile aan meer lagen toe te wijzen, maar het
;; gedrag hiervan is onbepaald. Maak hier dus geen gebruik van!
((mijn-eerste-laag 'add-drawable!) mijn-tile)
;; Tip: met (tile 'clear) kan je alles wat je zelf op een tile getekend hebt
;; met `draw-rectangle` en `draw-text` terug verwijderen. Probeer het eens uit.
;; Voor terug iets zichtbaar te krijgen kan je de oproepen van 'draw-rectangle
;; en 'draw-text opnieuw uitvoeren. Probeer eens de argumenten aan te passen
;; voor te ontdekken wat elke parameter doet. Let wel op... hou rekening mee dat
;; de tile maar 200 pixels breed is en 100 pixels hoog. Iets tekenen daarbuiten
;; gaat dus geen effect hebben. Bijvoorbeeld tekst tekenen op (300, 300) zal
;; niets aanpassen aan de tile.

;; Een tile wordt standaard op positie (0, 0) geplaatst (= linksboven). Je kan
;; de postie wijzigen door er berichten te sturen. De eerste lijn code past de
;; locatie aan op de x-as. De tweede lijn code past de locatie aan op de y-as.
((mijn-tile 'set-x!) 500)
((mijn-tile 'set-y!) 300)
;; Merk op! De coördinaat (0, 0) staat helemaal linksboven in het scherm. Om
;; naar links te gaan moet de x-coördinaat groter worden, om naar beneden te
;; gaan moet de y-coördinaat groter worden. Het assenstelsel ziet er dus als
;; volgt uit:
;;
;;  +---------------------------------------------+
;;  | Hallo, wereld!                        _ ☐ X |
;;  +---------------------------------------------+
;;  | (0, 0) ---> +x                              |
;;  |   |                                         |
;;  |   |                                         |
;;  |   |                                         |
;;  |   v +y                                      |
;;  |                                             |
;; 
;; Dit is dus anders dan bij vele wiskundige assenstelsels, waarbij de
;; verticale as (de y-as) naar boven groter wordt. Indien je dit verwarrend
;; vindt kan je ervoor kiezen om al je spellogica zodanig te schrijven dat de
;; y-as naar boven oploopt, maar dan ga je wel een omzetting moeten doen van
;; coördinaten bij het implementeren van je spellogica.

;; Een spel is natuurlijk pas een spel als er een vorm van interactie aanwezig
;; is. Daarom voorziet de grafische bibliotheek reeds een spellus waaraan je
;; zelf operaties aan kan toevoegen. Een spellus luistert naar de invoer van de
;; speler (bijvoorbeeld een toets die ingedrukt wordt), en bepaalt continu de
;; nieuwe spelsituatie. Onderstaande lijn code registreert een procedure die zal
;; opgeroepen worden door de grafische bibliotheek, elke keer als de speler op
;; een toets op zijn toetsenbord drukt (als het venster in focus is; als de
;; speler een ander programma gebruikt, zoals DrRacket, dan gaat deze procedure
;; niet opgeroepen worden).
((venster 'set-key-callback!)
 (lambda (type toets) (display `(toets ,type ,toets)) (newline)
   (if (and (eq? type 'pressed) (eq? toets 'up))
       ((mijn-tile 'set-y!) 0))))
;; Als je nu op de toets duwt met pijltje naar boven, dan gaat de tile die we
;; gemaakt hebben naar de bovenkant van het scherm.
;; Merk op dat we 2 dingen controleren: we kijken na of de toets die ingedrukt
;; is effectief de pijl-naar-boven-toets is (deze wordt voorgesteld met het
;; symbool 'up) maar we controleren ook of de toets ingedrukt is ('pressed).
;; De procedure zal namelijk ook opgeroepen worden als de toets losgelaten werd
;; (de variabele `type` zal dan 'released zijn).
;; Andere toetsen kan je hier terugvinden: https://bit.ly/2EGZ0yr
;; Merk op, toetsen die overeenkomen met een letter komen niet overeen met een
;; symbool, maar met een karakter. Voor te controleren of de speler de A-toets
;; indrukt controleer je dus niet of de toets gelijk is aan het het symbool 'a
;; (of 'A) maar aan het karakter #\a. Bijvoorbeeld: (eq? toets #\a)
;; Je kan de `set-key-callback!` meermaals uitvoeren, maar de oude procedure
;; "verdwijnt" dan: je kan maximaal 1 (key-)callback procedure hebben. Als je
;; zelf meerdere acties wil kunnen registreren dan ga je de code hiervoor zelf
;; moeten voorzien in de implementatie van jouw spel.

;; De procedure die meegegeven wordt aan de `set-key-callback!`-operatie wordt
;; ook wel eens een callback genoemd. Het is namelijk de grafische bibliotheek
;; die jouw operatie gaat toepassen. Het toepassen of oproepen van een procedure
;; wordt in het Engels ook wel, respectievelijk "to apply a procedure" en "to
;; call a procedure" genoemd. Wanneer de grafische bibliotheek jouw procedure
;; oproept, dan krijgt jouw eigen programme de controle terug (back). Vandaar
;; dus de benaming "callback".

;; We kunnen een eenvoudig "spelletje" maken waarbij de tile die we gemaakt
;; hebben automatisch naar beneden beweegt, en elke keer als de speler op de
;; pijltje-naar-boven-toets duwt gaat deze tile terug naar boven. Een echt spel
;; kan je het misschien niet noemen, maar het illustreert wel hoe je iets kan
;; aanpassen aan de spelsituatie, zonder dat de speler daarvoor op een toets
;; moet drukken.
;; Onderstaande procedure registreert een procedure die na elke update
;; (hertekening) van het venster opgeroepen wordt. Met deze procedure kan je
;; de spelsituatie laten veranderen doorheen het verloop van de tijd.
((venster 'set-update-callback!)
 (lambda (ms)
   (let* ((huidige-y (mijn-tile 'get-y))
          (nieuwe-y (+ huidige-y (* 0.10 ms))))
     ((mijn-tile 'set-y!) nieuwe-y))))
;; De parameter `ms` zal, bij oproep, gebonden worden aan het aantal
;; milliseconden (ms) sinds de vorige aanroep van de procedure die je meegegeven
;; hebt aan `set-update-callback!`. Gebruik deze om te vermijden dat je spel
;; onspeelbaar wordt bij een extreem hoge fps (= frames per seconde). Op een
;; snelle computer zal deze procedure waarschijnlijk veel meer opgeroepen worden
;; dan op een trage computer. Door de tile elke keer maar even ver te laten
;; bewegen in verhouding met de verstreken tijd, zorg je ervoor dat de tile
;; altijd even "snel" beweegt, ongeacht de snelheid van de computer waarop je
;; spel uitgevoerd wordt.
;; Pas bovenstaande constante (`0.10`) aan om de snelheid waarmee de tile
;; beweegt aan te passen. Een grotere waarde zorgt voor een hogere snelheid.
;; In dit voorbeeld vragen we rechtstreeks de positie van de tile op het scherm
;; op. Als je een goede scheiding hebt tussen spel- en tekenlogica, kan je
;; rechtstreeks aan een spelelement zijn positie opvragen, zonder dat je deze
;; informatie uit een tile objectje moet halen.

;; Naast het luisteren van toetsenbordinvoer (met de key callback-procedure) is
;; het ook mogelijk om te luisteren naar interacties met de computermuis. Er
;; zijn hier twee aparte callbacks voor: eentje die luistert naar bewegingen
;; van de muisaanwijzer en eentje die luistert naar het indrukken (en loslaten)
;; van de toetsen op de computermuis. De eerste kan ingesteld worden met
;; `set-mouse-move-callback!`, de tweede met `set-mouse-click-callback!`.
((venster 'set-mouse-move-callback!)
 (lambda (x y)
   (display "De nieuwe positie van de muisaanwijzer is (")
   (display x)
   (display ", ")
   (display y)
   (display ")")
   (newline)))
((venster 'set-mouse-click-callback!)
 (lambda (knop status x y)
   (if (and (eq? knop 'left)
            (eq? status 'pressed))
       (begin
         ((mijn-tile 'set-x!) x)
         ((mijn-tile 'set-y!) y)))))
;; De eerste callback luistert dus naar de bewegingen die de muisaanwijzer
;; maakt. Het krijgt steeds de nieuwe x- en y-waarde van de muisaanwijzer ten
;; opzichte van de positie van je eigen venster. Bovenstaande code gaat er dus
;; voor zorgen dat wanneer de gebruiker van je programma de muisaanwijzer
;; beweegt, dat de coördinaten van de muisaanwijzer geprint worden. Merk op dat
;; wanneer de muisaanwijzer buiten je venster is dat er niets geprint wordt: de
;; procedure wordt alleen opgeroepen wanneer de muisaanwijzer in je eigen
;; venster zit. De tweede callback zorgt ervoor dat wanneer de gebruiker de
;; linkermuisknop indrukt, dat de tile naar de positie van de muisaanwijzer
;; gezet wordt.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Het "spel" zoals het nu gemaakt is heeft een tile die automatisch naar
;; beneden beweegt. Elke keer als de speler op de pijltje-naar-boven-toets drukt
;; zal de tile terug naar de bovenkant van het scherm bewegen en als de
;; gebruiker de linkermuisknop indrukt gaat de tile naar de aangeduide positie
;; verplaatst worden.

;; De grafische bibliotheek heeft ook nog andere functionaliteiten die we hier
;; niet getoond of vermeld hebben. Deze functionaliteiten kan je terugvinden in
;; de documentatie die je op Canvas kan terugvinden.

;; Merk ook op dat de manier hoe we hier dit spel geïmplementeerd hebben niet
;; zorgt voor een goede codekwaliteit. We hebben immers alles rechtstreeks
;; geïmplementeerd zonder na te denken over abstracties en zonder rekening te
;; houden met de scheiding van spel- en tekenlogica. De nieuwe positie van
;; de tile wordt rechtstreeks berekend aan de hand van de huidige positie van de
;; tile.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Oefeningen:
;;
;; Deze oefeningen zijn vrij te maken en worden niet expliciet in een WPO
;; behandeld. We raden aan om deze oefeningen na het WPO te maken waarin de
;; werking van de grafische bibliotheek en het programmeren van objecten in
;; Scheme heeft plaatsgevonden. Indien je vragen hebt over deze oefeningen of
;; over je oplossing dan raden we je aan om naar een WPO Massaprogrammeren
;; te komen.
;;
;; +- Oefening 1 -+
;; Verander de beweging van de tile. In plaats van dat deze altijd naar beneden
;; beweegt kan je...
;; a. ... deze laten bewegen in een richting bepaald door de speler (gebruiker
;;        van de applicatie. Bijvoorbeeld door gebruik te maken van de
;;        pijltjestoetsen. Wanneer de speler op de linkerpijlknop drukt beweegt
;;        de tile naar links. (Moeilijkheid: 1/3)
;; b. ... de tile laten bewegen in een cirkel. Maak gebruik van `sin` en `cos`
;;        om de de tile correct te laten bewegen. Hint: maak gebruik van een
;;        globale variabele de verstreken tijd bijhoudt.
;;        (Moeilijkheid: 3/3)
;; c. ... de tile laten bewegen op een grid van 16x16 pixels. Je kan de positie
;;        afronden zodanig dat deze steeds op een grid van 16 bij 16 pixels
;;        groot beland. (Moeilijkheid: 2/3)
;; d. ... de richting willekeurig laten veranderen op willekeurige momenten, of
;;        wanneer de tile buiten het scherm beweegt. (Moeilijkheid: 1.5/3)
;;
;; +- Oefening 2 -+
;; Verander de tile met een bitmap tile door gebruik te maken van de procedure
;; `make-bitmap-tile` uit de grafische bibliotheek.
;; (Moeilijkheid: 1/3)
;;
;; +- Oefening 3 -+
;; Verander de achtergrondkleur van het spel wanneer de gebruiker een toets
;; indrukt.
;; a. maak gebruik van de toetsen op het toetsenbord
;; b. maak gebruik van de knoppen op de muis (of touchpad)
;; (Moeilijkheid: 1/3)
;; 
;; +- Oefening 4 -+
;; Maak een nieuwe tile en plaats deze op een laag (dit mag de bestaande laag
;; zijn). Laat de gebruiker tekst intypen en teken deze tekst op de tile.
;; Je zal hiervoor een variabele moeten voorzien met daarin een string in die
;; aangepast wordt als de speler gebruik maakt van het toetsenbord.
;; (Moeilijkheid: 2/3)
;; Bonus 1: Wanneer de speler op de enter-toets drukt verander je de titel van
;; het venster naar de ingegeven tekst en reset je de string (Moeilijkheid: 1/3)
;; Bonus 2: Wanneer de speler op de backspace-toets drukt verwijder je het
;; laatste karaketer (Moeilijkheid: 1/3)
;;
;; +- Oefening 5 -+
;; Maak een nieuwe tile aan die bewogen kan worden door de speler wanneer deze
;; de linkermuisknop ingedrukt houdt. Wanneer de knop losgelaten wordt stopt
;; de tile met bewegen.
;; (Moeilijkheid: 3/3) 
;;
;; +- Oefening 6 -+
;; Maak een tile die een pijl bevat (ofwel via een bitmap, ofwel via meerdere
;; oproepen van `draw-line!` uit het Tile ADT) en plaats deze op het scherm.
;; Vervolgens schrijf je de code die ervoor zorgt dat de tile steeds naar de
;; muisaanwijzer wijst door de tile te roteren. Wanneer de muisaanwijzer verder
;; weg is maak je de tile groter dan wanneer deze dichtbij is.
;; (Moeilijkheid: 2/3)
;; Bonus: Laat de speler gebruik maken van de pijltjestoetsen om de tile te
;; bewegen. Wanneer de speler de toets indrukt begint de tile te bewegen in de
;; gekozen richting, wanneer de speler de toets loslaat stopt deze met bewegen.
;; (Moeilijkheid: 2/3)
