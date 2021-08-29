# DrRacket

## Keybinding

File `keybinding.rkt` :

    #lang s-exp framework/keybinding-lang


    (text:get-completions/manuals false)

    (keybinding "m:TAB" (λ (editor event) (send editor auto-complete))) 

    (keybinding "c:m:l" (λ (editor event) (send editor insert "λ")))


