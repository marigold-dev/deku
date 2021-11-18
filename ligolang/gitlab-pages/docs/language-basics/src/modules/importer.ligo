#import "imported.ligo" "EURO"
type storage is EURO.t

function main (const action : unit; const store : storage) : (list (operation)) * storage is
 ((nil : list (operation)), EURO.add(store, EURO.one))
