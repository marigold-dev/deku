npm install

opam exec -- dune exec -- ligo man --format html | xargs -0 -I {} node manpages.js {}