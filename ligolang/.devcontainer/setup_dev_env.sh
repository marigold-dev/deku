opam init --disable-sandboxing --bare -y 

echo "Setting up dev switch"
opam update 
./scripts/setup_dev_switch.sh 


export PATH=~/.cargo/bin:$PATH

opam install -y --deps-only --with-test --locked=locked ./ligo.opam 


setup_opam="eval \`opam config env\` "
echo $setup_opam >> ~/.bashrc
echo $setup_opam >> ~/.zshrc
