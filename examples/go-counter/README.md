# Example: A Counter VM in Go

In `main.go` that accepts increment or decrement transactions,
reads a value from the Deku store, and then writes a
new value in its place.

You can build the example with:
```
go build
```

You can then run the example against a test JSON database using
the Deku CLI. For example:
```
alias deku-cli='nix run github:marigold-dev/deku/parametric#deku-cli --'
action='{"Action": "Increment"}'
db='[["counter", 41]]'
echo "$db" | deku-cli create-mock-transaction ./wallet.json ./vm $action
```
