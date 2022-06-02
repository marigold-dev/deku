# Implementation of FA2 standard for deku-p in typescript

# How to use ?

```
$ npm install
$ npm run build
```

Then choose an action to perform

## Balance of
```
action='{"type": "balance_of", "op": {"requests": [{"owner":"tz1c5ZDfNvSrfRFAsp6uPYvCaZhpviwYpBfy", "token_id": 1}], "callback": {"address": "callback_address"} } }'
```

## Transfer
```
action='{"type": "transfer", "op": [{"from": "tz1c5ZDfNvSrfRFAsp6uPYvCaZhpviwYpBfy", "txs": [{"to": "alice", "token": {"token_id": 1, "amount": 1}}]}]}'
```

## Remove operator
```
action='{"type": "update_operators", "op": [{"type": "remove", "operator": {"owner": "tz1c5ZDfNvSrfRFAsp6uPYvCaZhpviwYpBfy", "1": {"operator": "alice", "token_id": "1"}}}]}'
```

## Add operator
```
action='{"type": "update_operators", "op": [{"type": "add", "operator": {"owner": "tz1c5ZDfNvSrfRFAsp6uPYvCaZhpviwYpBfy", "1": {"operator": "alice", "token_id": "1"}}}]}'
```

## Mint a token operator
```
action='{"type": "mint_token", "op": [{"token_id": 123, "amount": 42, "token_metadata": {"": "", "symbol": "a new token", "name": "my new token", "decimals": ""}}]}'
```

You can then run the example against a test JSON database using
the Deku CLI. For example:
```bash
alias deku-cli='nix run github:marigold-dev/deku/parametric#deku-cli --'
db='[["tz1c5ZDfNvSrfRFAsp6uPYvCaZhpviwYpBfy-1", {"amount": 1, "operators": []}], ["1", {"": "", "symbol": "my custom symbol", "name": "my super token", "decimals": ""}], ["administrator", {"value": "tz1c5ZDfNvSrfRFAsp6uPYvCaZhpviwYpBfy"}]]'
echo "$db" | deku-cli create-mock-transaction ./wallet.json ./vm $action
```