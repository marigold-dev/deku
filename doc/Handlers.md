# Handlers

Each Deku-node exposes several HTTP/TCP endpoints to interact with the chain.
They are available within [deku_node.ml](../src/bin/deku_node.ml)
Here are listed each of them, with a `curl` example command to play with them.

Since we use RPC, all these endpoints are POST (only verb available over RPC).

## Prerequisites

In order to be able to play the following `curl` examples, you must have

- run `docker compose upd -d`
- run `./sandbox.sh setup`
- run `./start.sh`

## `/append-block-and-signature`

### Purpose

Check if the provided block is valid, where `valid` means:

- Height of the provided block is higher that the current height
- Each operation of this block is properly signed

### Expected data

- A [block](../src/protocol/block.mli#L2)
- The corresponding [signature](../src/protocol/protocol_signature.ml#L3)

### curl example

```shell script
$ curl -v -X POST http://127.0.0.1:4440/append-block-and-signature -d
  '"block": {
    "hash": "940eac02291b9b4fb1f02918830a20e28f484f9598ffffabb5f9dbcc6f56d3e1",
    "payload_hash": "86ed99d235178e6f32bcc5b5dea894f6523bd8da4e5f98b62318c3a2138d086c",
    "state_root_hash": "d4393a4cb29a09b62fbc4edc1b8414b27189bc0f6f569a205cebb1f2598e2047",
    "withdrawal_handles_hash": "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
    "validators_hash": "d139b7cda8577e16c79084a253d6e9ad5cfeb07e2e5ad46223fa0f87c500b9d8",
    "previous_hash": "373e143b34f9be47404e9b0203857c1f55647cb55b5357bf38fd3d6d40c771d3",
    "author": "tz1PmKMKTbSVcam69ssKUtmSpeBRkYPTBPvW",
    "block_height": 56754568,
    "operations": []
  },
  "signature": {
    "signature": "edsigtujsQZBBvVy7p6oKbp44c9KP8qhkEfDrPh3W4hae8zmF1knPxVNTDZHhix3c7triJtVhPvDeWegrGaC7Xtfmy7iebhe75P",
    "public_key": "edpkuqseqRnfW5ZWwpp2kWjdY2Tma8A8xhNND2Xgoos49gApQb46LP"
  }
}' -H "Content-type: application/json"

*   Trying 127.0.0.1:4440...
* Connected to 127.0.0.1 (127.0.0.1) port 4440 (#0)
> POST /append-block-and-signature HTTP/1.1
> Host: 127.0.0.1:4440
> User-Agent: curl/7.74.0
> Accept: */*
> Content-type: application/json
> Content-Length: 880
>
* upload completely sent off: 880 out of 880 bytes
* Mark bundle as not supporting multiuse
< HTTP/1.1 200 OK
< Content-Type: application/json
< Content-Length: 4
<
* Connection #0 to host 127.0.0.1 left intact
null
```

## `/append-signature`

### Purpose

Add the provided signature to the provided hash.

### Expected data

- An hash of type `BLAKE2B.t`
- The corresponding [signature](../src/protocol/protocol_signature.ml#L3)

### curl example

```shell script
$ curl -v -X POST http://127.0.0.1:4440/append-signature -d '{"hash":"c6c1aa9fd3dbb1eb8cfc319b4524ddfb6ff6e26edb196ce0310a56c5ac1d7934","signature":{"signature":"edsigtmjX41ajHPEnUTohQ4Jjy2d1LWPdSb4u2GrvwgLk4aoHFwMVDadw5zRepHyK1mcbUTJ6PmxKe4PaoyivJtYPJevuiL8iTq","public_key":"edpkuy1Qjwo4X7a3r1wA1HLAx9s5kgp7b9xUxfSpQQcTpFnqyzyrzH"}}' -H "Content-type: application/json"

*   Trying 127.0.0.1:4440...
* Connected to 127.0.0.1 (127.0.0.1) port 4440 (#0)
> POST /append-signature HTTP/1.1
> Host: 127.0.0.1:4440
> User-Agent: curl/7.74.0
> Accept: */*
> Content-type: application/json
> Content-Length: 273
>
* upload completely sent off: 273 out of 273 bytes
* Mark bundle as not supporting multiuse
< HTTP/1.1 200 OK
< Content-Type: application/json
< Content-Length: 4
<
* Connection #0 to host 127.0.0.1 left intact
null
```

## `/block-by-hash`

### Purpose

Retrieve block corresponding to the provided hash.

### Expected data

The hex representation of the Hash from which one we want to retrieve the block.

### Returned data

JSON of the corresponding [block](../src/protocol/block.mli#L2).

### curl example

```shell script
$ curl -v -X POST http://127.0.0.1:4440/block-by-hash -d '{"hash":"d1f1f05746b178cbfafa88814cf518a38e26ff41d03e5e7c3a323584844961a4"}' -H "Content-type: application/json"

*   Trying 127.0.0.1:4440...
* Connected to 127.0.0.1 (127.0.0.1) port 4440 (#0)
> POST /block-by-hash HTTP/1.1
> Host: 127.0.0.1:4440
> User-Agent: curl/7.74.0
> Accept: */*
> Content-type: application/json
> Content-Length: 75
>
* upload completely sent off: 75 out of 75 bytes
* Mark bundle as not supporting multiuse
< HTTP/1.1 200 OK
< Content-Type: application/json
< Content-Length: 586
<
* Connection #0 to host 127.0.0.1 left intact
{
  "hash": "d1f1f05746b178cbfafa88814cf518a38e26ff41d03e5e7c3a323584844961a4",
  "payload_hash": "5dd264b5aa3252d55ab6295e181fec4654a5c220a653bcdb61c306e2b4ea6749",
  "state_root_hash": "a44efb410e5be9deb38b3d80c3b750d89bcbf009292359878397c9a50dc680a5",
  "withdrawal_handles_hash": "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
  "validators_hash": "d139b7cda8577e16c79084a253d6e9ad5cfeb07e2e5ad46223fa0f87c500b9d8",
  "previous_hash": "f4ae816df742ec2588c90b838481e57fc3ff0f21e6b55b9668f612413a7d6ec9",
  "author": "tz1PmKMKTbSVcam69ssKUtmSpeBRkYPTBPvW",
  "block_height": 373,
  "operations": []
}
```

## `/block-level`

### Purpose

Retrieve the current height of the chain

### Expected data

A `null` JSON value.

### Returneed data

The current height of type `int64`

### curl example

```shell script
$ curl -v -X POST http://127.0.0.1:4440/block-level -d "null" -H "Content-type: application/json"

*   Trying 127.0.0.1:4440...
* Connected to 127.0.0.1 (127.0.0.1) port 4440 (#0)
> POST /block-level HTTP/1.1
> Host: 127.0.0.1:4440
> User-Agent: curl/7.74.0
> Accept: */*
> Content-type: application/json
> Content-Length: 4
>
* upload completely sent off: 4 out of 4 bytes
* Mark bundle as not supporting multiuse
< HTTP/1.1 200 OK
< Content-Type: application/json
< Content-Length: 13
<
* Connection #0 to host 127.0.0.1 left intact
{
    "level":772
}
```

## `/protocol-snapshot`

### Purpose

Retrieve the snapshot of the protocol.

### Expeted data

None.

### Returned data

- The current [snapshot](../src/node/snapshots.mli#L3)
- The additional blocks (which are not yet added to the chain)
- The last added block
- The signature of the last added block

### curl example

```shell script
$ curl -v -X POST http://127.0.0.1:4440/protocol-snapshot -d "null" -H "Content-type: application/json"
*   Trying 127.0.0.1:4440...
* Connected to 127.0.0.1 (127.0.0.1) port 4440 (#0)
> POST /protocol-snapshot HTTP/1.1
> Host: 127.0.0.1:4440
> User-Agent: curl/7.74.0
> Accept: */*
> Content-type: application/json
> Content-Length: 4
>
* upload completely sent off: 4 out of 4 bytes
* Mark bundle as not supporting multiuse
< HTTP/1.1 200 OK
< Content-Type: application/json
< Content-Length: 20430
<
{
  "snapshot": {
    "hash": "7158d5b36d2f7ac85c84d2e5c46e50479757c9e3769d67e3b7a72a114e316776",
    "data": "[{\"ledger\":{\"ledger\":[],\"withdrawal_handles\":{\"tree\":[\"Empty\"],\"top_bit\":0,\"last_key\":-1}}},[{\"hash\":\"0c1b39f232095618a08e2af196af9a2f25ff609064eae6d7a14a42a5d572d2c2\",\"payload\":{\"tezos_operation_hash\":\"oo6q5D8s927dCiUQ6XEmMEvPB7DBv8tpHrq4Ny7fmjoH73XMYME\",\"internal_operations\":[]}},{\"hash\":\"0d20e0c549fe04b893b54fbc25ae0861cb1e43876e65e033d50cd4cd58a4330c\",\"payload\":{\"tezos_operation_hash\":\"onvwCUrttAYph27HHtxz5SHDKXHVWTBcdA1QDvYGPX2bGc2Xis7\",\"internal_operations\":[]}},{\"hash\":\"0e3777127386654713fdcd48a12acfcbe344254cba063632c37d8c8689430926\",\"payload\":{\"tezos_operation_hash\":\"onrCs7Rh4ezJjFkbyEqoWmNmwQ9r8XGdkkch6mJRe3TVFxyQh94\",\"internal_operations\":[]}},{\"hash\":\"0f261836b713d5c54f3961462913ad0efc951668db8ece7a05d2006890c0e328\",\"payload\":{\"tezos_operation_hash\":\"ooZEoKWiw9AazkQsLGuWXLkV3dgy4Ss6Qqbw3bFTZ9J8HwvBLM5\",\"internal_operations\":[]}},{\"hash\":\"17311a506eb95048baa2116cc4b5482155644d16d098230f648cf70a86717e9a\",\"payload\":{\"tezos_operation_hash\":\"oon4CHYxs7YCCCxbWZq4TYytRWmcRJN98zXmLie6MQ6UWPCLKJw\",\"internal_operations\":[]}},{\"hash\":\"17768135e67cd5f6aa8a960ac55191a5906d8599797ba242572f06454d9fbdf9\",\"payload\":{\"tezos_operation_hash\":\"op9RsuKpzb7pwjHnr6uW91UqeuxaGCFY52psMNQfqzhvzDJ47eV\",\"internal_operations\":[]}},{\"hash\":\"2044c94027d25816d5eff4b01187b5885dc7b11ee9cf795a37edfa5fc14bbd38\",\"payload\":{\"tezos_operation_hash\":\"onhq9CV8pwM8ShsmG5FezeNgDoHJ7vdqa1cotTMpXfcGk6AVCdU\",\"internal_operations\":[]}},{\"hash\":\"25c471d41303108a314d94c5745526a7fe0b7233832b55c27c29ad28669bfa90\",\"payload\":{\"tezos_operation_hash\":\"onzMENYrfPaBo9ZovhkovLDPVf6dwMxs9uabUiP7aQYdvq3deEt\",\"internal_operations\":[]}},{\"hash\":\"274c4a5fd148b862cc2ec1e3330b9634e33095c697e384a7a40be69477c2f078\",\"payload\":{\"tezos_operation_hash\":\"oo3f6aXeVjzTcRCnbzMBM6VXrFxWFDLz3dVdCmkQxM8kQ3MXwjD\",\"internal_operations\":[]}},{\"hash\":\"2ebf10c2ea281e8c4937269bbd21085a94f0ff3608a9f733b306140ff5a1447d\",\"payload\":{\"tezos_operation_hash\":\"ooqPgjJr8wLxvuviRNbvsSwPKpA5LLdGd7zThH8W747LBC715Lv\",\"internal_operations\":[]}},{\"hash\":\"43cfec9b2d36c35dd396aa98756a11221242610da10872826a9efebeaaccce6b\",\"payload\":{\"tezos_operation_hash\":\"op2pAwmvscjyaFxyschYDWHwurGcXRtmUd9gpVaEdqaYV9pF2FQ\",\"internal_operations\":[]}},{\"hash\":\"51c5cfd8b3419640603492a299edd7dd99f31aae8b55f42f7572f63673ab2aff\",\"payload\":{\"tezos_operation_hash\":\"ooJ9Rq3Fjz2iEZA4cMmJXdeodvbPgWpPzVCwYMYF8cZHdLr1PvF\",\"internal_operations\":[]}},{\"hash\":\"573ece617bbb2f6416af40d70cc6bc4e77ec3a7f97f44cd25165c0b0016c1c88\",\"payload\":{\"tezos_operation_hash\":\"oo3L4R3u4NXCKLr5Sg4hDieabjJvN8LHhQ2rewJtRoYfWv4wAqv\",\"internal_operations\":[]}},{\"hash\":\"5dfca89951bddaf3eda8f80f2c483ae7ad98d056eb385db4c9099e0dabe4df0a\",\"payload\":{\"tezos_operation_hash\":\"oo2CNPw178EiHfAoXef8HL5BAL9dwtKhgzyXLCY9CqPacoP7ktL\",\"internal_operations\":[]}},{\"hash\":\"69465b59190235126ed8cdeab6d478502aeef42f3829deff54ae70e709462a25\",\"payload\":{\"tezos_operation_hash\":\"ooE14uGzg9KDyXgne4q1jeAV6QsoumLqcQbBhaLPfeWMcYBhAXD\",\"internal_operations\":[]}},{\"hash\":\"6d6c53ea7f177fb6d5c7370e9623c4a4bebdddec69e71ad5b85394c87077cb76\",\"payload\":{\"tezos_operation_hash\":\"onnKWbENHTMW3dcwZRRiz9hfR5632kCkKnkdLrTCmpe28UhSy6T\",\"internal_operations\":[]}},{\"hash\":\"789f58d3382da341a182eba5a25911a0dcca5a8c037d87c9c1688e0e75d35cc3\",\"payload\":{\"tezos_operation_hash\":\"opZuRxvYJF3sQQ2frB2MoePnnmBnuGcZuAwV9B7CAdSkkra5Ch3\",\"internal_operations\":[]}},{\"hash\":\"81d66c840d5f6fa0b0f4534fffb1ed748cd36776b0361a2c408af0538eee55f1\",\"payload\":{\"tezos_operation_hash\":\"onrUhNAS9BZwysPEw1pRxNYh8DkfBoSVWkF6aZHKcF8K63AJasA\",\"internal_operations\":[]}},{\"hash\":\"83a6ff002f08724c42823f0452e5d706654d51b9d1afc010d13b3183379d6b69\",\"payload\":{\"tezos_operation_hash\":\"ooRS7GihixT2z8iELN7u94N17mEwzQUxJfZ77GXHkAKs5UfsGob\",\"internal_operations\":[]}},{\"hash\":\"87ff051dba57ad9163889b89addafba86725232081fae4f0e2f5689df1bba9df\",\"payload\":{\"tezos_operation_hash\":\"ooXzWzQGFNYqH7N3zFZJ7Q8jY3DirsjcVuUgHvXHdbLAbHe3MVX\",\"internal_operations\":[]}},{\"hash\":\"8e343e62550c07e2e5c45ab25ea80daf6ec66746b7b469b828f00227ddd5e6e2\",\"payload\":{\"tezos_operation_hash\":\"ooycznk3PXxkpAY6wA84LKFVeWDGCsrsHmCArLHz2NkATm3DM7R\",\"internal_operations\":[]}},{\"hash\":\"95c85653255eff1285164c91137028bc0ed7e5fb8cac8e9208a45d2ded34c97e\",\"payload\":{\"tezos_operation_hash\":\"ooh4TsNUGBe1grHqa1j8EXV1X4Nj5xF2DPHXPTA6MCGvqqBdMAP\",\"internal_operations\":[]}},{\"hash\":\"9b5b6639958d13f63b462f7472c80668ecd485ed661865d048e67e430189b23b\",\"payload\":{\"tezos_operation_hash\":\"oni8oWkpU1hnqppSdNS1pAYJNS9eUuMPXzozhuc3Ltz3fmEvn1T\",\"internal_operations\":[]}},{\"hash\":\"9c066963fe283bc5a0a882e8974993142b4efe3f8144fca9590757fb89a2b957\",\"payload\":{\"tezos_operation_hash\":\"ooZhv2EkeHPR4wdVT8wA9TupjcZCxDv9KmwpsWvwvtFVPDSWZ3S\",\"internal_operations\":[]}},{\"hash\":\"9ca7831087de60aa836499199c4c7104aaf23fc137bb721bb82f2f53bc522388\",\"payload\":{\"tezos_operation_hash\":\"onmy9P1FEw4dt7LaL7mu9Zn7fS1Mf46gNV2qgun5sabhZF8ULMS\",\"internal_operations\":[]}},{\"hash\":\"a00be69053f5fbc573e748012eca852cc3bbbf782f2eb570aefb6dc3ab3dffc4\",\"payload\":{\"tezos_operation_hash\":\"opP2xBXZTvmSczknGiDFprt66jkxco78v8VR9FUd2hAxQ99nsZZ\",\"internal_operations\":[]}},{\"hash\":\"a2976e6ac556674b81b465c4a24c2ce6bbf559828d4b7b370e185890279083b2\",\"payload\":{\"tezos_operation_hash\":\"opAH49kLUYrmKodgEewRyyiKdDF9efHVQr1HhCi8V5iDMrivWcK\",\"internal_operations\":[]}},{\"hash\":\"a9d518f46404d312731e26d9f555a75932defb8b9838e175f7ff5a1d00da1a2a\",\"payload\":{\"tezos_operation_hash\":\"opDdceJ7EBum5er3TAv61p6NEYk3taFZyY6JSBCykcDzpwBbhwe\",\"internal_operations\":[]}},{\"hash\":\"ae8f090733e99e1508439a8764fbe54278093025efd437a71e2459a308ecb5f1\",\"payload\":{\"tezos_operation_hash\":\"opakwtcnQ8ffpVefRcdSXpLysuX66KfsKxk5hgB9cKAWuznTsYr\",\"internal_operations\":[]}},{\"hash\":\"af2d8acb491f37f28c544bff952b86e3c0bdd9566d7e1c35e33fd35745091553\",\"payload\":{\"tezos_operation_hash\":\"onu71bmxYvegVx1hfNQVgDm1GdVsyFZAYbqENQgNMzWPEi6Jjd9\",\"internal_operations\":[]}},{\"hash\":\"b05547b4235ebc61658e3cc05dfd42eacdb0bd99f29e1ac5afef0c2a5a258bb8\",\"payload\":{\"tezos_operation_hash\":\"onuPULVSqqsfxW8RxUKdG8jxfahK7RqYWcAXnRWKCZwpq2XRKvV\",\"internal_operations\":[]}},{\"hash\":\"ba6283795c49f2b9297b213217cd127b594a4285f555416914b630bb795ae593\",\"payload\":{\"tezos_operation_hash\":\"onfD7n55oee2XamcjLTiAvFKaZbjj5SZyjJfzFzwR6v5unpQ1Yq\",\"internal_operations\":[]}},{\"hash\":\"bb80af340cd84e2cd6f8fe07a5ffebc6404cd2bf4895c8db9b078fcc3ad56c3e\",\"payload\":{\"tezos_operation_hash\":\"opYxBF6KAojhrescwoUsCzZe8KvD6r8re9233MngC2XHVwWT1Uu\",\"internal_operations\":[]}},{\"hash\":\"d7aead24710d3124f08a0bce0626fd71ba619f45d503793f0004eead460f1d37\",\"payload\":{\"tezos_operation_hash\":\"op9E4pExNxHgDREKnUdRpQTuc9LMQP7vTZz2BLL36dFSiDHvWp9\",\"internal_operations\":[]}},{\"hash\":\"e1c7ece848b7123018e2521d639a005ea5c4345b8fbbe3651b59a01add3a1502\",\"payload\":{\"tezos_operation_hash\":\"ooH2fvvwkdaMwuMno3PF8LFVE2DPPmkWHr669sNW23DBsALRT55\",\"internal_operations\":[]}},{\"hash\":\"ee05d9f1e15f3ed7dc03ac3696d0c65125c4b457fdc58e9df8b063de4262f190\",\"payload\":{\"tezos_operation_hash\":\"ooesAzbNQCuMivCqE9MyAbsXnRXBy7jorCHkJcXvPuTrVYsDubV\",\"internal_operations\":[]}},{\"hash\":\"fcc05addd22e143fa137450ead1a0ba061fff9ca9f12693a4806c8fdf4638e57\",\"payload\":{\"tezos_operation_hash\":\"onnNdjy6yh8BDqXiUWMtWiEgxfNb9yjQkPmBHogXRbFRhuZv9FA\",\"internal_operations\":[]}}],[],{\"current\":{\"address\":\"tz1PmKMKTbSVcam69ssKUtmSpeBRkYPTBPvW\"},\"validators\":[{\"address\":\"tz1PmKMKTbSVcam69ssKUtmSpeBRkYPTBPvW\"},{\"address\":\"tz1TKGjSk8gpuuwK82fh36hs2fUUVBEvWCzk\"},{\"address\":\"tz1LmkDPkkEAU3QB3oBt7tH3vQ4S9RzkoDjc\"}],\"length\":3,\"hash\":\"d139b7cda8577e16c79084a253d6e9ad5cfeb07e2e5ad46223fa0f87c500b9d8\"},\"d139b7cda8577e16c79084a253d6e9ad5cfeb07e2e5ad46223fa0f87c500b9d8\",2129,\"2e55eb44a57ace271f9c778564e1633278270392a1a2d3c6ebfe9e92bc0ac293\",\"9511f19e526994ed0c76a80273ff578db48323246c70d448e9600589577bc235\"]"
  },
  "additional_blocks": [
    {
      "hash": "28f8c0e42b627e9357f65c3d759e26e36cbf5e7d1c95fb1326f38477cd46b4c6",
      "payload_hash": "2f338fa547652b58b267669c37dd477d4ea9eecf4b4027c2ea996421ce378082",
      "state_root_hash": "7158d5b36d2f7ac85c84d2e5c46e50479757c9e3769d67e3b7a72a114e316776",
      "withdrawal_handles_hash": "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
      "validators_hash": "d139b7cda8577e16c79084a253d6e9ad5cfeb07e2e5ad46223fa0f87c500b9d8",
      "previous_hash": "0486b61be7f424f6f2ede3f5dbd0c68990e7dce51e26463b73bade7767f6be68",
      "author": "tz1PmKMKTbSVcam69ssKUtmSpeBRkYPTBPvW",
      "block_height": 2148,
      "operations": []
    },
    {
      "hash": "0486b61be7f424f6f2ede3f5dbd0c68990e7dce51e26463b73bade7767f6be68",
      "payload_hash": "751be9f3529084c4fc34f06b87c1a36477357b6412052abb2542952af35955ff",
      "state_root_hash": "7158d5b36d2f7ac85c84d2e5c46e50479757c9e3769d67e3b7a72a114e316776",
      "withdrawal_handles_hash": "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
      "validators_hash": "d139b7cda8577e16c79084a253d6e9ad5cfeb07e2e5ad46223fa0f87c500b9d8",
      "previous_hash": "5d6109c326976a0b831b0a613b7174599c19d9f215d532bcbcacb161f8dec644",
      "author": "tz1PmKMKTbSVcam69ssKUtmSpeBRkYPTBPvW",
      "block_height": 2147,
      "operations": []
    }
  ],
  "last_block": {
    "hash": "cf73d4db3c4640116651d23d6791c60a872c9d74f9ee803fc3067a89f2f9cd85",
    "payload_hash": "040f080a0833bdf14b2b04a857d39ae91c0597d5f759b68add1a353c3fc9aa99",
    "state_root_hash": "7158d5b36d2f7ac85c84d2e5c46e50479757c9e3769d67e3b7a72a114e316776",
    "withdrawal_handles_hash": "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
    "validators_hash": "d139b7cda8577e16c79084a253d6e9ad5cfeb07e2e5ad46223fa0f87c500b9d8",
    "previous_hash": "28f8c0e42b627e9357f65c3d759e26e36cbf5e7d1c95fb1326f38477cd46b4c6",
    "author": "tz1PmKMKTbSVcam69ssKUtmSpeBRkYPTBPvW",
    "block_height": 2149,
    "operations": []
  },
  "last_block_signatures": [
    {
      "signature": "edsigtamTx6dHvcRg9oT3kMr1R8UpNGwuKVJZ4mMKHddkiJwRH1k3ZjBwHE6U9b6JcG765TQvnxnYGZKj2R1nYJCtmvLDWYE4re",
      "public_key": "edpkuqseqRnfW5ZWwpp2kWjdY2Tma8A8xhNND2Xgoos49gApQb46LP"
    },
    {
      "signature": "edsigtqw2eeXbeDXrgumtP7tG5SroLnvHsPmeLmJtPvuo6bbqSP5VsBdvvKkRH2U146nwMTvTgj9dBJhGem9QH2QJJbzh2Vosxc",
      "public_key": "edpkuy1Qjwo4X7a3r1wA1HLAx9s5kgp7b9xUxfSpQQcTpFnqyzyrzH"
    }
  ]
}
```

## `/request-nonce`

### Purpose

??

### Expected data

A URI?

### Returned data

The nonce of type `BLAKE2B.t`

### curl example

```shell script
$
```

## `/register-uri`

### Purpose

??Add the provided URI as a new validator??

### Expected data

- A URI
- The corresponding signature

### curl example

```shell script
$
```

## `/user-operation-gossip`

### Purpose

Add a user operation to the pending operations.
If the operation does not already exist, it is broadcasted to the gossip network

### Expected data

A [`core_user.t`](../src/protocol/protocol_operation.mli#L14).

### curl example

```shell script
$
```

## `/consensus-operation-gossip`

### Purpose

Like a user operation, but from consensus: add a consensus operation to the pending operations.

### Expected data

- The [consensus operation](../src/protocol//protocol_operation.mli#L3) to add
- The corresponding signature

### curl example

```shell script
$
```

## `/trusted-validators-membership`

### Purpose

Manage the trusted validators. It is possible to `add` or `remove` a validator.

### Expected data

- An [`action`](../src/node/networking.ml#L135)
- The corresponding [`key_hash`](../src/crypto/key_hash.mli#L#)

### Returned data

- The signature
- The provided payload (action + key_hash)

### curl example

```shell script
$
```

## `/withdraw-proof`

### Purpose

????

### Expected data

The hash of the operation to withdraw?

### Returned data

????

### curl example

```shell script
$
```

## `/ticket-balance`

### Purpose

????

### Expected data

- An address of type `Key_hash.t`
- A ticket id

### Returned data

The amount

### curl example

```shell script
$
```
