# HTTP APIs exposed by Deku

## /block-level

Retrieve the current block-level:

```shell script
$ curl http://localhost:4440/block-level -d "null"
```

Which response is the level wraped in a JSON structure:

```JSON
{
    "level": 232
}
```

## /protocol-snapshot

Get the snapshot of the protocol (last block and associated signature)

```shell script
$ curl http://localhost:4440/protocol-snapshot -d "null"
```

Which response is a JSON containing:
- data
- hash of the data

```JSON
{
    "snapshot": {
        "hash": "27c2b63640a7a3f29ab8c6fdbdb321185e58a2537f66ae461888c90692a3d360",
        "data": "[{\"ledger\":{\"ledger\":[],\"withdrawal_handles\":{\"tree\":[\"Empty\"],\"top_bit\":0,\"last_key\":-1}},\"vm_state\":[[\"tz1VULT8pu1NoWs7YPFWuvXSg3JSdGq55TXc\",{\"cookie_baker\":{\"number_of_cookie\":0,\"number_of_cursor\":0,\"number_of_grandma\":0,\"number_of_farm\":0,\"number_of_free_cursor\":0,\"number_of_free_grandma\":0,\"number_of_free_farm\":0,\"cursor_cost\":0,\"grandma_cost\":0,\"farm_cost\":0,\"cursor_cps\":0,\"grandma_cps\":0,\"farm_cps\":0}}]]},[{\"hash\":\"17ce9e0e7eeec6a36948e9ee02b30972e031fcdde3c80f5785510e0324bfbd39\",\"payload\":{\"tezos_operation_hash\":\"opSFQVBythV25zawFjpLAmetfERfXT8LQK8tcwktw8ZawXADMCn\",\"internal_operations\":[]}},{\"hash\":\"19dba394881f97c58ee09be85ff5912a86dfef9cec61ff24ab1f3cc89d196507\",\"payload\":{\"tezos_operation_hash\":\"ooVEqsqtdPkg6WapCAscojknytJB9EAwwBzRQmsezuJ4zD2NqGL\",\"internal_operations\":[]}},{\"hash\":\"23b6b0ad434885c0e4f06e77e0d99df1c78ea737097bc857141ba2ec581fb22c\",\"payload\":{\"tezos_operation_hash\":\"ooTfmdx5SHdApHa2ed9UT6jdLUsBuMThyo4Uf7hQNDgTt9zmz4h\",\"internal_operations\":[]}},{\"hash\":\"5d3b32efa6cefdd2c4ea319a9b617b7847c4b6b684c67183f58f33db5aa83c04\",\"payload\":{\"tezos_operation_hash\":\"ooKJvkak5s11PkppG4ePfXAKNt8jDbwiPaaSnWP1CJhg2brkupU\",\"internal_operations\":[]}},{\"hash\":\"660422f4f128e4175427d07290ed34718ca6d00393ab889defd720b22af08f66\",\"payload\":{\"tezos_operation_hash\":\"opCzmUR5JT1TrA6oKKEDwMDj3fPVqj5M4ZCS4TKomLjMZ7H59Mn\",\"internal_operations\":[]}},{\"hash\":\"6e4c1c72ff9d1768ccff2d352f6ca2ff974871a49e18c6e98dbd74bceea0ad0a\",\"payload\":{\"tezos_operation_hash\":\"oofLvSokeHKsLTcPxkNKmhmCn4AsQsmG1LysCbK5eEAMAiZmeMh\",\"internal_operations\":[]}},{\"hash\":\"73e6e1482f20386971bcaf9a6c945f3b3b6ea610e33029d3070711f5f8ae831d\",\"payload\":{\"tezos_operation_hash\":\"onwQwLezW2CYSfCWEsJsMHPaxozHxsh3ZLs4sHXbK7mKu1ENqXq\",\"internal_operations\":[]}},{\"hash\":\"752878e56f754275860d024d31c4e9cdf330969614ee854d7b3ef072362ab184\",\"payload\":{\"tezos_operation_hash\":\"opErWMgozbynf37CZSdcFYcuWLHengwxBRb14UC2n1oK9cLS8Dj\",\"internal_operations\":[]}},{\"hash\":\"9f1f2ed6a6b0723527d19016bbb7302eee69a6ec8f5fa47998b5a75db3e1359a\",\"payload\":{\"tezos_operation_hash\":\"ooYiLktPkpMsRoeUFJaioPEBSVTYQiR3oLqYebqatGqGnZJyVZv\",\"internal_operations\":[]}},{\"hash\":\"a9fb55444e9d9326265b31de75deaf9a4ee4c44f9534f5aa58742dc0392a6cf4\",\"payload\":{\"tezos_operation_hash\":\"oosQPSSnFfg1xKaKLDhbiCHNH8mFZqCc6spo4nou89f6ou2mc9Y\",\"internal_operations\":[]}},{\"hash\":\"af8ed3bb647cf580f16ecaaa1e00c0a039d2f15539aa5d34966566c16ca4b02d\",\"payload\":{\"tezos_operation_hash\":\"ooKB2BVCNeAwbYowB5fLvkDqKh6HHysTUzGUeSoYrGvdEfSuPn6\",\"internal_operations\":[]}},{\"hash\":\"d2702fc270f78129bd9ddd1d2aef732a19ab95434e56d41ac82e7e9f537ed7a5\",\"payload\":{\"tezos_operation_hash\":\"onevA4eEKYhZqqS8u1gRb5frLZXq1UbhkmV5PwAKcPt1qFEPs62\",\"internal_operations\":[]}},{\"hash\":\"f178fcc8b3f5781d7ec504b58baada54135c7d9b8f7962f0bd3756f469271322\",\"payload\":{\"tezos_operation_hash\":\"ooXrL79STNA3YzTdjcRHE1wJ19uGKyAktV6RRTeYR2kUQ6Lc7sK\",\"internal_operations\":[]}},{\"hash\":\"f48db014e452bff2b4de1f99740149bd5002ea9aed9697d9b4d85baef19cfbe0\",\"payload\":{\"tezos_operation_hash\":\"op9DW1jgrEc2ExNXxG9zZ4Upud7kjxKNHWUuXjsQ5reFG5n8Agw\",\"internal_operations\":[]}},{\"hash\":\"fa8206239bfc7d983583007192c69d3d9de70424fcd08b686051996333e89e00\",\"payload\":{\"tezos_operation_hash\":\"onghaHJL2ayLqDnKxNSQJGuvF48U9LLEXi5TjrLdQQmQBnKRRh4\",\"internal_operations\":[]}}],[],{\"current\":{\"address\":\"tz1U7rU4rq8cQbcd4x5wmM4f9xXdL7UMs13B\"},\"validators\":[{\"address\":\"tz1U7rU4rq8cQbcd4x5wmM4f9xXdL7UMs13B\"},{\"address\":\"tz1PDcywQjkiMgcWnCgrWw3uZcMVTMcSS24A\"},{\"address\":\"tz1eaWiBkNTmGTDbxSWVZMcsdBXyDi18DkPR\"}],\"length\":3,\"hash\":\"5d8b9d301a1ac256f0dcd2ce916a1bf0f10b44af24fe316424a93051903b6f41\"},\"5d8b9d301a1ac256f0dcd2ce916a1bf0f10b44af24fe316424a93051903b6f41\",177,\"8b3fd358afb4024424e6b2a6dfd8edb7ba15774716db542801145bc265061f54\",\"53ba3600b40302d16bd6732faf2293aee7cf4ed214587df234120b83fdd77022\"]"
    },
    "additional_blocks": [
        {
            "hash": "9242ab0b4f749c02621e1568bf2e1dcc6e8b56d036545fcc6f5b6ea74e9061d8",
            "payload_hash": "28dc91451588d3caad3ba911cf217174992bce84e0cbdb67b3e741de921df596",
            "state_root_hash": "27c2b63640a7a3f29ab8c6fdbdb321185e58a2537f66ae461888c90692a3d360",
            "withdrawal_handles_hash": "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
            "validators_hash": "5d8b9d301a1ac256f0dcd2ce916a1bf0f10b44af24fe316424a93051903b6f41",
            "previous_hash": "2772b33e8a7469cca4f29d2fd33cb0f26bd5007fcf70a7b7d24f281836fabf2d",
            "author": "tz1U7rU4rq8cQbcd4x5wmM4f9xXdL7UMs13B",
            "block_height": 194,
            "operations": []
        },
        {
            "hash": "2772b33e8a7469cca4f29d2fd33cb0f26bd5007fcf70a7b7d24f281836fabf2d",
            "payload_hash": "5f09d1b883b497abe59a0b76da08f5f65599051d85ea576529dc978216e1f47a",
            "state_root_hash": "27c2b63640a7a3f29ab8c6fdbdb321185e58a2537f66ae461888c90692a3d360",
            "withdrawal_handles_hash": "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
            "validators_hash": "5d8b9d301a1ac256f0dcd2ce916a1bf0f10b44af24fe316424a93051903b6f41",
            "previous_hash": "0078297bf8bcd98b23b7212a471cd0e874ce2e3ccc32580061e0401b6f98377d",
            "author": "tz1U7rU4rq8cQbcd4x5wmM4f9xXdL7UMs13B",
            "block_height": 193,
            "operations": []
        },
        {
            "hash": "0078297bf8bcd98b23b7212a471cd0e874ce2e3ccc32580061e0401b6f98377d",
            "payload_hash": "c6cfda78aa2ad70577d33991bd04436593bc0f7bfaf2bfd501fedaed8e28a7ed",
            "state_root_hash": "27c2b63640a7a3f29ab8c6fdbdb321185e58a2537f66ae461888c90692a3d360",
            "withdrawal_handles_hash": "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
            "validators_hash": "5d8b9d301a1ac256f0dcd2ce916a1bf0f10b44af24fe316424a93051903b6f41",
            "previous_hash": "805e9920756e024d438c3b4910828f4492799d81b776b96718206334c291bc22",
            "author": "tz1U7rU4rq8cQbcd4x5wmM4f9xXdL7UMs13B",
            "block_height": 192,
            "operations": []
        },
        {
            "hash": "805e9920756e024d438c3b4910828f4492799d81b776b96718206334c291bc22",
            "payload_hash": "d4fb9e0cc21f9b8d1f8ae29adc70532b3ceba9acc6b8d4c778f4db964e6720ef",
            "state_root_hash": "27c2b63640a7a3f29ab8c6fdbdb321185e58a2537f66ae461888c90692a3d360",
            "withdrawal_handles_hash": "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
            "validators_hash": "5d8b9d301a1ac256f0dcd2ce916a1bf0f10b44af24fe316424a93051903b6f41",
            "previous_hash": "0fa29973d947b7c0315d829e918cfbc4905757172c69ca4f38eb9533b3a17dd0",
            "author": "tz1U7rU4rq8cQbcd4x5wmM4f9xXdL7UMs13B",
            "block_height": 191,
            "operations": []
        },
        {
            "hash": "0fa29973d947b7c0315d829e918cfbc4905757172c69ca4f38eb9533b3a17dd0",
            "payload_hash": "e701f08b0617a6cac8d2629150f316c8239663561ce675f3747becda82cb2938",
            "state_root_hash": "27c2b63640a7a3f29ab8c6fdbdb321185e58a2537f66ae461888c90692a3d360",
            "withdrawal_handles_hash": "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
            "validators_hash": "5d8b9d301a1ac256f0dcd2ce916a1bf0f10b44af24fe316424a93051903b6f41",
            "previous_hash": "84823cc97618fe46ee4566f638832bc0d036a0fce07bda8f29c1da1901850693",
            "author": "tz1U7rU4rq8cQbcd4x5wmM4f9xXdL7UMs13B",
            "block_height": 190,
            "operations": []
        },
        {
            "hash": "84823cc97618fe46ee4566f638832bc0d036a0fce07bda8f29c1da1901850693",
            "payload_hash": "86bc98d65fdc8bd29bdfbffe8d8974e99ffb88c92ab9053cd31fe285646e5da0",
            "state_root_hash": "49937e743305e6e6ca2496ad287c7b0d70320be8e07e7f60bb5b4578a848e361",
            "withdrawal_handles_hash": "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
            "validators_hash": "5d8b9d301a1ac256f0dcd2ce916a1bf0f10b44af24fe316424a93051903b6f41",
            "previous_hash": "706b08ea20a0499c4050cb174dbaee8c5ce74f8938eda71de2e685ec5a835d7f",
            "author": "tz1U7rU4rq8cQbcd4x5wmM4f9xXdL7UMs13B",
            "block_height": 189,
            "operations": []
        },
        {
            "hash": "706b08ea20a0499c4050cb174dbaee8c5ce74f8938eda71de2e685ec5a835d7f",
            "payload_hash": "9871c23c2ac5bcbf843a3b49af8a89c0092e15c4a5706fea5c26213eb98e57c0",
            "state_root_hash": "49937e743305e6e6ca2496ad287c7b0d70320be8e07e7f60bb5b4578a848e361",
            "withdrawal_handles_hash": "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
            "validators_hash": "5d8b9d301a1ac256f0dcd2ce916a1bf0f10b44af24fe316424a93051903b6f41",
            "previous_hash": "edc403eff9ded454284158e3869a8a4dc1c133fbeed76275c9c66b4eb55a4330",
            "author": "tz1U7rU4rq8cQbcd4x5wmM4f9xXdL7UMs13B",
            "block_height": 188,
            "operations": []
        },
        {
            "hash": "edc403eff9ded454284158e3869a8a4dc1c133fbeed76275c9c66b4eb55a4330",
            "payload_hash": "53c43a901e71e6c0aa7024f5bb4c67566bcd1804c6326565bfa94570b95cba80",
            "state_root_hash": "49937e743305e6e6ca2496ad287c7b0d70320be8e07e7f60bb5b4578a848e361",
            "withdrawal_handles_hash": "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
            "validators_hash": "5d8b9d301a1ac256f0dcd2ce916a1bf0f10b44af24fe316424a93051903b6f41",
            "previous_hash": "7f53c5ebdb75ed620dd8e3a8425c194cf7f3666ea9b3388318dadde0786a5cc6",
            "author": "tz1U7rU4rq8cQbcd4x5wmM4f9xXdL7UMs13B",
            "block_height": 187,
            "operations": [
                [
                    "Core_tezos",
                    {
                        "hash": "eeac2cf2576afea97f25020eb225f91dd3c609af2fc9577e35639e56f26ff5b9",
                        "payload": {
                            "tezos_operation_hash": "ooyLGrLYWSGgMn8DF9rULCpwnpnWEDFsMskMorb6bVfWiBrcZrR",
                            "internal_operations": []
                        }
                    }
                ]
            ]
        },
        {
            "hash": "7f53c5ebdb75ed620dd8e3a8425c194cf7f3666ea9b3388318dadde0786a5cc6",
            "payload_hash": "c708f3f0a6c954ed91130bae6da74289307ac85828954a493c2be0951021ec23",
            "state_root_hash": "49937e743305e6e6ca2496ad287c7b0d70320be8e07e7f60bb5b4578a848e361",
            "withdrawal_handles_hash": "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
            "validators_hash": "5d8b9d301a1ac256f0dcd2ce916a1bf0f10b44af24fe316424a93051903b6f41",
            "previous_hash": "2ee37ea0d7f7331ab08fa4ce434ddc11bd76dd682c960a39a5538735cf397664",
            "author": "tz1U7rU4rq8cQbcd4x5wmM4f9xXdL7UMs13B",
            "block_height": 186,
            "operations": []
        },
        {
            "hash": "2ee37ea0d7f7331ab08fa4ce434ddc11bd76dd682c960a39a5538735cf397664",
            "payload_hash": "1f6459ad8059347b0a1ecdea2ecea93974546f0a9d94aac4bb94af156fd82d8f",
            "state_root_hash": "49937e743305e6e6ca2496ad287c7b0d70320be8e07e7f60bb5b4578a848e361",
            "withdrawal_handles_hash": "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
            "validators_hash": "5d8b9d301a1ac256f0dcd2ce916a1bf0f10b44af24fe316424a93051903b6f41",
            "previous_hash": "0f6e2dcbb38c4f8cc87b10a3015713d432044a8b9af9a893e4eb236ac58674c1",
            "author": "tz1U7rU4rq8cQbcd4x5wmM4f9xXdL7UMs13B",
            "block_height": 185,
            "operations": []
        },
        {
            "hash": "0f6e2dcbb38c4f8cc87b10a3015713d432044a8b9af9a893e4eb236ac58674c1",
            "payload_hash": "fdf4949874414e747ed164e5f1431d2a158f31c6fb21f9ececd05952e139a6f9",
            "state_root_hash": "49937e743305e6e6ca2496ad287c7b0d70320be8e07e7f60bb5b4578a848e361",
            "withdrawal_handles_hash": "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
            "validators_hash": "5d8b9d301a1ac256f0dcd2ce916a1bf0f10b44af24fe316424a93051903b6f41",
            "previous_hash": "086bcbd1929fa82187e08aec8a56bd62dee6a9882baeb4f1e83ee8a9fc3a4ad0",
            "author": "tz1U7rU4rq8cQbcd4x5wmM4f9xXdL7UMs13B",
            "block_height": 184,
            "operations": []
        },
        {
            "hash": "086bcbd1929fa82187e08aec8a56bd62dee6a9882baeb4f1e83ee8a9fc3a4ad0",
            "payload_hash": "c15102e37ac5fda6f43914e390e6d62830916e7251e2520d567a02986800d084",
            "state_root_hash": "49937e743305e6e6ca2496ad287c7b0d70320be8e07e7f60bb5b4578a848e361",
            "withdrawal_handles_hash": "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
            "validators_hash": "5d8b9d301a1ac256f0dcd2ce916a1bf0f10b44af24fe316424a93051903b6f41",
            "previous_hash": "b65cbd1a04524469dcc3657ad97983c7682e2fa6e162654ebb0ea337f313579a",
            "author": "tz1U7rU4rq8cQbcd4x5wmM4f9xXdL7UMs13B",
            "block_height": 183,
            "operations": []
        },
        {
            "hash": "b65cbd1a04524469dcc3657ad97983c7682e2fa6e162654ebb0ea337f313579a",
            "payload_hash": "51866cbed231edf739d1a3acbd2cd3fda6be64a4f7ffc18c510371d409d025b9",
            "state_root_hash": "49937e743305e6e6ca2496ad287c7b0d70320be8e07e7f60bb5b4578a848e361",
            "withdrawal_handles_hash": "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
            "validators_hash": "5d8b9d301a1ac256f0dcd2ce916a1bf0f10b44af24fe316424a93051903b6f41",
            "previous_hash": "a503dfec940c58a75faa90605b52c998782b03c917a01448c54d3e274856d707",
            "author": "tz1U7rU4rq8cQbcd4x5wmM4f9xXdL7UMs13B",
            "block_height": 182,
            "operations": []
        },
        {
            "hash": "a503dfec940c58a75faa90605b52c998782b03c917a01448c54d3e274856d707",
            "payload_hash": "ef1658cae547457a66cdec4a91d65e36f0f873e3d49b9322a8dd3a57fc7018a1",
            "state_root_hash": "49937e743305e6e6ca2496ad287c7b0d70320be8e07e7f60bb5b4578a848e361",
            "withdrawal_handles_hash": "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
            "validators_hash": "5d8b9d301a1ac256f0dcd2ce916a1bf0f10b44af24fe316424a93051903b6f41",
            "previous_hash": "a846606df603237ec3babb92f25210900c13a84170ba88b164f4e5e5fe047185",
            "author": "tz1U7rU4rq8cQbcd4x5wmM4f9xXdL7UMs13B",
            "block_height": 181,
            "operations": []
        },
        {
            "hash": "a846606df603237ec3babb92f25210900c13a84170ba88b164f4e5e5fe047185",
            "payload_hash": "a369d529554932c773017b74c43089dda4b7c35bd49c9351cf8d17194cfd9695",
            "state_root_hash": "49937e743305e6e6ca2496ad287c7b0d70320be8e07e7f60bb5b4578a848e361",
            "withdrawal_handles_hash": "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
            "validators_hash": "5d8b9d301a1ac256f0dcd2ce916a1bf0f10b44af24fe316424a93051903b6f41",
            "previous_hash": "cf2a7519526e18eac41de225805a0b2672ab38d2467ee9615c1edb4eff0c0f42",
            "author": "tz1U7rU4rq8cQbcd4x5wmM4f9xXdL7UMs13B",
            "block_height": 180,
            "operations": []
        },
        {
            "hash": "cf2a7519526e18eac41de225805a0b2672ab38d2467ee9615c1edb4eff0c0f42",
            "payload_hash": "7fe6971a63b7f2070e1bc9bcb3813fe3d3a37bfbd9cfd70b33f56d63ca46c09f",
            "state_root_hash": "49937e743305e6e6ca2496ad287c7b0d70320be8e07e7f60bb5b4578a848e361",
            "withdrawal_handles_hash": "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
            "validators_hash": "5d8b9d301a1ac256f0dcd2ce916a1bf0f10b44af24fe316424a93051903b6f41",
            "previous_hash": "fb9bd722fc376817608f211d03ed2860bf52a6c7fa395ac72cfca891b212e31f",
            "author": "tz1U7rU4rq8cQbcd4x5wmM4f9xXdL7UMs13B",
            "block_height": 179,
            "operations": []
        },
        {
            "hash": "fb9bd722fc376817608f211d03ed2860bf52a6c7fa395ac72cfca891b212e31f",
            "payload_hash": "831b8dc3e50fd123292d9810ec35ce472202b23ab0bb3fca09aec5b5b0faf028",
            "state_root_hash": "49937e743305e6e6ca2496ad287c7b0d70320be8e07e7f60bb5b4578a848e361",
            "withdrawal_handles_hash": "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
            "validators_hash": "5d8b9d301a1ac256f0dcd2ce916a1bf0f10b44af24fe316424a93051903b6f41",
            "previous_hash": "8b3fd358afb4024424e6b2a6dfd8edb7ba15774716db542801145bc265061f54",
            "author": "tz1U7rU4rq8cQbcd4x5wmM4f9xXdL7UMs13B",
            "block_height": 178,
            "operations": []
        }
    ],
    "last_block": {
        "hash": "5743c7bfd79b567c6e5316e53054e127a50872d5697226b526074e0942dea127",
        "payload_hash": "0e0c82988754875cbed6bb6dae7613d1e85da935063f2aa1542144263255dd38",
        "state_root_hash": "27c2b63640a7a3f29ab8c6fdbdb321185e58a2537f66ae461888c90692a3d360",
        "withdrawal_handles_hash": "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
        "validators_hash": "5d8b9d301a1ac256f0dcd2ce916a1bf0f10b44af24fe316424a93051903b6f41",
        "previous_hash": "9242ab0b4f749c02621e1568bf2e1dcc6e8b56d036545fcc6f5b6ea74e9061d8",
        "author": "tz1U7rU4rq8cQbcd4x5wmM4f9xXdL7UMs13B",
        "block_height": 195,
        "operations": []
    },
    "last_block_signatures": [
        {
            "signature": "edsigtd15i1RaPqybbcAgrxNgTAHFEzdH2SC6G65iioVaT4Sy9mbKydZdWugcgBeeR978rXbTVpXiaKCn6Kb3fhZsCHmPBSeWWj",
            "public_key": "edpkvLqzPukFvnEz4ZobJa3ZLwa94YcDemk4KrDfVemkT7QNbWZZzz"
        },
        {
            "signature": "edsigtkm6g9DWpXVVeshYE7hKCSY9a3PYQ7Azyw1vBbzhCx98TvS8iNgd9ySqUajG2YcyLRBpJwBWYQWRWJUPqiky5T42wyZJby",
            "public_key": "edpkudpeGyUx9zoHdVuMBa1XqPLZ331UQXjCTAqYezNmgK5msDKdk4"
        }
    ]
}
```

## /vm-state

Retrieve the VM state

```shell script
$ curl http://localhost:4440/vm-state -d "null"
```

With the example of cookie-game state, response will be something like

```JSON
{
    "state": [
        [
            "tz1VULT8pu1NoWs7YPFWuvXSg3JSdGq55TXc",
            {
                "cookie_baker": {
                    "number_of_cookie": 0,
                    "number_of_cursor": 0,
                    "number_of_grandma": 0,
                    "number_of_farm": 0,
                    "number_of_free_cursor": 0,
                    "number_of_free_grandma": 0,
                    "number_of_free_farm": 0,
                    "cursor_cost": 0,
                    "grandma_cost": 0,
                    "farm_cost": 0,
                    "cursor_cps": 0,
                    "grandma_cps": 0,
                    "farm_cps": 0
                }
            }
        ]
    ]
}
```

## /user-operation-gossip

Submit an operation to the nodes

```shell script
$ curl http://localhost:4440/user-operation-gossip -H "Content-type: application/json" -d \
'{
  "user_operation": {
    "hash": "c62e6c83681dc160f2a2c74bc7e53cc2f3a0e0bb15521d2f8d9d26ecff026296",
    "key": "edpkv5a9ZSXJDErjRC2N8hR4GbimYsQ45q646tsuBvCTDiVnpKU3Q9",
    "signature": "edsigtwykT6F922f7FCQyY8jy77Zwf51m6TXRVNuziRsrHLiEAyaRobxrceDcsDtXWhX9eUrLBhRFFoqLsos4hBkUY5Eqqwmws3",
    "nonce": 1387604187,
    "block_height": 448,
    "data": {
      "hash": "d6d9a446af772944a4f477b29044d5c7acd2181e7eb4493342217fe788d23416",
      "source": "tz1VULT8pu1NoWs7YPFWuvXSg3JSdGq55TXc",
      "initial_operation": [
        "Vm_transaction",
        {
          "payload": "cookie"
        }
      ]
    }
  }
}'
```

The answer will only be the print of the provided hash, like following:

```shell script
operation.hash c62e6c83681dc160f2a2c74bc7e53cc2f3a0e0bb15521d2f8d9d26ecff026296
```

## /append-signature

Append a signature to an already existing block

```shell script
$ curl http://localhost:4440/append-signature -H "Content-type: application/json" -d \
'{
  "hash": "238283eaafb77785b0e454f3fa72367eaa4a2ec3c4f44efa595130c853fca28b",
  "signature": {
    "signature": "edsigtvq5CHkdeLJLAEBqzGV4dccPonDhVmdEBUaYV76EjqGjyBb4vgPL5YdMChL1dMY2rtoRQiJGYiuLEMCe4yPTSr3kQpy3td",
    "public_key": "edpkvSzBrQ7g5wuncyK7gvEjNn6VYewbpJFp6BzZ5TmByMN7XfyV3g"
  }
}'
```