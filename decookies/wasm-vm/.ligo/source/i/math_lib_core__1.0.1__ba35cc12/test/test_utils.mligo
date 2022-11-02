#import "../utils.mligo" "Utils"

let test =

    let _test_is_implicit = 
        //0x050a00000016 00 00430d6ec166d9c623104776aaad3bb50615c6791f

        // let is_implicit(elt: address) : bool = 
        //     let pack_elt : bytes = Bytes.pack elt in
        //     let () = Test.log((elt, pack_elt)) in
        //     let size : nat = Bytes.length pack_elt in
        //     let is_imp : bytes = Bytes.sub 6n 1n pack_elt in
        //     let addr_bin : bytes = Bytes.sub 7n (abs(size - 7n)) pack_elt in
        //     let value : nat = Utils.Bytes.bytes_to_nat(addr_bin) in
        //     let () = Test.log(value) in
        //     ( is_imp = 0x00 )
        // in

        // TEST
        let test_addr_implicit : address list = [
            ("tz1hxxVY5jc6bsfCTzv27FLhVBZfKFaB164R" : address);
            ("tz1RyejUffjfnHzWoRp1vYyZwGnfPuHsD5F5" : address);
            ("tz1hHfjC5wX3gdQBZ6LyBWTrBczWQjktEu5E" : address);
            ("tz1fujCnFL1wQa7rtWKcvBvRk1uWoiDTQ5CX" : address);
            ("tz1Ytm4W7j6W4jj4EhnB854JfwEkTMUpRQQ2" : address);
            ("tz1MtU1ZH9cLSRjyrZXstEGxJcbRfvX4E9Ga" : address);
            ("tz1UbRzhYjQKTtWYvGUWcRtVT4fN3NESDVYT" : address);
            ("tz1Rka4HepbaCwH8K1fCn2Hq2GCjVLuRv4AL" : address);
        ] in
        let test_addr_non_implicit : address list = [
            ("KT1Cp18EbxDk2WcbC1YVUyGuwuvtzyujwu4U" : address);
            ("KT1Qg4FmXDmViQgyYLT5QkgZQSmPKvKjZbzn" : address);
            ("KT1CW6CgagaNoP95aDcp1B7PcwaAess66SYS" : address);
            ("KT1TwzD6zV3WeJ39ukuqxcfK2fJCnhvrdN1X" : address);
            ("KT1MsktCnwfS1nGZmf8QbaTpZ8euVijWdmkC" : address);
            ("KT1Xobej4mc6XgEjDoJoHtTKgbD1ELMvcQuL" : address);
            ("KT1LTmkz5J7vESxZjHqazzdhGohS5uTXEa2G" : address);
            ("KT1F7zytGMtjWvrZubpFTq7Px4ubaQybZMoH" : address);
            ("KT19T6ZqpUWvMYX9LH9FmCkahyEyGY27PEPT" : address);
            ("KT1RMfFJphfVwdbaJx7DhFrZ9du5pREVSEyN" : address);
            ("KT1P6fPe5KDGaHJwCRB5o5XzH8iSbRVsXReg" : address);
            ("KT1XkmNDDEcsjxgJJ3LVBme9bx86tNagYuKi" : address);
            ("KT1Uo3LThxuwmBjQBTJV486JAUMKrZ45TBkc" : address);
            ("KT1N2kdJLjyamWxjibFcyQKe7dNoaxf263tz" : address);
            ("KT1MWSBaGkVMEx4wHfxqPym427rLvTVM1siB" : address);
            ("KT1JZHtsz7r9uYLLRKGgLvP8HR5eM75MfrHU" : address);
            ("KT1GtoXddGuPfBSwdT1GCG9HQE7AMMKzWf3H" : address);
            ("KT1W6FrLW9d8Y6NVQzx487KCXrTGK1RWtJNh" : address);
            ("KT1WqRhUKCqnq7nwwtySk3SPP3ebyrKtfbx9" : address);
            ("KT1MCJsFdhpgKJaJ8t8o99tDuyB2DdjdpRUe" : address);
        ] in
        let all_tests : (address * bool) list = ([] : (address * bool) list) in
        let add_negative_tests_from_inputs (inputs: address list) (tests: (address * bool) list) : (address * bool) list = 
            let fill_expectation_false ((acc, elt) : (address * bool) list * address) : (address * bool) list = (elt, false) :: acc in
            List.fold fill_expectation_false inputs tests 
        in
        let add_positive_tests_from_inputs (inputs: address list) (tests: (address * bool) list) : (address * bool) list = 
            let fill_expectation_true ((acc, elt) : (address * bool) list * address) : (address * bool) list = (elt, true) :: acc in
            List.fold fill_expectation_true inputs tests 
        in        
        let run_tests (all_tests : (address * bool) list) (f: (address) -> bool) : unit =
            let run_test (elt : (address * bool)) : unit = 
                let calculated : bool = f(elt.0) in
                let expected : bool = elt.1 in
                assert (calculated = expected)
            in 
            List.iter run_test all_tests
        in

        // for KT1... is_implicit() should answer false
        let all_tests = add_negative_tests_from_inputs test_addr_non_implicit all_tests in
        // for tz1... is_implicit() should answer false
        let all_tests = add_positive_tests_from_inputs test_addr_implicit all_tests in
        // Run tests
        let () = run_tests all_tests Utils.Address.is_implicit in

        Test.log("Test finished")
    in


    let _test_bytes_is_adddress = (* chest key/payload and time matches -> OK *)
    
        let payload : bytes = 0x050a000000160000430d6ec166d9c623104776aaad3bb50615c6791f in
        let value : bool option = Utils.Bytes.Packed.is_internal_address_implicit(payload) in
        let () = match value with
        | None -> failwith("a boolean true was expected")
        | Some b -> assert(b = true) 
        in

        let payload : bytes = 0x050a0000001601f4186c23ee65145a1038193686882c29a1f2009e00 in
        let value : bool option = Utils.Bytes.Packed.is_internal_address_implicit(payload) in
        let () = match value with
        | None -> failwith("a boolean false was expected")
        | Some b -> assert(b = false) 
        in
        let payload : bytes = 0x07020000001601f4186c23ee65145a1038193686882c29a1f2009e00 in
        let value : bool option = Utils.Bytes.Packed.is_internal_address_implicit(payload) in
        let () = match value with
        | None -> assert(true)
        | Some _ -> failwith("a None was expected")
        in


        Test.log("Test finished")
    in
    let _test_convert_bytes_to_nat = (* chest key/payload and time matches -> OK *)
    
        let payload : bytes = 0x00 in
        let value : nat = Utils.Bytes.Conversion.bytes_to_nat(payload) in
        let () = assert(value = 0n) in

        let payload : bytes = 0x0a in
        let value : nat = Utils.Bytes.Conversion.bytes_to_nat(payload) in
        let () = assert(value = 10n) in

        let payload : bytes = 0x0A in
        let value : nat = Utils.Bytes.Conversion.bytes_to_nat(payload) in
        let () = assert(value = 10n) in

        let payload : bytes = 0x2c in
        let value : nat = Utils.Bytes.Conversion.bytes_to_nat(payload) in
        let () = assert(value = 44n) in

        let payload : bytes = 0x2C in
        let value : nat = Utils.Bytes.Conversion.bytes_to_nat(payload) in
        let () = assert(value = 44n) in

        let payload : bytes = 0xFF in
        let value : nat = Utils.Bytes.Conversion.bytes_to_nat(payload) in
        let () = assert(value = 255n) in

        let payload : bytes = 0x1234 in
        let value : nat = Utils.Bytes.Conversion.bytes_to_nat(payload) in
        let () = assert(value = 4660n) in

        let payload : bytes = 0x001234 in
        let value : nat = Utils.Bytes.Conversion.bytes_to_nat(payload) in
        let () = assert(value = 4660n) in

        let payload : bytes = 0x123400 in
        let value : nat = Utils.Bytes.Conversion.bytes_to_nat(payload) in
        let () = assert(value = 4660n * 256n) in


        Test.log("Test 'rational' finished")
    in
    ()

