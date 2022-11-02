#import "./math.mligo" "Math"

module Address = struct 

    (** Check if the given address is an implicit account (i.e tz1...) *)
    let is_implicit(elt: address) : bool = 
        let pack_elt : bytes = Bytes.pack elt in
        let is_imp : bytes = Bytes.sub 6n 1n pack_elt in
        //let size : nat = Bytes.length pack_elt in
        //let addr_bin : bytes = Bytes.sub 7n (abs(size - 7n)) pack_elt in
        //let value : nat = bytes_to_nat(addr_bin) in
        //let () = Test.log(value) in
        ( is_imp = 0x00 )

end

module Bytes = struct 

    module Packed = struct 

        let is_internal_address(pack_elt: bytes) : bool = 
            ((Bytes.sub 0n 1n pack_elt) = 0x05) && 
            ((Bytes.sub 1n 1n pack_elt) = 0x0a) && 
            ((Bytes.sub 2n 4n pack_elt) = 0x00000016) && 
            ((Bytes.length (Bytes.sub 6n 22n pack_elt)) = 22n)

        let is_internal_address_implicit(candidat: bytes) : bool option = 
            if (is_internal_address(candidat)) 
            then Some ( (Bytes.sub 6n 1n candidat) = 0x00 )
            else None

    end

    module Helpers = struct 

        // helpers
        let bytes_to_list (ch: bytes) : bytes list = 
            let rec split_bytes_rec(acc, payload : bytes list * bytes) : bytes list =
                let size : nat = (Bytes.length payload) in
                if size = 1n then
                    payload :: acc
                else
                    let one_last_bytes = Bytes.sub (abs(size - 1n)) 1n payload in
                    let left_bytes = Bytes.sub 0n (abs(size - 1n)) payload in 
                    split_bytes_rec(one_last_bytes :: acc, left_bytes)
            in
            split_bytes_rec(([]: bytes list), ch) 
        
        let bytes_from_list (lst: bytes list) : bytes = 
            let concatenate ((acc, elt): bytes * bytes) : bytes = Bytes.concat acc elt in
            List.fold concatenate lst 0x 

        let read_nb_bytes (nb : nat) (payload_param: bytes) : bytes * bytes =
            let size : nat = (Bytes.length payload_param) in 
            let left_bytes : bytes = Bytes.sub 0n nb payload_param in
            let right_bytes = Bytes.sub nb (abs(size - nb)) payload_param in 
            (left_bytes, right_bytes)

    end

    module Conversion = struct 

        [@private]
        let byte_to_nat(hexa : bytes) : nat =
            let _check_size : unit = assert_with_error (Bytes.length hexa = 1n) "Can only convert 1 byte" in    
            if hexa = 0x00 then 0n
            else if hexa = 0x01 then 1n
            else if hexa = 0x02 then 2n
            else if hexa = 0x03 then 3n
            else if hexa = 0x04 then 4n
            else if hexa = 0x05 then 5n
            else if hexa = 0x06 then 6n
            else if hexa = 0x07 then 7n
            else if hexa = 0x08 then 8n
            else if hexa = 0x09 then 9n
            else if hexa = 0x0A then 10n
            else if hexa = 0x0B then 11n
            else if hexa = 0x0C then 12n
            else if hexa = 0x0D then 13n
            else if hexa = 0x0E then 14n
            else if hexa = 0x0F then 15n
            else if hexa = 0x10 then 16n
            else if hexa = 0x11 then 17n
            else if hexa = 0x12 then 18n
            else if hexa = 0x13 then 19n
            else if hexa = 0x14 then 20n
            else if hexa = 0x15 then 21n
            else if hexa = 0x16 then 22n
            else if hexa = 0x17 then 23n
            else if hexa = 0x18 then 24n
            else if hexa = 0x19 then 25n
            else if hexa = 0x1A then 26n
            else if hexa = 0x1B then 27n
            else if hexa = 0x1C then 28n
            else if hexa = 0x1D then 29n
            else if hexa = 0x1E then 30n
            else if hexa = 0x1F then 31n
            else if hexa = 0x20 then 32n
            else if hexa = 0x21 then 33n
            else if hexa = 0x22 then 34n
            else if hexa = 0x23 then 35n
            else if hexa = 0x24 then 36n
            else if hexa = 0x25 then 37n
            else if hexa = 0x26 then 38n
            else if hexa = 0x27 then 39n
            else if hexa = 0x28 then 40n
            else if hexa = 0x29 then 41n
            else if hexa = 0x2A then 42n
            else if hexa = 0x2B then 43n
            else if hexa = 0x2C then 44n
            else if hexa = 0x2D then 45n
            else if hexa = 0x2E then 46n
            else if hexa = 0x2F then 47n
            else if hexa = 0x30 then 48n
            else if hexa = 0x31 then 49n
            else if hexa = 0x32 then 50n
            else if hexa = 0x33 then 51n
            else if hexa = 0x34 then 52n
            else if hexa = 0x35 then 53n
            else if hexa = 0x36 then 54n
            else if hexa = 0x37 then 55n
            else if hexa = 0x38 then 56n
            else if hexa = 0x39 then 57n
            else if hexa = 0x3A then 58n
            else if hexa = 0x3B then 59n
            else if hexa = 0x3C then 60n
            else if hexa = 0x3D then 61n
            else if hexa = 0x3E then 62n
            else if hexa = 0x3F then 63n
            else if hexa = 0x40 then 64n
            else if hexa = 0x41 then 65n
            else if hexa = 0x42 then 66n
            else if hexa = 0x43 then 67n
            else if hexa = 0x44 then 68n
            else if hexa = 0x45 then 69n
            else if hexa = 0x46 then 70n
            else if hexa = 0x47 then 71n
            else if hexa = 0x48 then 72n
            else if hexa = 0x49 then 73n
            else if hexa = 0x4A then 74n
            else if hexa = 0x4B then 75n
            else if hexa = 0x4C then 76n
            else if hexa = 0x4D then 77n
            else if hexa = 0x4E then 78n
            else if hexa = 0x4F then 79n
            else if hexa = 0x50 then 80n
            else if hexa = 0x51 then 81n
            else if hexa = 0x52 then 82n
            else if hexa = 0x53 then 83n
            else if hexa = 0x54 then 84n
            else if hexa = 0x55 then 85n
            else if hexa = 0x56 then 86n
            else if hexa = 0x57 then 87n
            else if hexa = 0x58 then 88n
            else if hexa = 0x59 then 89n
            else if hexa = 0x5A then 90n
            else if hexa = 0x5B then 91n
            else if hexa = 0x5C then 92n
            else if hexa = 0x5D then 93n
            else if hexa = 0x5E then 94n
            else if hexa = 0x5F then 95n
            else if hexa = 0x60 then 96n
            else if hexa = 0x61 then 97n
            else if hexa = 0x62 then 98n
            else if hexa = 0x63 then 99n
            else if hexa = 0x64 then 100n
            else if hexa = 0x65 then 101n
            else if hexa = 0x66 then 102n
            else if hexa = 0x67 then 103n
            else if hexa = 0x68 then 104n
            else if hexa = 0x69 then 105n
            else if hexa = 0x6A then 106n
            else if hexa = 0x6B then 107n
            else if hexa = 0x6C then 108n
            else if hexa = 0x6D then 109n
            else if hexa = 0x6E then 110n
            else if hexa = 0x6F then 111n
            else if hexa = 0x70 then 112n
            else if hexa = 0x71 then 113n
            else if hexa = 0x72 then 114n
            else if hexa = 0x73 then 115n
            else if hexa = 0x74 then 116n
            else if hexa = 0x75 then 117n
            else if hexa = 0x76 then 118n
            else if hexa = 0x77 then 119n
            else if hexa = 0x78 then 120n
            else if hexa = 0x79 then 121n
            else if hexa = 0x7A then 122n
            else if hexa = 0x7B then 123n
            else if hexa = 0x7C then 124n
            else if hexa = 0x7D then 125n
            else if hexa = 0x7E then 126n
            else if hexa = 0x7F then 127n
            else if hexa = 0x80 then 128n
            else if hexa = 0x81 then 129n
            else if hexa = 0x82 then 130n
            else if hexa = 0x83 then 131n
            else if hexa = 0x84 then 132n
            else if hexa = 0x85 then 133n
            else if hexa = 0x86 then 134n
            else if hexa = 0x87 then 135n
            else if hexa = 0x88 then 136n
            else if hexa = 0x89 then 137n
            else if hexa = 0x8A then 138n
            else if hexa = 0x8B then 139n
            else if hexa = 0x8C then 140n
            else if hexa = 0x8D then 141n
            else if hexa = 0x8E then 142n
            else if hexa = 0x8F then 143n
            else if hexa = 0x90 then 144n
            else if hexa = 0x91 then 145n
            else if hexa = 0x92 then 146n
            else if hexa = 0x93 then 147n
            else if hexa = 0x94 then 148n
            else if hexa = 0x95 then 149n
            else if hexa = 0x96 then 150n
            else if hexa = 0x97 then 151n
            else if hexa = 0x98 then 152n
            else if hexa = 0x99 then 153n
            else if hexa = 0x9A then 154n
            else if hexa = 0x9B then 155n
            else if hexa = 0x9C then 156n
            else if hexa = 0x9D then 157n
            else if hexa = 0x9E then 158n
            else if hexa = 0x9F then 159n
            else if hexa = 0xA0 then 160n
            else if hexa = 0xA1 then 161n
            else if hexa = 0xA2 then 162n
            else if hexa = 0xA3 then 163n
            else if hexa = 0xA4 then 164n
            else if hexa = 0xA5 then 165n
            else if hexa = 0xA6 then 166n
            else if hexa = 0xA7 then 167n
            else if hexa = 0xA8 then 168n
            else if hexa = 0xA9 then 169n
            else if hexa = 0xAA then 170n
            else if hexa = 0xAB then 171n
            else if hexa = 0xAC then 172n
            else if hexa = 0xAD then 173n
            else if hexa = 0xAE then 174n
            else if hexa = 0xAF then 175n
            else if hexa = 0xB0 then 176n
            else if hexa = 0xB1 then 177n
            else if hexa = 0xB2 then 178n
            else if hexa = 0xB3 then 179n
            else if hexa = 0xB4 then 180n
            else if hexa = 0xB5 then 181n
            else if hexa = 0xB6 then 182n
            else if hexa = 0xB7 then 183n
            else if hexa = 0xB8 then 184n
            else if hexa = 0xB9 then 185n
            else if hexa = 0xBA then 186n
            else if hexa = 0xBB then 187n
            else if hexa = 0xBC then 188n
            else if hexa = 0xBD then 189n
            else if hexa = 0xBE then 190n
            else if hexa = 0xBF then 191n
            else if hexa = 0xC0 then 192n
            else if hexa = 0xC1 then 193n
            else if hexa = 0xC2 then 194n
            else if hexa = 0xC3 then 195n
            else if hexa = 0xC4 then 196n
            else if hexa = 0xC5 then 197n
            else if hexa = 0xC6 then 198n
            else if hexa = 0xC7 then 199n
            else if hexa = 0xC8 then 200n
            else if hexa = 0xC9 then 201n
            else if hexa = 0xCA then 202n
            else if hexa = 0xCB then 203n
            else if hexa = 0xCC then 204n
            else if hexa = 0xCD then 205n
            else if hexa = 0xCE then 206n
            else if hexa = 0xCF then 207n
            else if hexa = 0xD0 then 208n
            else if hexa = 0xD1 then 209n
            else if hexa = 0xD2 then 210n
            else if hexa = 0xD3 then 211n
            else if hexa = 0xD4 then 212n
            else if hexa = 0xD5 then 213n
            else if hexa = 0xD6 then 214n
            else if hexa = 0xD7 then 215n
            else if hexa = 0xD8 then 216n
            else if hexa = 0xD9 then 217n
            else if hexa = 0xDA then 218n
            else if hexa = 0xDB then 219n
            else if hexa = 0xDC then 220n
            else if hexa = 0xDD then 221n
            else if hexa = 0xDE then 222n
            else if hexa = 0xDF then 223n
            else if hexa = 0xE0 then 224n
            else if hexa = 0xE1 then 225n
            else if hexa = 0xE2 then 226n
            else if hexa = 0xE3 then 227n
            else if hexa = 0xE4 then 228n
            else if hexa = 0xE5 then 229n
            else if hexa = 0xE6 then 230n
            else if hexa = 0xE7 then 231n
            else if hexa = 0xE8 then 232n
            else if hexa = 0xE9 then 233n
            else if hexa = 0xEA then 234n
            else if hexa = 0xEB then 235n
            else if hexa = 0xEC then 236n
            else if hexa = 0xED then 237n
            else if hexa = 0xEE then 238n
            else if hexa = 0xEF then 239n
            else if hexa = 0xF0 then 240n
            else if hexa = 0xF1 then 241n
            else if hexa = 0xF2 then 242n
            else if hexa = 0xF3 then 243n
            else if hexa = 0xF4 then 244n
            else if hexa = 0xF5 then 245n
            else if hexa = 0xF6 then 246n
            else if hexa = 0xF7 then 247n
            else if hexa = 0xF8 then 248n
            else if hexa = 0xF9 then 249n
            else if hexa = 0xFA then 250n
            else if hexa = 0xFB then 251n
            else if hexa = 0xFC then 252n
            else if hexa = 0xFD then 253n
            else if hexa = 0xFE then 254n
            else if hexa = 0xFF then 255n
            else
                (failwith("Wrong hexa") : nat)

        let bytes_to_nat(payload : bytes) : nat =
            let rec convert_to_nat(acc, indice, payload : nat * nat * bytes) : nat =
                if indice = 1n then
                    acc + byte_to_nat(payload)
                else
                    let size : nat = (Bytes.length payload) in
                    let one_left_bytes = Bytes.sub 0n 1n payload in
                    let right_bytes = Bytes.sub 1n (abs(size - 1n)) payload in 
                    let one_left_nat = byte_to_nat(one_left_bytes) * Math.power(256n, abs(indice - 1n)) in 
                    convert_to_nat(acc + one_left_nat, abs(indice - 1n), right_bytes)
            in
            convert_to_nat(0n, Bytes.length payload, payload)
    end
end