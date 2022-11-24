open Deku_protocol
open Deku_stdlib
open Deku_concepts
open Deku_gossip
open Deku_crypto

let post_directly_to_node ~env ~operation =
  let host = "127.0.0.1" in
  let port = 4440 in
  let net = Eio.Stdenv.net env in
  let content = Message.Content.operation operation in
  let (Message { network; _ }) = Message.encode ~content in
  let (Network_message { raw_header; raw_content }) = network in
  let open Deku_network in
  let message = Network_message.message ~raw_header ~raw_content in
  Network_protocol.Client.connect ~net ~host ~port @@ fun connection ->
  Network_protocol.Connection.write connection message

let post_to_api ~sw ~env ~operation =
  let node = "http://localhost:8080/api/v1/operations" |> Uri.of_string in
  let json =
    Data_encoding.Json.construct Operation.Signed.encoding operation
    |> Data_encoding.Json.to_string
  in
  let body = Piaf.Body.of_string json in
  let post_result = Piaf.Client.Oneshot.post ~body ~sw env node in
  match post_result with
  | Ok _ -> print_endline "operation submitted"
  | Error _ -> print_endline "FAIL to submit operation"

let make_identity secret =
  secret |> Secret.of_b58 |> Option.get |> Identity.make

type level_response = { level : Level.t }

let make_level ~sw ~env () =
  let response =
    Piaf.Client.Oneshot.get ~sw env
      (Uri.of_string "http://localhost:8080/api/v1/chain/level")
  in
  let body =
    match response with
    | Error _ -> failwith "cannot connect to the API"
    | Ok res -> res.body
  in
  let string = Piaf.Body.to_string body in
  let body =
    match string with
    | Error _ -> failwith "cannot parse body"
    | Ok body -> body
  in
  let json = Data_encoding.Json.from_string body in
  match json with
  | Ok json ->
      let level = Data_encoding.Json.destruct Level.encoding json in
      level
  | _ -> failwith "cannot decode level"

let make_nonce () =
  let rng = Stdlib.Random.State.make_self_init () in
  Stdlib.Random.State.bits64 rng
  |> Int64.abs |> Z.of_int64 |> N.of_z |> Option.get |> Nonce.of_n

let main ~env ~sw:_ =
  let identity =
    make_identity "edsk4UWkJqpZrAm26qvJE8uY9ZFGFqQiFuBcDyEPASXeHxuD68WvvF"
  in
  let level = Level.zero in
  let nonce = make_nonce () in
  let _content2 =
    {|
    { "operation":
    { "initial_storage": [ "Map", [] ],
      "module":
        "0061736d0100000001c3808080000d60017e017e60017e0060017e017f60027e7e017e60017f017e6000017e60027e7f0060037e7e7e017e60027e7f017e60027f7e017e60037e7e7e0060017f00600000028a888080005103656e760769735f6c656674000203656e76086475705f686f7374000103656e760470616972000303656e7606706169725f6e000403656e7606756e70616972000103656e76057a5f616464000303656e76057a5f737562000303656e76057a5f6d756c000303656e76036e6567000003656e76036c736c000303656e7606636f6e636174000303656e76036c7372000303656e7607636f6d70617265000303656e7603636172000003656e7603636472000003656e7604736f6d65000003656e76036e696c000503656e760474727565000503656e7608756e706169725f6e000603656e760566616c7365000503656e76046e6f6e65000503656e7604756e6974000503656e76047a65726f000503656e7609656d7074795f6d6170000503656e7609656d7074795f736574000503656e760d656d7074795f6269675f6d6170000503656e760673656e646572000503656e7606736f75726365000503656e76076d61705f676574000303656e76036d656d000303656e7606757064617465000703656e760469746572000603656e76036d6170000803656e760769665f6c656674000203656e760769665f6e6f6e65000203656e760769665f636f6e73000203656e760569736e6174000003656e76036e6f74000003656e76026f72000303656e7603616e64000303656e7603786f72000303656e760a64657265665f626f6f6c000203656e76036e6571000003656e76086661696c77697468000103656e76056765745f6e000903656e760465786563000303656e76056170706c79000303656e7605636f6e7374000403656e7603616273000003656e76026571000003656e76026774000003656e76026c74000003656e7607636c6f73757265000403656e76046c656674000003656e76057269676874000003656e7604636f6e73000303656e760f7472616e736665725f746f6b656e73000703656e760761646472657373000003656e7608636f6e7472616374000003656e760473656c66000503656e760c73656c665f61646472657373000503656e760e6765745f616e645f757064617465000a03656e760b726561645f7469636b6574000103656e76067469636b6574000303656e760c6a6f696e5f7469636b657473000003656e760c73706c69745f7469636b6574000303656e7606616d6f756e74000503656e760762616c616e6365000503656e760465646976000303656e76026765000003656e76026c65000003656e760473697a65000003656e7603696e74000003656e7610696d706c696369745f6163636f756e74000003656e7607626c616b653262000003656e76047061636b000003656e7606756e7061636b000003656e76066b656363616b000003656e7606736861323536000003656e760473686133000003656e760673686135313200000391808080001008060b0b0b0c0b0b05010b00000000000485808080000170010404058380808000010004069980808000047f0041000b7f0141a01f0b7f0141e8070b7f00418080020b07c580808000060470757368005a03706f700059046d61696e006008636c6f737572657301000d63616c6c5f63616c6c6261636b00511263616c6c5f63616c6c6261636b5f756e69740052098a80808000010041000b045f5e5d5c0afad780800010898080800000200020011100000b898080800000200020011101000bc48080800001037f4100210123012102230220006b22032402034041082303200320016a6a6c4108200220016a6c290300370300200141016a22012000470d000b200220006a24010bc48080800001037f230120006b22022401230221034100210103404108200220016a6c23034108200320016a6c6a290300370300200141016a22012000470d000b200320006a24020b8f80808000004108230120006a6c29030010010b948080800001027e10592100105921012000105a2001105a0bcb8080800002037f017e230120006a210323012201220241086c29030021040340410820016c200241016a220241086c290300370300200141016a210120012003490d000b410820036c20043703000bc28080800002027f017e4108230120006a22016c29030021030340410820016c210220024108200141016b22016c29030037030023012001490d000b410820016c20033703000b958080800001017f4108230122006c290300200041016a24010b978080800001017f4108230141016b22016c2000370300200124010b898080800000230120006a24010b8c8280800001017e2000105a10591004105941071012410710581059100410591059101c105a1059102204401016105a10561016105a105910591002105a1016105a41051058105910591002105a105910591002105a1016105a41061058105910591002105a1016105a41061058105910591002105a105910591002105a105910591002105a410310581016105a105910591002105a1016105a1016105a105910591002105a105910591002105a410410581016105a105910591002105a1016105a41051058105910591002105a105910591002105a105910591002105a105910591002105a105910591002105a0510564102105841031058410410584105105841061058410710584107105b0b10590b888380800001017e2000105a4100102f105a410110551059100d105a1059100d105a1059100e105a1059100d105a1059100d105a105910591007105a410a102f105a410210551059100d105a1059100e105a1059100d105a1059100e105a1059100e105a105910591007105a410b102f105a410310551059100d105a1059100e105a1059100d105a1059100d105a1059100e105a105910591007105a410c102f105a410410551059100d105a1059100e105a1059100e105a1059100d105a1059100e105a105910591007105a410d102f105a410510551059100d105a1059100d105a1059100e105a1059100e105a1059100d105a105910591007105a410e102f105a410610551059100d105a1059100d105a1059100d105a1059100d105a1059100e105a105910591007105a410f102f105a410710581059100e105a105910591007105a10564102105841031058410410584105105841061058105910591005105a105910591005105a105910591005105a105910591005105a105910591005105a105910591005105a10590bc88a80800001017e2000105a105910041059410810124108105810591004105910210440410210584105105841061058410710584104105b10591021044041031058410410584102105b1059102104404101105b410010551059100d105a1059100d105a1059100d105a1059100d105a1059100e105a4108102f105a105910591002105a41031055105610591059102d105a41021058105910591007105a10561059100d105a1059100d105a1059100d105a1059100d105a1059100e105a4102102f105a105910591002105a41021058105610591059102d105a1056105910591044105a1059102204404109102f105a1059102b000b1059100d105a054104105b1016105a0b05410210584101105b105910210440410210584102105b410010551059100d105a1059100d105a1059100e105a1059100d105a1059100d105a4108102f105a105910591002105a41031055105610591059102d105a41021058105910591007105a10561059100d105a1059100d105a1059100e105a1059100d105a1059100d105a4102102f105a105910591002105a41021058105610591059102d105a1056105910591044105a1059102204404109102f105a1059102b000b1059100d105a05410310584102105b410010551059100d105a1059100d105a1059100e105a1059100e105a1059100d105a4108102f105a105910591002105a41031055105610591059102d105a41021058105910591007105a10561059100d105a1059100d105a1059100e105a1059100e105a1059100d105a4102102f105a105910591002105a41021058105610591059102d105a1056105910591044105a1059102204404109102f105a1059102b000b1059100d105a0b0b054103105841041058410810584103105b10591021044041021058410310584102105b105910210440410310584102105b410010551059100d105a1059100e105a1059100d105a1059100d105a1059100e105a4108102f105a105910591002105a41031055105610591059102d105a41021058105910591007105a10561059100d105a1059100e105a1059100d105a1059100d105a1059100e105a4102102f105a105910591002105a41021058105610591059102d105a1056105910591044105a1059102204404109102f105a1059102b000b1059100d105a05410210584102105b410010551059100d105a1059100e105a1059100d105a1059100e105a1059100e105a4108102f105a105910591002105a41031055105610591059102d105a41021058105910591007105a10561059100d105a1059100e105a1059100d105a1059100e105a1059100e105a4102102f105a105910591002105a41021058105610591059102d105a1056105910591044105a1059102204404109102f105a1059102b000b1059100d105a0b0541041058410510584102105b105910210440410210584102105b410010551059100d105a1059100e105a1059100e105a1059100d105a1059100e105a4108102f105a105910591002105a41031055105610591059102d105a41021058105910591007105a10561059100d105a1059100e105a1059100e105a1059100d105a1059100e105a4102102f105a105910591002105a41021058105610591059102d105a1056105910591044105a1059102204404109102f105a1059102b000b1059100d105a05410310584102105b410010551059100e105a4108102f105a105910591002105a41031055105610591059102d105a41021058105910591007105a10561059100e105a4102102f105a105910591002105a41021058105610591059102d105a1056105910591044105a1059102204404109102f105a1059102b000b1059100d105a0b0b0b10590bb28180800001017e2000105a105910044100102f105a105910591002105a105910591002105a10591035105a034010591021044010591004105910041016105a4103105510591059100c105a10591031105a1059102904401056410210584102105b10591036105a054100102f105a41031058105910591006105a10591030105a410210554103105841031058105910591007105a105910591002105a105910591002105a10591035105a0b105910000d010b0b10590b9cc380800001017e2000105a41001034105a4101102f105a4102102f105a4103102f105a4104102f105a4105102f105a4106102f105a4107102f105a41011034105a410810554108105541081055410810554108105541081055410810554108105541081003105a10591059102e105a410810584101105b41021034105a41031034105a4109105541091055410910554109105541091055410910554109105541071003105a10591059102e105a410310584104105841051058410610584107105841081058410910584107105b4103105810591004101b105a410110551059100e105a1059100d105a10591021044010591021044041051058410610584103105b4102105541011055105910591002105a41041058105610591059102d105a1016105a410310551059100d105a1059100d105a10591059100c105a10591032105a410310551059100d105a1059100d105a410210551059100d105a1059100d105a1059100d105a1059100e105a1059100d105a10591059100c105a10591045105a105910591027105a105910290440410210551059100d105a1059100d105a410110551059100d105a1059100d105a1059100e105a1059100d105a1059100e105a105910591005105a410310581059100d105a1059100d105a410210551059100d105a1059100d105a1059100d105a1059100e105a1059100d105a105910591006105a10591030105a4104105841031058410010551059100e105a410110551059100d105a1059100e105a410210551059100d105a1059100d105a1059100e105a410310551059100d105a1059100d105a1059100d105a1059100e105a1059100e105a41061058105910591002105a410410581059100d105a1059100d105a1059100d105a1059100d105a105910591002105a105910591002105a105910591002105a105910591002105a410010551059100e105a410110551059100d105a1059100e105a410210551059100d105a1059100d105a1059100e105a1059100e105a41051058410410551059100d105a1059100d105a1059100e105a1059100d105a1059100d105a105910591002105a105910591002105a410310581059100d105a1059100d105a1059100d105a105910591002105a105910591002105a105910591002105a1059100f105a41021058105910591059101e105a054103105b0b054101105b410110551059100d105a1059100e105a1059102204404110102f105a1059102b000b41001055105910210440105910210440105910210440410310584102105b4102105541021055105910591002105a41041058105610591059102d105a410010551059100d105a1059100d105a1059100d105a1059100d105a1059100d105a410110551059100d105a1059100d105a1059100d105a1059100e105a1059100d105a10591059100c105a10591045105a1059102904404100102f105a410110551059100d105a1059100d105a1059100d105a1059100d105a1059100e105a105910591005105a410110551059100d105a1059100d105a1059100d105a1059100d105a1059100d105a410210551059100d105a1059100d105a1059100d105a1059100e105a1059100d105a105910591006105a10591030105a41021058410010551059100e105a410110551059100d105a1059100e105a410210551059100d105a1059100d105a1059100e105a410310551059100d105a1059100d105a1059100d105a1059100e105a1059100e105a41051058105910591002105a410410581059100d105a1059100d105a1059100d105a1059100d105a105910591002105a105910591002105a105910591002105a105910591002105a410010551059100e105a410110551059100d105a1059100e105a410210551059100d105a1059100d105a1059100e105a410310551059100d105a1059100d105a1059100d105a1059100e105a41051058410510581059100d105a1059100d105a1059100d105a1059100d105a1059100d105a105910591002105a105910591002105a105910591002105a105910591002105a105910591002105a4100105541021058105910591002105a41051058105610591059102d105a4101105541051058105610591059102d105a41021058410010551059100e105a410110551059100d105a1059100e105a410210551059100d105a1059100d105a1059100e105a410310551059100d105a1059100d105a1059100d105a1059100e105a410410581059100d105a1059100d105a1059100d105a1059100d105a1059100e105a41061058105910591002105a105910591002105a105910591002105a105910591002105a105910591002105a410010551059100e105a410110551059100d105a1059100e105a1059100e105a1059100e105a1059100e105a41031058105910591002105a410210551059100d105a1059100e105a1059100e105a1059100d105a105910591002105a410210551059100d105a1059100e105a1059100d105a105910591002105a410210581059100d105a1059100d105a105910591002105a105910591002105a4102105810561059100f105a41021058105910591059101e105a0510564102105841041058410510584105105b0b05105641061058410710584104105b4102105541011055105910591002105a41041058105610591059102d105a410210581059100d105a1059100d105a410110551059100d105a1059100d105a1059100d105a1059100e105a1059100d105a105910591005105a4103105841021058410010551059100e105a410110551059100d105a1059100e105a410210551059100d105a1059100d105a1059100e105a410310551059100d105a1059100d105a1059100d105a1059100e105a1059100e105a41061058105910591002105a410410581059100d105a1059100d105a1059100d105a1059100d105a105910591002105a105910591002105a105910591002105a105910591002105a1059100f105a41021058105910591059101e105a0b05410310584101105b1059102104404101105b4102105541021055105910591002105a41041058105610591059102d105a410010551059100d105a1059100d105a1059100d105a1059100e105a1059100e105a410110551059100d105a1059100d105a1059100d105a1059100e105a1059100d105a10591059100c105a10591045105a1059102904404100102f105a410110551059100d105a1059100d105a1059100e105a1059100d105a1059100d105a105910591005105a410110551059100d105a1059100d105a1059100d105a1059100e105a1059100e105a410210551059100d105a1059100d105a1059100d105a1059100e105a1059100d105a105910591006105a10591030105a41021058410010551059100e105a410110551059100d105a1059100e105a410210551059100d105a1059100d105a1059100e105a410310551059100d105a1059100d105a1059100d105a1059100e105a1059100e105a41051058105910591002105a410410581059100d105a1059100d105a1059100d105a1059100d105a105910591002105a105910591002105a105910591002105a105910591002105a410010551059100e105a410110551059100d105a1059100e105a410210551059100d105a1059100d105a1059100e105a1059100e105a410310551059100d105a1059100d105a1059100e105a1059100d105a1059100e105a41051058105910591002105a105910591002105a410310581059100d105a1059100d105a1059100d105a105910591002105a105910591002105a105910591002105a4100105541021058105910591002105a41051058105610591059102d105a4101105541051058105610591059102d105a41021058410010551059100e105a410110551059100d105a1059100e105a410210551059100d105a1059100d105a1059100e105a41051058410410551059100d105a1059100d105a1059100d105a1059100e105a1059100d105a105910591002105a410410581059100d105a1059100d105a1059100d105a1059100d105a105910591002105a105910591002105a105910591002105a105910591002105a410010551059100e105a410110551059100d105a1059100e105a1059100e105a1059100e105a1059100e105a41031058105910591002105a410210551059100d105a1059100e105a1059100e105a1059100d105a105910591002105a410210551059100d105a1059100e105a1059100d105a105910591002105a410210581059100d105a1059100d105a105910591002105a105910591002105a4102105810561059100f105a41021058105910591059101e105a0510564102105841041058410510584105105b0b054101105b4102105541021055105910591002105a41041058105610591059102d105a410010551059100d105a1059100d105a1059100e105a1059100e105a1059100e105a410110551059100d105a1059100d105a1059100d105a1059100e105a1059100d105a10591059100c105a10591045105a1059102904404100102f105a410110551059100d105a1059100d105a1059100e105a1059100e105a1059100d105a105910591005105a410110551059100d105a1059100d105a1059100e105a1059100e105a1059100e105a410210551059100d105a1059100d105a1059100d105a1059100e105a1059100d105a105910591006105a10591030105a41021058410010551059100e105a410110551059100d105a1059100e105a410210551059100d105a1059100d105a1059100e105a410310551059100d105a1059100d105a1059100d105a1059100e105a1059100e105a41051058105910591002105a410410581059100d105a1059100d105a1059100d105a1059100d105a105910591002105a105910591002105a105910591002105a105910591002105a410010551059100e105a410110551059100d105a1059100e105a410210551059100d105a1059100d105a1059100e105a1059100e105a1059100e105a41041058105910591002105a410310551059100d105a1059100d105a1059100e105a1059100d105a105910591002105a410310581059100d105a1059100d105a1059100d105a105910591002105a105910591002105a105910591002105a4100105541021058105910591002105a41051058105610591059102d105a4101105541051058105610591059102d105a41021058410010551059100e105a410110551059100d105a1059100e105a41041058410310551059100d105a1059100d105a1059100e105a1059100e105a1059100d105a105910591002105a410310551059100d105a1059100d105a1059100e105a1059100d105a105910591002105a410310581059100d105a1059100d105a1059100d105a105910591002105a105910591002105a105910591002105a410010551059100e105a410110551059100d105a1059100e105a1059100e105a1059100e105a1059100e105a41031058105910591002105a410210551059100d105a1059100e105a1059100e105a1059100d105a105910591002105a410210551059100d105a1059100e105a1059100d105a105910591002105a410210581059100d105a1059100d105a105910591002105a105910591002105a4102105810561059100f105a41021058105910591059101e105a0510564102105841041058410510584105105b0b0b0b05410310584101105b1059102104401059102104404101105b4102105541021055105910591002105a41041058105610591059102d105a410010551059100d105a1059100e105a1059100d105a1059100d105a1059100d105a410110551059100d105a1059100d105a1059100d105a1059100e105a1059100d105a10591059100c105a10591045105a1059102904404100102f105a410110551059100d105a1059100e105a1059100d105a1059100d105a1059100e105a105910591005105a410110551059100d105a1059100e105a1059100d105a1059100d105a1059100d105a410210551059100d105a1059100d105a1059100d105a1059100e105a1059100d105a105910591006105a10591030105a41021058410010551059100e105a410110551059100d105a1059100e105a410210551059100d105a1059100d105a1059100e105a410310551059100d105a1059100d105a1059100d105a1059100e105a1059100e105a41051058105910591002105a410410581059100d105a1059100d105a1059100d105a1059100d105a105910591002105a105910591002105a105910591002105a105910591002105a410010551059100e105a410110551059100d105a1059100e105a1059100e105a410210551059100d105a1059100e105a1059100d105a1059100e105a41041058410410551059100d105a1059100e105a1059100d105a1059100d105a1059100d105a105910591002105a105910591002105a105910591002105a410210581059100d105a1059100d105a105910591002105a105910591002105a4100105541021058105910591002105a41051058105610591059102d105a4101105541051058105610591059102d105a41021058410010551059100e105a410110551059100d105a1059100e105a1059100e105a410210551059100d105a1059100e105a1059100d105a1059100e105a410310551059100d105a1059100e105a1059100d105a1059100d105a1059100e105a41061058105910591002105a105910591002105a105910591002105a410210581059100d105a1059100d105a105910591002105a105910591002105a410010551059100e105a410110551059100d105a1059100e105a1059100e105a1059100e105a1059100e105a41031058105910591002105a410210551059100d105a1059100e105a1059100e105a1059100d105a105910591002105a410210551059100d105a1059100e105a1059100d105a105910591002105a410210581059100d105a1059100d105a105910591002105a105910591002105a4102105810561059100f105a41021058105910591059101e105a0510564102105841041058410510584105105b0b054101105b4102105541021055105910591002105a41041058105610591059102d105a410010551059100d105a1059100e105a1059100d105a1059100e105a1059100d105a410110551059100d105a1059100d105a1059100d105a1059100e105a1059100d105a10591059100c105a10591045105a1059102904404100102f105a410110551059100d105a1059100e105a1059100d105a1059100e105a1059100e105a105910591005105a410110551059100d105a1059100e105a1059100d105a1059100e105a1059100d105a410210551059100d105a1059100d105a1059100d105a1059100e105a1059100d105a105910591006105a10591030105a41021058410010551059100e105a410110551059100d105a1059100e105a410210551059100d105a1059100d105a1059100e105a410310551059100d105a1059100d105a1059100d105a1059100e105a1059100e105a41051058105910591002105a410410581059100d105a1059100d105a1059100d105a1059100d105a105910591002105a105910591002105a105910591002105a105910591002105a410010551059100e105a410110551059100d105a1059100e105a1059100e105a41031058410310551059100d105a1059100e105a1059100d105a1059100e105a1059100d105a105910591002105a410310551059100d105a1059100e105a1059100d105a1059100d105a105910591002105a105910591002105a410210581059100d105a1059100d105a105910591002105a105910591002105a4100105541021058105910591002105a41051058105610591059102d105a4101105541051058105610591059102d105a41021058410010551059100e105a410110551059100d105a1059100e105a1059100e105a410210551059100d105a1059100e105a1059100d105a1059100e105a1059100e105a41051058105910591002105a410310551059100d105a1059100e105a1059100d105a1059100d105a105910591002105a105910591002105a410210581059100d105a1059100d105a105910591002105a105910591002105a410010551059100e105a410110551059100d105a1059100e105a1059100e105a1059100e105a1059100e105a41031058105910591002105a410210551059100d105a1059100e105a1059100e105a1059100d105a105910591002105a410210551059100d105a1059100e105a1059100d105a105910591002105a410210581059100d105a1059100d105a105910591002105a105910591002105a4102105810561059100f105a41021058105910591059101e105a0510564102105841041058410510584105105b0b0b051059102104404101105b4102105541021055105910591002105a41041058105610591059102d105a410010551059100d105a1059100e105a1059100e105a1059100d105a1059100d105a410110551059100d105a1059100d105a1059100d105a1059100e105a1059100d105a10591059100c105a10591045105a1059102904404100102f105a410110551059100d105a1059100e105a1059100e105a1059100d105a1059100e105a105910591005105a410110551059100d105a1059100e105a1059100e105a1059100d105a1059100d105a410210551059100d105a1059100d105a1059100d105a1059100e105a1059100d105a105910591006105a10591030105a41021058410010551059100e105a410110551059100d105a1059100e105a410210551059100d105a1059100d105a1059100e105a410310551059100d105a1059100d105a1059100d105a1059100e105a1059100e105a41051058105910591002105a410410581059100d105a1059100d105a1059100d105a1059100d105a105910591002105a105910591002105a105910591002105a105910591002105a410010551059100e105a410110551059100d105a1059100e105a1059100e105a1059100e105a41031058410310551059100d105a1059100e105a1059100e105a1059100d105a1059100d105a105910591002105a105910591002105a410210551059100d105a1059100e105a1059100d105a105910591002105a410210581059100d105a1059100d105a105910591002105a105910591002105a4100105541021058105910591002105a41051058105610591059102d105a4101105541051058105610591059102d105a41021058410010551059100e105a410110551059100d105a1059100e105a1059100e105a1059100e105a410210551059100d105a1059100e105a1059100e105a1059100d105a1059100e105a41051058105910591002105a105910591002105a410210551059100d105a1059100e105a1059100d105a105910591002105a410210581059100d105a1059100d105a105910591002105a105910591002105a410010551059100e105a410110551059100d105a1059100e105a1059100e105a1059100e105a1059100e105a41031058105910591002105a410210551059100d105a1059100e105a1059100e105a1059100d105a105910591002105a410210551059100d105a1059100e105a1059100d105a105910591002105a410210581059100d105a1059100d105a105910591002105a105910591002105a4102105810561059100f105a41021058105910591059101e105a0510564102105841041058410510584105105b0b054101105b4102105541021055105910591002105a41041058105610591059102d105a410010551059100d105a1059100e105a1059100e105a1059100e105a1059100e105a410110551059100d105a1059100d105a1059100d105a1059100e105a1059100d105a10591059100c105a10591045105a1059102904404100102f105a410110551059100e105a105910591005105a410110551059100d105a1059100e105a1059100e105a1059100e105a1059100e105a410210551059100d105a1059100d105a1059100d105a1059100e105a1059100d105a105910591006105a10591030105a41021058410010551059100e105a410110551059100d105a1059100e105a410210551059100d105a1059100d105a1059100e105a410310551059100d105a1059100d105a1059100d105a1059100e105a1059100e105a41051058105910591002105a410410581059100d105a1059100d105a1059100d105a1059100d105a105910591002105a105910591002105a105910591002105a10564101105b105910591002105a4100105541021058105910591002105a41051058105610591059102d105a4101105541051058105610591059102d105a41021058410010551059100e105a41031058410210551059100d105a1059100e105a1059100e105a1059100e105a1059100d105a105910591002105a410210551059100d105a1059100e105a1059100e105a1059100d105a105910591002105a410210551059100d105a1059100e105a1059100d105a105910591002105a410210581059100d105a1059100d105a105910591002105a105910591002105a410010551059100e105a410110551059100d105a1059100e105a1059100e105a1059100e105a1059100e105a41031058105910591002105a410210551059100d105a1059100e105a1059100e105a1059100d105a105910591002105a410210551059100d105a1059100e105a1059100d105a105910591002105a410210581059100d105a1059100d105a105910591002105a105910591002105a4102105810561059100f105a41021058105910591059101e105a0510564102105841041058410510584105105b0b0b0b0b0b0541051058410610584103105b4102105541011055105910591002105a41041055105610591059102d105a1016105a410310551059100d105a1059100d105a10591059100c105a10591032105a410310551059100d105a1059100d105a410210551059100d105a1059100d105a1059100d105a1059100e105a1059100d105a10591059100c105a10591045105a105910591027105a105910290440410210551059100e105a1059100e105a1059102204404111102f105a1059102b000b4104105541011055105910591002105a41061058105610591059102d105a410410581059100d105a1059100d105a41001055410410551059100d105a1059100d105a1059100d105a1059100e105a1059100d105a105910591006105a1056410210551059100d105a1059100d105a1059100d105a1059100e105a1059100d105a105910591005105a4106105841051058410010551059100e105a410110551059100d105a1059100e105a410210551059100d105a1059100d105a1059100e105a410310551059100d105a1059100d105a1059100d105a1059100e105a1059100e105a4107105810591030105a105910591002105a410410581059100d105a1059100d105a1059100d105a1059100d105a105910591002105a105910591002105a105910591002105a105910591002105a1059100f105a41051058105910591059101e105a41021058410010551059100e105a410110551059100d105a1059100e105a410210551059100d105a1059100d105a1059100e105a410310551059100d105a1059100d105a1059100d105a1059100e105a1059100e105a41061058105910591002105a410410581059100d105a1059100d105a1059100d105a1059100d105a105910591002105a105910591002105a105910591002105a105910591002105a1059100f105a41021058105910591059101e105a05105641021058410410584104105b0b0b1010105a105910591002105a10590b",
      "constants":
        [ [ 0, [ "Int", "1" ] ], [ 1, [ "Int", "15" ] ],
          [ 2, [ "Int", "100" ] ], [ 3, [ "Int", "1100" ] ],
          [ 4, [ "Int", "12000" ] ], [ 5, [ "Int", "130000" ] ],
          [ 6, [ "Int", "1400000" ] ], [ 7, [ "Int", "20000000" ] ],
          [ 8, [ "Int", "115" ] ], [ 9, [ "String", "DIV by 0" ] ],
          [ 10, [ "Int", "3" ] ], [ 11, [ "Int", "8" ] ],
          [ 12, [ "Int", "47" ] ], [ 13, [ "Int", "260" ] ],
          [ 14, [ "Int", "1400" ] ], [ 15, [ "Int", "7800" ] ],
          [ 16, [ "String", "Operation is mandatory for minting" ] ],
          [ 17, [ "String", "There is not recipient" ] ] ],
      "entrypoints": {} }, "tickets": [] }
|}
  in
  (* Change this string with your appropriate needs*)
  let _content =
    {| 
    {"operation":"{ \"address\": \"DK1AYirVcQa1sVtXFGhz9TddMbWw71WbBxx7\",\n  \"argument\":\n    [ \"Pair\",\n      [ [ \"Pair\",\n          [ [ \"Int\", \"1\" ],\n            [ \"Option\",\n              [ \"Some\",\n                [ \"Union\",\n                  [ \"Left\",\n                    [ \"Union\",\n                      [ \"Left\", [ \"Union\", [ \"Right\", [ \"Unit\" ] ] ] ] ] ] ] ] ] ] ],\n        [ \"Pair\",\n          [ [ \"Union\", [ \"Left\", [ \"Union\", [ \"Right\", [ \"Unit\" ] ] ] ] ],\n            [ \"Option\", [ \"None\", {} ] ] ] ] ] ] }","tickets":[]}
    |}
  in
  let operation = Data_encoding.Json.from_string _content2 in
  let operation =
    match operation with
    | Ok operation ->
        Data_encoding.Json.destruct Ocaml_wasm_vm.Operation_payload.encoding
          operation
    | _ -> failwith "impossible to decode operation"
  in
  print_endline (Ocaml_wasm_vm.Operation_payload.show operation);

  let (Deku_protocol.Operation.Signed.Signed_operation transaction as op) =
    Operation.Signed.vm_transaction ~level ~nonce ~content:operation ~identity
  in
  let (Deku_protocol.Operation.Initial.Initial_operation { hash; _ }) =
    transaction.initial
  in
  Format.printf "hash: %a\n%!" Operation_hash.pp hash;
  let address =
    Deku_ledger.Contract_address.of_user_operation_hash
      (Deku_protocol.Operation_hash.to_blake2b hash)
    |> Deku_ledger.Contract_address.to_b58
  in
  (match operation.operation with
  | Originate _ ->
      print_newline ();
      print_endline ("Address: " ^ address ^ "\n");
      print_newline ()
  | _ -> ());
  let _ = post_directly_to_node ~identity ~env ~operation:op in
  ()

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw -> main ~env ~sw
