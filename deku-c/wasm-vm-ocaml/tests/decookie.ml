let decookie_invoke =
  let str =
    {|  {"operation":"{ \"address\": \"DK1NmndDdhkWdWpX7NMArqEjjnWR3xLfM4Kf\",\n  \"argument\":\n    [ \"Pair\",\n      [ [ \"Pair\",\n          [ [ \"Int\", \"1\" ],\n            [ \"Option\",\n              [ \"Some\",\n                [ \"Union\",\n                  [ \"Left\",\n                    [ \"Union\",\n                      [ \"Left\", [ \"Union\", [ \"Right\", [ \"Unit\" ] ] ] ] ] ] ] ] ] ] ],\n        [ \"Pair\",\n          [ [ \"Union\", [ \"Left\", [ \"Union\", [ \"Right\", [ \"Unit\" ] ] ] ] ],\n            [ \"Option\", [ \"None\", {} ] ] ] ] ] ] }","tickets":[]} |}
  in
  let res =
    Ocaml_wasm_vm.Operation_payload.t_of_yojson @@ Yojson.Safe.from_string str
  in
  res.operation

let decookie_originate =
  let str =
    {|  {"operation":"{ \"initial_storage\": [ \"Map\", [] ],\n  \"module\":\n    \"0061736d0100000001c3808080000d60017e017e60017e0060027e7e017e6000017e60037e7e7e017e60027e7f0060027e7f017e60017e017f60027f7e017e60017f017e60037e7e7e0060017f0060000002e0878080004e03656e76086475705f686f7374000103656e760470616972000203656e7606756e70616972000103656e76057a5f616464000203656e76057a5f737562000203656e76057a5f6d756c000203656e76036e6567000003656e76036c736c000203656e7606636f6e636174000203656e76036c7372000203656e7607636f6d70617265000203656e7603636172000003656e7603636472000003656e7604736f6d65000003656e76036e696c000303656e760474727565000303656e760566616c7365000303656e76046e6f6e65000303656e7604756e6974000303656e76047a65726f000303656e7609656d7074795f6d6170000303656e7609656d7074795f736574000303656e760d656d7074795f6269675f6d6170000303656e760673656e646572000303656e7606736f75726365000303656e76076d61705f676574000203656e76036d656d000203656e7606757064617465000403656e760469746572000503656e76036d6170000603656e760769665f6c656674000703656e760769665f6e6f6e65000703656e760769665f636f6e73000703656e760569736e6174000003656e76036e6f74000003656e76026f72000203656e7603616e64000203656e7603786f72000203656e760a64657265665f626f6f6c000703656e76036e6571000003656e76086661696c77697468000103656e76056765745f6e000803656e760465786563000203656e76056170706c79000203656e7605636f6e7374000903656e7603616273000003656e76026571000003656e76026774000003656e76026c74000003656e7607636c6f73757265000903656e76046c656674000003656e76057269676874000003656e7604636f6e73000203656e760f7472616e736665725f746f6b656e73000403656e760761646472657373000003656e7608636f6e7472616374000003656e760473656c66000303656e760c73656c665f61646472657373000303656e760e6765745f616e645f757064617465000a03656e760b726561645f7469636b6574000103656e76067469636b6574000203656e760c6a6f696e5f7469636b657473000003656e760c73706c69745f7469636b6574000203656e7606616d6f756e74000303656e760762616c616e6365000303656e760465646976000203656e76026765000003656e76026c65000003656e760473697a65000003656e7603696e74000003656e7610696d706c696369745f6163636f756e74000003656e7607626c616b653262000003656e76047061636b000003656e7606756e7061636b000003656e76066b656363616b000003656e7606736861323536000003656e760473686133000003656e760673686135313200000391808080001006050b0b0b0c0b0b03010b00000000000485808080000170010404058380808000010004069980808000047f0041000b7f0141a01f0b7f0141e8070b7f00418080020b07c580808000060470757368005703706f700057046d61696e005d08636c6f737572657301000d63616c6c5f63616c6c6261636b004e1263616c6c5f63616c6c6261636b5f756e6974004f098a80808000010041000b045c5b5a590a82d080800010898080800000200020011100000b898080800000200020011101000bc48080800001037f4100210123012102230220006b22032402034041082303200320016a6a6c4108200220016a6c290300370300200141016a22012000470d000b200220006a24010bc48080800001037f230120006b22022401230221034100210103404108200220016a6c23034108200320016a6c6a290300370300200141016a22012000470d000b200320006a24020b8f80808000004108230120006a6c29030010000b948080800001027e105621001056210120001057200110570bcb8080800002037f017e230120006a210323012201220241086c29030021040340410820016c200241016a220241086c290300370300200141016a210120012003490d000b410820036c20043703000bc28080800002027f017e4108230120006a22016c29030021030340410820016c210220024108200141016b22016c29030037030023012001490d000b410820016c20033703000b958080800001017f4108230122006c290300200041016a24010b978080800001017f4108230141016b22016c2000370300200124010b898080800000230120006a24010be18180800001017e200010571056100210561056101910571056101f0440101310571013105710131057105610561001105710131057101310571056105610011057105610561001105710131057101310571056105610011057101310571013105710561056100110571056105610011057105610561001105710131057101310571056105610011057101310571013105710561056100110571056105610011057101310571013105710561056100110571013105710131057105610561001105710561056100110571056105610011057105610561001105710561056100110570b10560ba88280800001017e200010574100102c1057410110521056100b10571056100b10571056100c10571056100b10571056100b105710561056100510574109102c1057410210521056100b10571056100c10571056100b10571056100c10571056100c10571056105610051057410a102c1057410310521056100b10571056100c10571056100b10571056100b10571056100c10571056105610051057410b102c1057410410521056100b10571056100c10571056100c10571056100b10571056100c10571056105610051057410c102c1057410510551056100b10571056100b10571056100c10571056100c10571056100b105710561056100510571053410210554103105541041055105610561003105710561056100310571056105610031057105610561003105710560b9e8980800001017e20001057105610021053105610021056100b10571056100c10571056101e04401056101e04401056101e044041011058410010521056100b10571056100b10571056100b10571056100b10571056100c10574101102c1057105610561001105741021055105310561056102a10574102102c105710561056100510574103102c1057410210551056100b10571056100b10571056100b10571056100b10571056100c10571056105610051057105310561056104110571056101f04404104102c105710561028000b1056100b10570541031058101310570b051056101e044041011058410010521056100b10571056100b10571056100c10571056100b10571056100b10574101102c1057105610561001105741021055105310561056102a10574105102c105710561056100510574103102c1057410210551056100b10571056100b10571056100c10571056100b10571056100b10571056105610051057105310561056104110571056101f04404104102c105710561028000b1056100b10570541011058410010521056100b10571056100b10571056100c10571056100c10571056100b10574101102c1057105610561001105741021055105310561056102a10574102102c105710561056100510574103102c1057410210551056100b10571056100b10571056100c10571056100c10571056100b10571056105610051057105310561056104110571056101f04404104102c105710561028000b1056100b10570b0b051056101e04401056101e044041011058410010521056100b10571056100c10571056100b10571056100b10571056100c10574101102c1057105610561001105741021055105310561056102a10574106102c105710561056100510574103102c1057410210551056100b10571056100c10571056100b10571056100b10571056100c10571056105610051057105310561056104110571056101f04404104102c105710561028000b1056100b10570541011058410010521056100b10571056100c10571056100b10571056100c10571056100c10574101102c1057105610561001105741021055105310561056102a10574103102c105710561056100510574103102c1057410210551056100b10571056100b10571056100c10571056100b10571056100b10571056105610051057105310561056104110571056101f04404104102c105710561028000b1056100b10570b051056101e044041011058410010521056100b10571056100c10571056100c10571056100b10571056100c10574101102c1057105610561001105741021055105310561056102a10574107102c105710561056100510574103102c1057410210551056100b10571056100c10571056100c10571056100b10571056100c10571056105610051057105310561056104110571056101f04404104102c105710561028000b1056100b10570541011058410010521056100c10574101102c1057105610561001105741021055105310561056102a10574108102c105710561056100510574103102c1057410210551056100c10571056105610051057105310561056104110571056101f04404104102c105710561028000b1056100b10570b0b0b10560bab8180800001017e20001057105610024100102c10571056105610011057105610561001105710561032105703401056101e0d001056100210561002101310574103105210561056100a10571056102e105710561026044010534102105541021058105610331057054100102c10574103105510561056100410571056102d10574102105241031055410310551056105610051057105610561001105710561056100110571056103210570b0b10560be0bd80800001017e200010574100103110574101103110574101105210561056102b1057105341011058410210311057410310311057410310551056100210181057410110521056100c10571056100b10571056101e04401056101e0440410210554105105541061055410410584101105241011052105610561001105741031055105310561056102a10574100102c1057410110521056100b10571056100b10571056100c10571056100b10571056100c105710561056100310574103105541021055410010521056100c1057410110521056100b10571056100c1057410210521056100b10571056100b10571056100c10571056100c105741051055410410521056100b10571056100b10571056100c10571056100b10571056100b105710561056100110571056105610011057410310551056100b10571056100b10571056100b10571056105610011057105610561001105710561056100110571056100d105741021055105610561056101b10570541011058410110521056100b10571056100c10571056101e04401056101e04401056101e0440410110584102105241011052105610561001105741041055105310561056102a1057410010521056100b10571056100b10571056100b10571056100b10571056100b1057410110521056100b10571056100b10571056100b10571056100c10571056100b105710561056100a10571056104210571056102604404100102c1057410110521056100b10571056100b10571056100b10571056100b10571056100c10571056105610031057410110521056100b10571056100b10571056100b10571056100b10571056100b1057410210521056100b10571056100b10571056100b10571056100c10571056100b105710561056100410571056102d10574102105241051055105610561001105741071055105310561056102a10574103105241071055105310561056102a10574106105541051055410010521056100c1057410110521056100b10571056100c1057410210521056100b10571056100b10571056100c1057410310521056100b10571056100b10571056100b10571056100c10571056100c1057410810551056105610011057410410551056100b10571056100b10571056100b10571056100b10571056105610011057105610561001105710561056100110571056105610011057410010521056100c1057410110521056100b10571056100c1057410210521056100b10571056100b10571056100c1057410310521056100b10571056100b10571056100b10571056100c105741081055410510551056100b10571056100b10571056100b10571056100b10571056100b105710561056100110571056105610011057105610561001105710561056100110571056105610011057410010521056100c1057410110521056100b10571056100c1057410210521056100b10571056100b10571056100c1057410310521056100b10571056100b10571056100b10571056100c1057410410551056100b10571056100b10571056100b10571056100b10571056100c10574107105510561056100110571056105610011057105610561001105710561056100110571056105610011057410010521056100c1057410110521056100b10571056100c10571056100c10571056100c10571056100c1057410410551056105610011057410210521056100b10571056100c10571056100c10571056100b10571056105610011057410210521056100b10571056100c10571056100b10571056105610011057410210551056100b10571056100b1057105610561001105710561056100110571056100d105741021055105610561056101b1057051053410210554104105541051055410510580b05410210554105105541061055410410584101105241011052105610561001105741031055105310561056102a10574100102c1057410110521056100b10571056100b10571056100b10571056100c10571056100b105710561056100310574103105541021055410010521056100c1057410110521056100b10571056100c1057410210521056100b10571056100b10571056100c1057410310521056100b10571056100b10571056100b10571056100c10571056100c1057410610551056105610011057410410551056100b10571056100b10571056100b10571056100b105710561056100110571056105610011057105610561001105710561056100110571056100d105741021055105610561056101b10570b051056101e0440410110584102105241011052105610561001105741041055105310561056102a1057410010521056100b10571056100b10571056100b10571056100c10571056100c1057410110521056100b10571056100b10571056100b10571056100c10571056100b105710561056100a10571056104210571056102604404100102c1057410110521056100b10571056100b10571056100c10571056100b10571056100b10571056105610031057410110521056100b10571056100b10571056100b10571056100c10571056100c1057410210521056100b10571056100b10571056100b10571056100c10571056100b105710561056100410571056102d10574102105241051055105610561001105741071055105310561056102a10574103105241071055105310561056102a10574106105541051055410010521056100c1057410110521056100b10571056100c1057410210521056100b10571056100b10571056100c1057410310521056100b10571056100b10571056100b10571056100c10571056100c1057410810551056105610011057410410551056100b10571056100b10571056100b10571056100b10571056105610011057105610561001105710561056100110571056105610011057410010521056100c1057410110521056100b10571056100c1057410210521056100b10571056100b10571056100c10571056100c1057410310521056100b10571056100b10571056100c10571056100b10571056100c10574108105510561056100110571056105610011057410310551056100b10571056100b10571056100b1057105610561001105710561056100110571056105610011057410010521056100c1057410110521056100b10571056100c1057410210521056100b10571056100b10571056100c105741061055410410521056100b10571056100b10571056100b10571056100c10571056100b10571056105610011057410410551056100b10571056100b10571056100b10571056100b10571056105610011057105610561001105710561056100110571056105610011057410010521056100c1057410110521056100b10571056100c10571056100c10571056100c10571056100c1057410410551056105610011057410210521056100b10571056100c10571056100c10571056100b10571056105610011057410210521056100b10571056100c10571056100b10571056105610011057410210551056100b10571056100b1057105610561001105710561056100110571056100d105741021055105610561056101b1057051053410210554104105541051055410510580b05410110584102105241011052105610561001105741041055105310561056102a1057410010521056100b10571056100b10571056100c10571056100c10571056100c1057410110521056100b10571056100b10571056100b10571056100c10571056100b105710561056100a10571056104210571056102604404100102c1057410110521056100b10571056100b10571056100c10571056100c10571056100b10571056105610031057410110521056100b10571056100b10571056100c10571056100c10571056100c1057410210521056100b10571056100b10571056100b10571056100c10571056100b105710561056100410571056102d10574102105241051055105610561001105741071055105310561056102a10574103105241071055105310561056102a10574106105541051055410010521056100c1057410110521056100b10571056100c1057410210521056100b10571056100b10571056100c1057410310521056100b10571056100b10571056100b10571056100c10571056100c1057410810551056105610011057410410551056100b10571056100b10571056100b10571056100b10571056105610011057105610561001105710561056100110571056105610011057410010521056100c1057410110521056100b10571056100c1057410210521056100b10571056100b10571056100c10571056100c10571056100c1057410710551056105610011057410310521056100b10571056100b10571056100c10571056100b10571056105610011057410310551056100b10571056100b10571056100b1057105610561001105710561056100110571056105610011057410010521056100c1057410110521056100b10571056100c105741051055410310521056100b10571056100b10571056100c10571056100c10571056100b10571056105610011057410310521056100b10571056100b10571056100c10571056100b10571056105610011057410310551056100b10571056100b10571056100b1057105610561001105710561056100110571056105610011057410010521056100c1057410110521056100b10571056100c10571056100c10571056100c10571056100c1057410410551056105610011057410210521056100b10571056100c10571056100c10571056100b10571056105610011057410210521056100b10571056100c10571056100b10571056105610011057410210551056100b10571056100b1057105610561001105710561056100110571056100d105741021055105610561056101b1057051053410210554104105541051055410510580b0b0b051056101e04401056101e0440410110584102105241011052105610561001105741041055105310561056102a1057410010521056100b10571056100c10571056100b10571056100b10571056100b1057410110521056100b10571056100b10571056100b10571056100c10571056100b105710561056100a10571056104210571056102604404100102c1057410110521056100b10571056100c10571056100b10571056100b10571056100c10571056105610031057410110521056100b10571056100c10571056100b10571056100b10571056100b1057410210521056100b10571056100b10571056100b10571056100c10571056100b105710561056100410571056102d10574102105241051055105610561001105741071055105310561056102a10574103105241071055105310561056102a10574106105541051055410010521056100c1057410110521056100b10571056100c1057410210521056100b10571056100b10571056100c1057410310521056100b10571056100b10571056100b10571056100c10571056100c1057410810551056105610011057410410551056100b10571056100b10571056100b10571056100b10571056105610011057105610561001105710561056100110571056105610011057410010521056100c1057410110521056100b10571056100c10571056100c1057410210521056100b10571056100c10571056100b10571056100c105741071055410410521056100b10571056100c10571056100b10571056100b10571056100b1057105610561001105710561056100110571056105610011057410210551056100b10571056100b105710561056100110571056105610011057410010521056100c1057410110521056100b10571056100c10571056100c1057410210521056100b10571056100c10571056100b10571056100c1057410310521056100b10571056100c10571056100b10571056100b10571056100c105741071055105610561001105710561056100110571056105610011057410210551056100b10571056100b105710561056100110571056105610011057410010521056100c1057410110521056100b10571056100c10571056100c10571056100c10571056100c1057410410551056105610011057410210521056100b10571056100c10571056100c10571056100b10571056105610011057410210521056100b10571056100c10571056100b10571056105610011057410210551056100b10571056100b1057105610561001105710561056100110571056100d105741021055105610561056101b1057051053410210554104105541051055410510580b05410110584102105241011052105610561001105741041055105310561056102a1057410010521056100b10571056100c10571056100b10571056100c10571056100b1057410110521056100b10571056100b10571056100b10571056100c10571056100b105710561056100a10571056104210571056102604404100102c1057410110521056100b10571056100c10571056100b10571056100c10571056100c10571056105610031057410110521056100b10571056100c10571056100b10571056100c10571056100b1057410210521056100b10571056100b10571056100b10571056100c10571056100b105710561056100410571056102d10574102105241051055105610561001105741071055105310561056102a10574103105241071055105310561056102a10574106105541051055410010521056100c1057410110521056100b10571056100c1057410210521056100b10571056100b10571056100c1057410310521056100b10571056100b10571056100b10571056100c10571056100c1057410810551056105610011057410410551056100b10571056100b10571056100b10571056100b10571056105610011057105610561001105710561056100110571056105610011057410010521056100c1057410110521056100b10571056100c10571056100c105741061055410310521056100b10571056100c10571056100b10571056100c10571056100b10571056105610011057410310521056100b10571056100c10571056100b10571056100b105710561056100110571056105610011057410210551056100b10571056100b105710561056100110571056105610011057410010521056100c1057410110521056100b10571056100c10571056100c1057410210521056100b10571056100c10571056100b10571056100c10571056100c1057410610551056105610011057410310521056100b10571056100c10571056100b10571056100b105710561056100110571056105610011057410210551056100b10571056100b105710561056100110571056105610011057410010521056100c1057410110521056100b10571056100c10571056100c10571056100c10571056100c1057410410551056105610011057410210521056100b10571056100c10571056100c10571056100b10571056105610011057410210521056100b10571056100c10571056100b10571056105610011057410210551056100b10571056100b1057105610561001105710561056100110571056100d105741021055105610561056101b1057051053410210554104105541051055410510580b0b051056101e0440410110584102105241011052105610561001105741041055105310561056102a1057410010521056100b10571056100c10571056100c10571056100b10571056100b1057410110521056100b10571056100b10571056100b10571056100c10571056100b105710561056100a10571056104210571056102604404100102c1057410110521056100b10571056100c10571056100c10571056100b10571056100c10571056105610031057410110521056100b10571056100c10571056100c10571056100b10571056100b1057410210521056100b10571056100b10571056100b10571056100c10571056100b105710561056100410571056102d10574102105241051055105610561001105741071055105310561056102a10574103105241071055105310561056102a10574106105541051055410010521056100c1057410110521056100b10571056100c1057410210521056100b10571056100b10571056100c1057410310521056100b10571056100b10571056100b10571056100c10571056100c1057410810551056105610011057410410551056100b10571056100b10571056100b10571056100b10571056105610011057105610561001105710561056100110571056105610011057410010521056100c1057410110521056100b10571056100c10571056100c10571056100c105741061055410310521056100b10571056100c10571056100c10571056100b10571056100b105710561056100110571056105610011057410210521056100b10571056100c10571056100b10571056105610011057410210551056100b10571056100b105710561056100110571056105610011057410010521056100c1057410110521056100b10571056100c10571056100c10571056100c1057410210521056100b10571056100c10571056100c10571056100b10571056100c10574106105510561056100110571056105610011057410210521056100b10571056100c10571056100b10571056105610011057410210551056100b10571056100b105710561056100110571056105610011057410010521056100c1057410110521056100b10571056100c10571056100c10571056100c10571056100c1057410410551056105610011057410210521056100b10571056100c10571056100c10571056100b10571056105610011057410210521056100b10571056100c10571056100b10571056105610011057410210551056100b10571056100b1057105610561001105710561056100110571056100d105741021055105610561056101b1057051053410210554104105541051055410510580b05410110584102105241011052105610561001105741041055105310561056102a1057410010521056100b10571056100c10571056100c10571056100c10571056100c1057410110521056100b10571056100b10571056100b10571056100c10571056100b105710561056100a10571056104210571056102604404100102c1057410110521056100c10571056105610031057410110521056100b10571056100c10571056100c10571056100c10571056100c1057410210521056100b10571056100b10571056100b10571056100c10571056100b105710561056100410571056102d10574102105241051055105610561001105741071055105310561056102a10574103105241071055105310561056102a10574106105541051055410010521056100c1057410110521056100b10571056100c1057410210521056100b10571056100b10571056100c1057410310521056100b10571056100b10571056100b10571056100c10571056100c1057410810551056105610011057410410551056100b10571056100b10571056100b10571056100b105710561056100110571056105610011057105610561001105710561056100110574104105510531056100b10571056105610011057410010521056100c105741041055410210521056100b10571056100c10571056100c10571056100c10571056100b10571056105610011057410210521056100b10571056100c10571056100c10571056100b10571056105610011057410210521056100b10571056100c10571056100b10571056105610011057410210551056100b10571056100b105710561056100110571056105610011057410010521056100c1057410110521056100b10571056100c10571056100c10571056100c10571056100c1057410410551056105610011057410210521056100b10571056100c10571056100c10571056100b10571056105610011057410210521056100b10571056100c10571056100b10571056105610011057410210551056100b10571056100b1057105610561001105710561056100110571056100d105741021055105610561056101b1057051053410210554104105541051055410510580b0b0b0b0b054105105541061055410310584102105241011052105610561001105741041052105310561056102a1057410210521056100c10571056100c10571056101f0440410d102c105710561028000b4104105241011052105610561001105741061055105310561056102a1057410410551056100b10571056100b105741001052410410521056100b10571056100b10571056100b10571056100c10571056100b105710561056100410571053410210521056100b10571056100b10571056100b10571056100c10571056100b105710561056100310574106105541051055410010521056100c1057410110521056100b10571056100c1057410210521056100b10571056100b10571056100c1057410310521056100b10571056100b10571056100b10571056100c10571056100c1057410710551056102d10571056105610011057410410551056100b10571056100b10571056100b10571056100b105710561056100110571056105610011057105610561001105710561056100110571056100d105741051055105610561056101b105741021055410010521056100c1057410110521056100b10571056100c1057410210521056100b10571056100b10571056100c1057410310521056100b10571056100b10571056100b10571056100c10571056100c1057410610551056105610011057410410551056100b10571056100b10571056100b10571056100b105710561056100110571056105610011057105610561001105710561056100110571056100d105741021055105610561056101b10570b100e1057105610561001105710560b\",\n  \"constants\":\n    [ [ 0, [ \"Int\", \"1\" ] ], [ 1, [ \"Int\", \"115\" ] ],\n      [ 2, [ \"Int\", \"130000\" ] ], [ 3, [ \"Int\", \"100\" ] ],\n      [ 4, [ \"String\", \"DIV by 0\" ] ], [ 5, [ \"Int\", \"15\" ] ],\n      [ 6, [ \"Int\", \"1100\" ] ], [ 7, [ \"Int\", \"12000\" ] ],\n      [ 8, [ \"Int\", \"20000000\" ] ], [ 9, [ \"Int\", \"3\" ] ],\n      [ 10, [ \"Int\", \"8\" ] ], [ 11, [ \"Int\", \"47\" ] ],\n      [ 12, [ \"Int\", \"260\" ] ],\n      [ 13, [ \"String\", \"There is not recipient\" ] ] ], \"entrypoints\": {} }","tickets":[]}|}
  in
  let res =
    Ocaml_wasm_vm.Operation_payload.t_of_yojson @@ Yojson.Safe.from_string str
  in
  res.operation

let new_address () =
  let open Deku_crypto in
  let open Deku_ledger in
  let secret = Ed25519.Secret.generate () in
  let secret = Secret.Ed25519 secret in
  let key = Key.of_secret secret in
  let key_hash = Key_hash.of_key key in
  Address.of_key_hash key_hash

let decookie_test =
  let open Alcotest in
  test_case "Originate/Invoke increment" `Quick (fun () ->
      let open Ocaml_wasm_vm in
      let addr = new_address () in
      let x =
        Env.execute
          ~operation_hash:(Deku_crypto.BLAKE2b.hash "tutturu")
          ~tickets:[]
          Env.
            {
              source = addr;
              sender = addr;
              ledger = Deku_ledger.Ledger.initial;
              state = State.empty;
              ticket_table = Ticket_table.init [];
            }
          ~operation:decookie_originate
      in
      let state = Result.get_ok x in
      let (State_entry.Entry { storage = _; _ }) =
        State.fetch_contract state.state
          Deku_ledger.(
            Contract_address.of_user_operation_hash
              (Deku_crypto.BLAKE2b.hash "tutturu"))
      in
      (check bool) "Originate" true ("assertion about state here" = "");
      let x =
        Env.execute
          ~operation_hash:(Deku_crypto.BLAKE2b.hash "tutturu")
          ~tickets:[]
          Env.
            {
              source = addr;
              sender = addr;
              ledger = Deku_ledger.Ledger.initial;
              state = state.state;
              ticket_table = Ticket_table.init [];
            }
          ~operation:decookie_invoke
      in
      let state = Result.get_ok x in
      let (State_entry.Entry { storage = _; _ }) =
        State.fetch_contract state.state
          Deku_ledger.(
            Contract_address.of_user_operation_hash
              (Deku_crypto.BLAKE2b.hash "tutturu"))
      in
      (check bool) "Invoke" true ("assertion about state here" = "");
      ())