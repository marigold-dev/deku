type tokenId = nat;
type tokenOwner = address;
type tokenAmount = nat;
type transferContents = {
    to_: tokenOwner,
    token_id: tokenId,
    amount: tokenAmount
};
type transfer = {
    from_: tokenOwner,
    txs: list(transferContents)
};
type transferContentsMichelson = michelson_pair_right_comb(transferContents);
type transferAuxiliary = {
    from_: tokenOwner,
    txs: list(transferContentsMichelson)
};
type transferMichelson = michelson_pair_right_comb(transferAuxiliary);
type transferParameter = list(transferMichelson);
type parameter = 
| Transfer(transferParameter)
type storage = big_map(tokenId, tokenOwner);
type entrypointParameter = (parameter, storage);
type entrypointReturn = (list(operation), storage);
let errorTokenUndefined = "TOKEN_UNDEFINED";
let errorNotOwner = "NOT_OWNER";
let errorInsufficientBalance = "INSUFFICIENT_BALANCE";
type transferContentsIteratorAccumulator = (storage, tokenOwner);
let transferContentsIterator = ((accumulator, transferContentsMichelson): (transferContentsIteratorAccumulator, transferContentsMichelson)): transferContentsIteratorAccumulator => {
    let (storage, from_) = accumulator;
    let transferContents: transferContents = Layout.convert_from_right_comb(transferContentsMichelson);
    let tokenOwner: option(tokenOwner) = Map.find_opt(transferContents.token_id, storage);
    let tokenOwner = switch (tokenOwner) {
        | None => (failwith(errorTokenUndefined): tokenOwner)
        | Some(tokenOwner) => if (tokenOwner == from_) {
                tokenOwner
            } else {
                (failwith(errorInsufficientBalance): tokenOwner);
            }
    };
    let storage = Map.update(
        transferContents.token_id,
        Some(transferContents.to_),
        storage
    );
    (storage, from_)
};
let allowOnlyOwnTransfer = (from: tokenOwner): unit => {
    if (from != Tezos.sender) {
        failwith(errorNotOwner)
    } else { (); }
}
let transferIterator = ((storage, transferMichelson): (storage, transferMichelson)): storage => {
    let transferAuxiliary2: transferAuxiliary = Layout.convert_from_right_comb(transferMichelson);
    let from_: tokenOwner = transferAuxiliary2.from_;
    allowOnlyOwnTransfer(from_);
    let (storage, _) = List.fold(
        transferContentsIterator, 
        transferAuxiliary2.txs,
        (storage, from_)
    );
    storage
};
let transfer = ((transferParameter, storage): (transferParameter, storage)): entrypointReturn => {
    let storage = List.fold(transferIterator, transferParameter, storage);
    (([]: list(operation)), storage);
};
let main = ((parameter, storage): entrypointParameter): entrypointReturn => {
    switch (parameter) {
        | Transfer(transferParameter) => transfer((transferParameter, storage))
    }
}
