include Uri;

let to_yojson = t => `String(to_string(t));
// TODO: exception here
let of_yojson = json => json |> [%of_yojson: string] |> Result.map(of_string);
