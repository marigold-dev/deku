open Lwt.Syntax;

let invoke = (env_command_name, arguments) => {
  let sh = "sh";
  let* command_path = {
    let* command_path =
      Lwt_process.pmap(
        (sh, [|sh, "-c", Printf.sprintf("echo $%s", env_command_name)|]),
        "",
      );
    let command_path = String.trim(command_path);
    if (command_path != "") {
      Lwt.return(Ok(command_path));
    } else {
      Lwt.return(
        Error(
          Printf.sprintf(
            "couldn't find $%s in environment",
            env_command_name,
          ),
        ),
      );
    };
  };
  switch (command_path) {
  | Ok(command_path) =>
    let invokation =
      String.concat(" ", [command_path, ...Array.to_list(arguments)]);
    Printf.printf("invoking %s\n", invokation);
    let* output =
      Lwt_process.pmap(
        (
          command_path,
          Array.concat([[|command_path|], arguments]) // TODO: why does this have to go through sh?
        ),
        "",
      );
    switch (
      Stringext.cut(output, ~on="=============== output ==============="),
      Stringext.cut(output, ~on="=============== error ==============="),
    ) {
    | (Some((_, output)), _) => Lwt.return(Ok(String.trim(output)))
    | (_, Some((_, msg))) => Lwt.return(Error(msg))
    | _ =>
      Lwt.return(
        Error(Printf.sprintf("invalid output from command! \n\n%s", output)),
      )
    };
  | x => Lwt.return(x)
  };
};

module Hello = {
  let call = () => {
    invoke("Hello", [||]);
  };
};

module Ligo_wrapper = {
  let call = ligo => {
    let* zinc_str = invoke("Ligo_wrapper", [|"ligo-to-zinc", ligo|]);
    switch (zinc_str) {
    | Ok(zinc_str) =>
      Lwt.return(
        zinc_str
        |> Yojson.Safe.from_string
        |> Zinc.Types.program_of_yojson
        |> Result.map_error("error parsing zinc program"),
      )

    | err => err
    };
  };
};
