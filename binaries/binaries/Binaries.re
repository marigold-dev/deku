open Lwt.Syntax;

module Hello = {
  let call = () => {
    let command = "sh";
    let* output =
      Lwt_process.pmap((command, [|command, "-c", "$Hello"|]), "");
    Lwt.return(Ok(output));
  };
};
