module Hello = {
  let call_hello = () => {
    let command = "sh";
    Lwt.bind(
      Lwt_process.pmap((command, [|command, "-c", "$hello"|]), ""),
      output => {
        print_string(output);
        Lwt.return(Ok());
      },
    );
  };
};
