let await = Lwt.return;
let (let.await) = Lwt.bind;

module Hello {
  let call_hello = () =>
    let.await output =
      Lwt_process.pmap((command, [|command, "-c", "$hello"|]), "");
    print_string(output);

}
