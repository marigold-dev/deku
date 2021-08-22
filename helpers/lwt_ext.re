module Let_syntax = {
  let await = Lwt.return;
  let (let.await) = Lwt.bind;
};
