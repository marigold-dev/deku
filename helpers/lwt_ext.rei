module Let_syntax: {
  let await: 'a => Lwt.t('a);
  let (let.await): (Lwt.t('a), 'a => Lwt.t('b)) => Lwt.t('b);
};
