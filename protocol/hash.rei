type t('a) =
  pri {
    hash: string,
    data: 'a,
  };

module SHA256: {
  let hash: 'a => t('a);
  let verify: (~hash: string, 'a) => result(t('a), string);
};
