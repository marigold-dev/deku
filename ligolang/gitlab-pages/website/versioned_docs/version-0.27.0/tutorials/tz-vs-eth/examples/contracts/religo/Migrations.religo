type migrations = {
  last_completed_migration: int,
  owner: address
};

let main = ((completed_migration, migrations): (int, migrations)) => {
  let st =
    if (Tezos.sender != migrations.owner) {
      migrations
    } else {
      {...migrations, last_completed_migration: completed_migration }
    };
  ([] : list(operation), st)
};
