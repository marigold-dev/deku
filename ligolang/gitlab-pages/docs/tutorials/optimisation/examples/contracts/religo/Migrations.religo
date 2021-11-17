type migrations = {last_completed_migration: int, owner: address };

let main = ((completed_migration, migrations): (int, migrations)) => {
  let st =
    if (Tezos.sender == migrations.owner) {
      {...migrations, last_completed_migration: completed_migration}
    } else {
      migrations
    };
  ([] : list(operation), st)
};
