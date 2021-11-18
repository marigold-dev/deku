type migrations = {
  owner : address;
  last_completed_migration : int;
}

let main (completed_migration, migrations : int * migrations) =
  let st =
    if sender <> migrations.owner
    then migrations
    else { migrations with last_completed_migration = completed_migration }
  in ([] : operation list), st

