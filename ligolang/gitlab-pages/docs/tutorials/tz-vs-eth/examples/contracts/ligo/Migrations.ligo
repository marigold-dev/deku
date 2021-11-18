type migrations is record
  owner : address;
  last_completed_migration : int;
end

function main (const completed_migration: int ; var migrations : migrations) : (list(operation) * migrations) is
  block {
    if sender =/= migrations.owner
    then
      skip
    else
      migrations.last_completed_migration := completed_migration;
  } with ((nil : list(operation)), migrations);
