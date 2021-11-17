type migrations is record [owner : address; last_completed_migration : int]

function main (const completed_migration : int; var migrations : migrations)
  : (list (operation) * migrations) is
block {
  if sender = migrations.owner
  then migrations.last_completed_migration := completed_migration
  else skip
} with ((nil : list (operation)), migrations)
