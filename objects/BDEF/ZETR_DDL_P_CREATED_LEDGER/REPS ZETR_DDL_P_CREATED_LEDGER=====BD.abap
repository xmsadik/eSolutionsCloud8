projection;
strict ( 2 );

define behavior for zetr_ddl_p_created_ledger alias CreatedLedger
{

  use action create_ledger;

  use association _ledgerParts;
  use association _ledgerTotals;
}

define behavior for zetr_ddl_p_created_ledger_part alias LedgerParts
{

  use association _createdLedger;
}

define behavior for zetr_ddl_p_created_ledger_tot alias LedgerTotals
{

  use association _createdLedger;
}