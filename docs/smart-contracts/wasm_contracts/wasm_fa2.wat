(module
    (import "env" "syscall" (func $syscall (param i64) (result i32)))
    (memory (export "memory") 1)
    (global $last_balance (mut i32) (i32.const 0))
    (global $first_balance (mut i32) (i32.const 0))

    ;; Balance lists are linked lists containing an address, an entry is 16 bytes wide,
    ;; structured like so :
    ;; address (8 byte i64) | balance (4 byte i32) | next_address_offset (4 byte i32)
    ;; If an entry is the last in a list, its next_address_offset should be 0. Any function
    ;; that accesses the next_address_offset in a list entry should understand a value of 0
    ;; to indicate the end of the list. Balance lists support look-up by address, balance
    ;; adjustments, and appending entries to the list. The offset in linear memory of the
    ;; last entry in the balance list is maintained in the global $last_balance and should
    ;; be updated if/when ever an entry is appended to the list. Furthermore, the next_address_offset
    ;; in the last entry should be updated when a new entry is appended.

    ;; Given an $addr representing an account and a $list, essentially
    ;; an offset in linear memory where a balance list begins, this function
    ;; linearly traverses the list looking for an element whose address (first field)
    ;; is equal to $addr. If the element is found, the offset of the element
    ;; in linear memory is returned. Otherwise, -1 is returned. 
    (func $lookup_account (param $addr i64) (param $list i32) (result i32)
      (local $next i32)
      local.get $list ;; push the offset of the first address in the list
      i64.load ;; load the address
      local.get $addr
      i64.eq ;; Check if the address at the beginning of $list is the same as $addr
      (if
        (then
          local.get $list ;; Return offset of located entry in list
          return
        )
        (else
          local.get $list
          i32.load offset=12 ;; Load the offset of the next address
          local.set $next ;; Store the address offset
          local.get $next
          i64.load ;; Load the address itself as i64
          i64.const 0 ;; Check if the next address is 0
          i64.eq
          (if
            (then
              i32.const -1
              return
            )
            (else
              local.get $addr
              local.get $next
              call $lookup_account ;; Recursively call with next address in the list
              return
            )
          )
        )
      )
      i32.const -1
      return
    )

    ;; To check the balance of an address, we specify the $addr and $list and look
    ;; it up using $lookup_account. If the address is found, we simply load the balance
    ;; which is an i32 8 bytes out from the address location. If it's not found, we
    ;; return -1
    (func $get_balance (param $addr i64) (param $list i32) (result i32)
      (local $addr_offset i32)
      local.get $addr
      local.get $list
      call $lookup_account ;; Look up the account to either get its offset in memory or find that it doesn't exist
      local.set $addr_offset
      local.get $addr_offset
      i32.const -1
      i32.eq ;; Check whether the offset we found was actually just -1, indicating the address was not found in the list
      (if (result i32)
        (then
          i32.const -1
        )
        (else
          local.get $addr_offset ;; Push the address offset to the stack 
          i32.load offset=8 ;; Then read the balance 8 bytes out from it
        )
      )
    )

    ;; Given an address whose balance is being adjusted, a signed i32 amount by which to adjust,
    ;; and the location of the first entry in the list, this function looks up the given address
    ;; and, if found successfully, replaces the balance of the given address with the sum of
    ;; the current balance and the $amount. E.g., if an address' balance is 12, and the amount is -3
    ;; then the balance of 12 is replaced with a balance of 9. On success, the new balance is returned.
    ;; On failure, -1 is returned.
    (func $adjust_balance (param $addr i64) (param $amount i32) (param $list i32) (result i32)
      (local $addr_offset i32)
      local.get $addr
      local.get $list
      call $lookup_account ;; Look up the address to find the location of its entry
      local.set $addr_offset
      local.get $addr_offset
      i32.const -1
      i32.eq ;; Check if an entry was found
      (if (result i32)
        (then
          i32.const -1
        )
        (else
          local.get $addr_offset
          local.get $addr_offset
          i32.load offset=8
          local.get $amount
          i32.add ;; Load the balance in this entry and sum it with the specified $amount
          i32.store offset=8 ;; Now that $addr_offset is right under the sum in the stack, we can store the sum as the balance
          local.get $addr_offset
          i32.load offset=8
        )
      )
    )

    ;; Takes two addresses, one to transfer from and one to transfer to, an amount to transfer, and the offset
    ;; of the first address in a balance list. First determines if $from's balance is high enough to transfer
    ;; $amount to $to. If it is, then $amount is subtracted from $from's balance and is added to $to's
    ;; balance. If the balance of $from is insufficient for the transfer of $amount, -1 is returned. If $from
    ;; isn't found in the list, -2 is returned. If $to isn't found in the list, -3 is returned. If the transfer
    ;; is successful, 0 is returned.
    (func $transfer (param $from i64) (param $to i64) (param $amount i32) (param $list i32) (result i32)
      (local $from_balance i32)
      (local $to_balance i32)
      local.get $from
      local.get $list
      call $get_balance ;; Push $from's balance
      local.set $from_balance
      local.get $from_balance
      i32.const -1
      i32.eq
      (if (result i32)
        (then
          i32.const -2 ;; Return -2 indicating address not found in list
        )
        (else
          local.get $from_balance
          local.get $amount
          i32.ge_s ;; Check that $from's balance >= $amount
          (if (result i32)
            (then
              local.get $from
              local.get $amount
              i32.const -1
              i32.mul ;; Multiply $amount by -1 so that $adjust_balance subtracts the amount rather than adding it
              local.get $list
              call $adjust_balance ;; Reduce from's balance by amount
              local.get $to
              local.get $amount
              local.get $list
              call $adjust_balance ;; Increase to's balance by amount
              local.set $to_balance
              local.get $to_balance
              i32.const -1
              i32.eq
              (if (result i32)
                (then
                  i32.const -3 ;; Return -3 indicating $to not found in list
                )
                (else
                  i32.const 0 ;; Return 0 indicating a successful transfer
                )
              )
              return
            )
            (else
              i32.const -1 ;; Return -1 indicating insufficient balance for transfer
            )
          )
        )
      )
    )

    ;; Given an $addr, create a new list entry 16 bytes away from $last_balance
    ;; with a balance of 0. 
    (func $add_account (param $addr i64) (result i32)
      global.get $last_balance
      local.get $addr
      i64.store offset=16 ;; Store the given address in the next available space, 16 bytes from the last entry
      global.get $last_balance
      i32.const 0
      i32.store offset=24 ;; 8 bytes from the offset where we stored $addr, store an empty balance
      global.get $last_balance
      i32.const 0
      i32.store offset=28 ;; 4 bytes from the balance, store an offset of 0 indicating that there are no more entries
      global.get $last_balance
      global.get $last_balance
      i32.const 16
      i32.add
      i32.store offset=12 ;; 12 bytes from the previous last element, store the offset of the new last element, 16 bytes out
      global.get $last_balance
      i32.const 16
      i32.add
      global.set $last_balance ;; Set the offset of the last entry to the beginning of the new entry
      global.get $last_balance
      return
    )

    (func $handle_transfer_op (param $from i64) (param $to i64) (param $amount i32) (result i32)
      local.get $from
      call $add_account
      local.get $to
      call $add_account
      local.get $from
      local.get $amount
      global.get $first_balance
      call $adjust_balance ;; Give $from balance of $amount so that the transfer will succeed
      local.get $from
      local.get $to
      local.get $amount
      global.get $first_balance
      call $transfer
      return
    )

    (func (export "main")  (param $storage_offset i32) (result i64 i64 i64)
      local.get $storage_offset
      i32.const -16
      i32.add
      global.set $last_balance ;; This way the first entry to the balance list will be created at the start of storage

      local.get $storage_offset
      global.set $first_balance

      ;; Begin handling operation
      i32.const 0
      i32.load ;; Load operation type
      i32.const 0
      i32.eq
      (if (result i32);; If operation_type is 0, then we're doing a transfer
        (then
          i32.const 4
          i64.load ;; Load $from address
          i32.const 12
          i64.load ;; Load $to address
          i32.const 20
          i32.load ;; Load amount
          call $handle_transfer_op
        )
        (else ;; Other operations aren't supported yet
          i32.const 0
        )
      )

      global.get $first_balance
      i64.extend_i32_s
      global.get $last_balance
      i32.const 16
      i32.add ;; Compute size of balance list for return
      local.get $storage_offset
      i32.const -1
      i32.mul
      i32.add ;; Subtract the storage offset from the storage size, giving us the true storage size
      i64.extend_i32_s
      i64.const 300 ;; Arbitrarily high value for operation list
      return
    )
)