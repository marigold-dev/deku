(module
  (table $stack 4000 externref)
  (global $sp (mut i32) (i32.const 4000)) ;; stack pointer
  (table $shadow_stack 1000 externref)
  (global $sh_sp (mut i32) (i32.const 1000)) ;;shadow_stack stack pointer
    (func $dip (param $n i32) (result)
        (local $stop i32) 
        (local $sp' i32)
        (local $sh_sp' i32)
        (local.set $stop (i32.const 0))
        (local.set $sp'  (global.get $sp))
        (local.tee $sh_sp' (i32.sub (global.get $sh_sp) (local.get $n)))
        global.set $sh_sp
        (loop $l
          (i32.add (local.get $sh_sp') (local.get $stop))
          (table.get $stack (i32.add (local.get $sp') (local.get $stop)))
          (table.set $shadow_stack)
          (local.tee $stop (i32.add (local.get $stop) (i32.const 1)))
          (local.get $n)
          i32.ne
          br_if $l
        )
    	(global.set $sp 
           (i32.add 
             (local.get $sp') (local.get $n)
            )
         )
  )
    (func $undip (param $n i32) (result)
        (local $stop i32)
        (local $sp' i32)
        (local $sh_sp' i32)
        (local.tee $sp'  (i32.sub (global.get $sp) (local.get $n)))
        global.set $sp
        (local.set $sh_sp' (global.get $sh_sp))
        (local.set $stop (i32.const 0))
         (loop $l
          (i32.add (local.get $sp') (local.get $stop))
          (table.get $shadow_stack (i32.add (local.get $sh_sp') (local.get $stop)))
          (table.set $stack)
          (local.tee $stop (i32.add (local.get $stop) (i32.const 1)))
          (local.get $n)
          i32.ne
          br_if $l
        )
    	(global.set $sh_sp (i32.add (local.get $sh_sp') (local.get $n)))
  )

  (func $dup (param $n i32) (result)
        (table.get $stack (i32.add (global.get $sp) (local.get $n)))
    	(call $push)
  )


    (func $dug (param $n i32) (result)
         (local $idx i32)
         (local $loop_idx i32)
         (local $sp' i32)
         (local $top externref)
		 (local.set $sp' (i32.add (global.get $sp) (local.get $n)))
      	 (local.tee $idx (global.get $sp))
      	 (local.tee $loop_idx)
         table.get $stack
         local.set $top
        (loop $loop
          (local.get $idx)
          (i32.add (local.get $loop_idx) (i32.const 1))
          local.tee $loop_idx
          table.get $stack
          table.set $stack
		  (local.set $idx (i32.add (local.get $idx) (i32.const 1)))
          (local.get $idx)
          (local.get $sp')
          i32.lt_u
          br_if $loop
        )
        (table.set $stack (local.get $sp') (local.get $top))
   )
    (func $dig (param $n i32) (result)
        (local $idx i32)
        (local $loop_idx i32)
      	(local $sp' i32)
        (local $digged externref)
      	(local.set $sp' (global.get $sp))
        (local.tee $idx (i32.add (local.get $sp') (local.get $n)))
      	(local.tee $loop_idx)
        table.get $stack
        local.set $digged
      (loop $loop
        (local.get $idx)
        (i32.sub (local.get $loop_idx) (i32.const 1))
        local.tee $loop_idx 
        table.get $stack
        table.set $stack
		(local.set $idx (i32.sub (local.get $idx) (i32.const 1)))
        (local.get $sp')
        (local.get $loop_idx)
        i32.lt_u
        br_if $loop
       )
       (table.set $stack (global.get $sp) (local.get $digged))
  )
  (func $pop (result externref)   
       (local $spp i32)
       (local.tee $spp (global.get $sp))
       table.get $stack
       (global.set $sp (i32.add (local.get $spp) (i32.const 1)))  ;;set stackptr
  )
    (func $push (param $value externref) (result) 
       (local $spp i32)
       (local.tee $spp (i32.sub (global.get $sp) (i32.const 1)) )
       (table.set $stack (local.get $value))
       (global.set $sp (local.get $spp) )  ;;set stackptr
  )  
  (func $drop (param $n i32) (result)   
       (global.set $sp (i32.add (global.get $sp) (local.get $n)))  ;;set stackptr
  )
  (type $add_t (func (param externref) (result externref  )))
  (func $main (type $add_t) (param $value externref) (result   externref )
     local.get $value
     call $push
     call $pop
     )
  (export "main" (func $add_one_f))
  (export "push" (func $push))
  (export "pop" (func $push))
)
