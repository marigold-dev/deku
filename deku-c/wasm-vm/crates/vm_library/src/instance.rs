use std::{cell::RefCell, ptr::NonNull, rc::Rc};

use slotmap::{DefaultKey, Key, KeyData};
use wasmer::Instance;

use crate::{
    arena::{populate_predef, push_constants, ARENA},
    env::{Context, Inner},
    errors::{vm::VmError, VMResult},
    execution_result::ExecutionResult,
    incoming::InvokeManaged,
    managed::{
        imports,
        value::{Union, Value},
    },
    path::Path,
};

pub fn invoke_managed(t: InvokeManaged) -> VMResult<ExecutionResult> {
    let arena = unsafe { &mut ARENA };
    let module = t.mod_;
    let env = Context {
        table: t.table,
        inner: Rc::new(RefCell::new(Inner {
            instance: None,
            pusher: None,
            gas_limit: 10000,
            call_unit: None,
            call: None,
        })),
    };
    populate_predef(t.sender, t.self_addr, t.source);
    push_constants(t.constants);
    let store = module.store();

    let instance = Box::from(
        Instance::new(module, &imports::make_imports(&env, store))
            .map_err(|err| VmError::RuntimeErr(format!("Failed to create instance {}", err)))?,
    );

    {
        let new = NonNull::from(instance.as_ref());
        let pusher = Box::from(
            instance
                .exports
                .get_native_function::<i64, ()>("push")
                .map_err(|_| VmError::RuntimeErr("Miscompiled contract".to_owned()))?,
        );
        let call_unit = Box::from(
            instance
                .exports
                .get_native_function::<(i64, i32), ()>("call_callback_unit")
                .map_err(|_| VmError::RuntimeErr("Miscompiled contract".to_owned()))?,
        );
        let call = Box::from(
            instance
                .exports
                .get_native_function::<(i64, i32), i64>("call_callback")
                .map_err(|_| VmError::RuntimeErr("Miscompiled contract".to_owned()))?,
        );

        env.set_instance(Some(new));
        env.set_pusher(Some(NonNull::from(pusher.as_ref())));
        env.set_call_unit(Some(NonNull::from(call_unit.as_ref())));

        env.set_call(Some(NonNull::from(call.as_ref())));

        env.set_gas_left(t.gas_limit as u64);
    }
    let fst = match t.entrypoint_path {
        Some(path) => {
            let res = path
                .iter()
                .rev()
                .fold(Box::from(t.arg), |acc, path| match path {
                    Path::Left => {
                        let l = Value::Union(Union::Left(acc));
                        Box::from(l)
                    }
                    Path::Right => {
                        let l = Value::Union(Union::Right(acc));
                        Box::from(l)
                    }
                });
            res
        }
        None => Box::from(t.arg),
    };
    let snd = t.initial_storage;
    let arg = Value::Pair { fst, snd };
    let arg = arena.insert(arg).data().as_ffi();

    let caller = instance
        .exports
        .get_native_function::<i64, i64>("main")
        .map_err(|_| VmError::RuntimeErr("Miscompiled contract".to_owned()))?;

    let result: VMResult<i64> = caller.call(arg as i64).map_err(Into::into);
    let result = result?;
    let key = DefaultKey::from(KeyData::from_ffi(result as u64));
    let value = arena.remove(key);

    value.map_or_else(
        || {
            Err(VmError::RuntimeErr(
                "Runtime Error, result not available".to_owned(),
            ))
        },
        |ok| match ok {
            Value::Pair { fst, snd } => {
                let value = snd;
                let ops = fst;
                Ok(ExecutionResult {
                    new_storage: value,
                    ops,
                    remaining_gas: env.get_gas_left(),
                })
            }
            _ => Err(VmError::RuntimeErr(
                "Type mismatch in final result, result not available".to_owned(),
            )),
        },
    )
}
