use crate::{
    arena::{ARENA, TICKETABLE},
    errors::{vm::VmError, VMResult},
    managed::value::Value,
    ticket_table::TicketTable,
};
use slotmap::{DefaultKey, Key};
use std::{cell::RefCell, ptr::NonNull, rc::Rc};
use wasmer::{HostEnvInitError, Instance, WasmerEnv};
use wasmer_middlewares::metering::{get_remaining_points, set_remaining_points, MeteringPoints};

pub struct Context {
    pub inner: Rc<RefCell<Inner>>,
}

pub struct Inner {
    pub instance: Option<NonNull<Instance>>,
    pub pusher: Option<NonNull<wasmer::NativeFunc<i64, ()>>>,
    pub gas_limit: u64,
    pub call_unit: Option<NonNull<wasmer::NativeFunc<(i64, i32), ()>>>,
    pub call: Option<NonNull<wasmer::NativeFunc<(i64, i32), i64>>>,
}

impl Clone for Context {
    fn clone(&self) -> Self {
        Context {
            inner: Rc::clone(&self.inner),
        }
    }
}
impl WasmerEnv for Context {
    fn init_with_instance(&mut self, _instance: &Instance) -> Result<(), HostEnvInitError> {
        Ok(())
    }
}
unsafe impl Send for Context {}

unsafe impl Sync for Context {}

impl Context {
    pub fn with_instance<C, R>(&self, callback: C) -> VMResult<R>
    where
        C: FnOnce(&Instance) -> VMResult<R>,
    {
        match self.inner.as_ref().borrow().instance {
            Some(instance_ptr) => {
                let instance_ref = unsafe { instance_ptr.as_ref() };
                callback(instance_ref)
            }
            None => Err(VmError::InstantiationErr(
                "instance missing, lifecycle error".to_owned(),
            )),
        }
    }
    pub fn set_instance(&self, wasmer_instance: Option<NonNull<Instance>>) {
        self.inner.as_ref().borrow_mut().instance = wasmer_instance;
    }
    pub fn set_pusher(&self, pusher: Option<NonNull<wasmer::NativeFunc<i64, ()>>>) {
        self.inner.as_ref().borrow_mut().pusher = pusher;
    }

    pub fn set_call_unit(&self, f: Option<NonNull<wasmer::NativeFunc<(i64, i32), ()>>>) {
        self.inner.as_ref().borrow_mut().call_unit = f;
    }
    pub fn set_call(&self, f: Option<NonNull<wasmer::NativeFunc<(i64, i32), i64>>>) {
        self.inner.as_ref().borrow_mut().call = f;
    }
    pub fn get_gas_left(&self) -> u64 {
        self.with_instance(|instance| {
            Ok(match get_remaining_points(instance) {
                MeteringPoints::Remaining(count) => count,
                MeteringPoints::Exhausted => 0,
            })
        })
        .expect("impossible")
    }

    pub fn set_gas_left(&self, new_value: u64) {
        self.with_instance(|instance| {
            set_remaining_points(instance, new_value);
            Ok(())
        })
        .expect("impossible")
    }

    #[inline]
    pub fn update_gas(&self, cost: u64) -> VMResult<()> {
        let gas_left = self.get_gas_left();
        if cost > gas_left {
            Err(VmError::OutOfGas)
        } else {
            let new_limit = gas_left.saturating_sub(cost);
            self.set_gas_left(new_limit);
            Ok(())
        }
    }
    pub fn push_value(&self, value: i64) -> VMResult<()> {
        match self.inner.as_ref().borrow().pusher {
            Some(instance_ptr) => {
                let func = unsafe { instance_ptr.as_ref() };
                func.call(value)
                    .map_err(|x| VmError::RuntimeErr(x.to_string()))
            }
            None => Err(VmError::InstantiationErr(
                "pusher missing, lifecycle error".to_owned(),
            )),
        }
    }
    pub fn call(&self, value: i64, idx: i32) -> VMResult<i64> {
        match self.inner.as_ref().borrow().call {
            Some(instance_ptr) => {
                let func = unsafe { instance_ptr.as_ref() };
                func.call(value, idx)
                    .map_err(|x| VmError::RuntimeErr(x.to_string()))
            }
            None => Err(VmError::InstantiationErr(
                "pusher missing, lifecycle error".to_owned(),
            )),
        }
    }
    pub fn call_unit(&self, value: i64, idx: i32) -> VMResult<()> {
        match self.inner.as_ref().borrow().call_unit {
            Some(instance_ptr) => {
                let func = unsafe { instance_ptr.as_ref() };
                func.call(value, idx)
                    .map_err(|x| VmError::RuntimeErr(x.to_string()))
            }
            None => Err(VmError::InstantiationErr(
                "pusher missing, lifecycle error".to_owned(),
            )),
        }
    }
    pub fn bump(&self, value: Value) -> u64 {
        let arena = unsafe { &mut ARENA };

        arena.insert(value).data().as_ffi()
    }
    pub fn bump_raw(&self, value: Value) -> DefaultKey {
        let arena = unsafe { &mut ARENA };

        arena.insert(value)
    }
    pub fn get(&self, value: DefaultKey) -> VMResult<Value> {
        let arena = unsafe { &mut ARENA };

        arena
            .remove(value)
            .map_or_else(|| Err(VmError::RuntimeErr("Value doesnt exist".into())), Ok)
    }
    pub fn get_ref(&self, value: DefaultKey) -> VMResult<&Value> {
        let arena = unsafe { &mut ARENA };

        arena
            .get(value)
            .map_or_else(|| Err(VmError::RuntimeErr("Value doesnt exist".into())), Ok)
    }
    pub fn with_table<A>(&self, f: impl FnOnce(&mut TicketTable) -> VMResult<A>) -> VMResult<A> {
        let t = unsafe { &mut TICKETABLE };
        f(t)
    }
}
